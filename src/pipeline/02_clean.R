# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# 02_clean.R - DATA QUALITY FILTERING PIPELINE
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Apply systematic quality control filters to remove invalid responses
#          from processed survey data. Ensures only high-quality responses
#          proceed to final analysis.
#
# INPUTS:  Processed .sav files from DIR_SPLIT (output of 01_split.R)
#          - ~40+ country/language-specific datasets
#          - Each contains raw survey responses with minimal preprocessing
#
# OUTPUTS: Cleaned .sav files written to DIR_CLEAN
#          - [dataset]_clean.sav
#          - clean_summary_<timestamp>.xlsx - Audit trail showing how many responses removed at each step
#
# CLEANING STEPS (7 filters applied, in this order):
#   1. Missing Response: Remove rows with missing core scale data (FTOS, LPS)
#   2. Constant Answers: Remove rows where participant gave same answer to all items in a scale
#   3. Attention Checks: Remove participants who failed embedded attention-check items
#   4. Short Duration: Remove responses submitted too quickly (<10 min; CN_277273 + US_868141 only)
#   5. Zigzag Patterns: Remove alternating response patterns (1-7-1-7-1-7...)
#   6. Atypical Response Patterns: Remove statistical outliers, combining Mahalanobis distance
#      and Guttman errors per scale (thresholds differ for CN/US vs IT/BRPT/SI datasets)
#   7. US External Check: Remove US participants not found in an external inclusion dataset
#      (matched by composite ID + age)
#
# DATASET-SPECIFIC LOGIC:
#   - Different filters applied to different datasets based on content
#   - First-stage vs second-stage surveys have different requirements
#   - Country-specific scales (e.g., IPIP for Brazil/Portugal, GRIT for US)
#
# AUDIT TRAIL: Creates detailed Excel summary tracking:
#   - Initial N for each dataset
#   - Rows removed at each cleaning step
#   - Final N after all filters
#   - Percentage retained
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Preparation ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Load required libraries
library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(tools)
library(writexl)
library(here)

# Load configuration and helper functions
source(here::here("config", "paths.R"))
source(here::here("src", "utils", "cleaning_functions.R"))
source(here::here("src", "utils", "validation.R"))
source(here::here("config", "scales.R"))  # depends on mk_step() from cleaning_functions.R

# Load external US inclusion list for extra check
external_us_file <- file.path(DIR_EXTERNAL, "DataSet US - Extra Check.sav")
external_us_df <- read_sav(external_us_file)

# DATASET GROUPINGS: Define which datasets get which filters
# These groupings are loaded from config/paths.R
# They allow applying different filters to different subsets of data
brpt <- DATASETS$brpt                    # Brazil & Portugal datasets
cn <- DATASETS$cn                          # China datasets
us <- DATASETS$us                          # USA datasets                 
cn_us_10_min <- DATASETS$cn_us_10_min      # Datasets to filter for <10 min duration
first_stage <- DATASETS$first_stage        # All first-stage surveys
first_stage_brpt <- DATASETS$first_stage_brpt  # Brazil/Portugal first-stage
datasets_to_remove <- DATASETS$datasets_to_remove            # Datasets to exclude from cleaning (e.g., removed datasets)

# Get list of all split SPSS files to clean including files in subfolders
file_list <- list.files(
  path = DIR_SPLIT,              # Directory containing processed files
  pattern = "\\.sav$",               # Only .sav files (SPSS format)
  full.names = TRUE,                  # Return full paths (not just filenames)
  recursive = TRUE                   # Include subdirectories
)

# Update list of datasets to clean, excluding any in the "datasets_to_remove" list
updated_file_list <- file_list %>%
  set_names(basename(.) %>% file_path_sans_ext()) %>%  # Name list by dataset name (without .sav)
  keep(~ !any(str_detect(., datasets_to_remove)))               # Exclude datasets in "datasets_to_remove" list

# VALIDATE: every entry in datasets_to_remove should have matched at least one
# file in DIR_SPLIT. A token that matches nothing means that dataset is
# silently NOT being excluded (this exact failure mode let CH_999625/MS_999625
# leftovers slip through cleaning before the CN/MY renames caught up everywhere).
assert_datasets_exist(
  datasets_to_remove,
  basename(file_list),
  context = "02_clean.R datasets_to_remove",
  fixed = TRUE
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MISSING RESPONSE FILTER ----
# PURPOSE: Remove participants who didn't answer core scales
# RATIONALE: Can't calculate scale scores without complete data
#
# FILTERS:
#   - FTOS_v1 missing: Remove if missing first-stage FTOS (except BRPT)
#   - FTOS_v2 or LPS missing: Remove if missing second-stage scales
#   - FTOS_pilot missing: Remove if missing pilot FTOS
#   - FTOS or Psy_LOT missing: Remove if missing FTOS or LOT (BRPT only)
#
# NOTE: Different datasets use different scale versions (v1, v2, pilot)
#       so need separate checks for each
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
filter_na <- mk_group("Missing response",
  steps = list(
    # Check 1: First-stage FTOS (FTOS_v1)
    # Skip Brazil/Portugal (they use different scale combination)
    mk_step(
      "FTOS_v1 missing",
      step_drop_na_block(),               # Default pattern checks FTOS_v1
      exclude = brpt                     # Don't apply to BRPT datasets
    ),
    
    # Check 2: Second-stage scales (FTOS_v2 or LPS_v2)
    # Remove if missing either scale (both needed for second-stage)
    mk_step(
      "FTOS_v2 or LPS missing",
      step_drop_na_block("^(FTOS_v2_\\d+|LPS_v2_\\d+)$"),  # Regex: FTOS_v2_1, LPS_v2_1, etc.
      exclude = c(brpt, "IT_AUTO")      # Skip BRPT and Italian auto
    ),
    
    # Check 3: Pilot FTOS
    # No exclusions - applies to all datasets with pilot data
    mk_step(
      "FTOS_pilot missing",
      step_drop_na_block("^FTOS_pilot_\\d+$")  # Regex: FTOS_pilot_1, FTOS_pilot_2, etc.
    ),
    
    # Check 4: FTOS or Psy_LOT (Brazil/Portugal specific)
    # BRPT use different scale structure: FTOS (any version) + LOT scale
    mk_step(
      "FTOS or Psy_LOT missing",
      step_drop_na_block("^(Psy_LOT\\d+|FTOS_(?:pilot|v1|v2)_\\d+)$"),  # LOT or any FTOS version
      datasets = brpt                    # ONLY apply to BRPT datasets
    )
  )
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# CONSTANT ANSWER FILTER ----
# PURPOSE: Remove participants who gave the same answer to all items in a scale
# RATIONALE: Indicates participant wasn't reading questions (satisficing behavior)
#
# EXAMPLE: Responding "4" to all 15 FTOS items (straight-lining)
#
# SCALES CHECKED:
#   - Core scales: FTOS_v1, FTOS_v2, FTOS_pilot, DGI, LOT
#   - Country-specific: IPIP (BRPT), LS (China), MLQ (BRPT/CN/US), AS (BRPT/CN/US), GRIT (US)
#
# NOTE: Different datasets have different scales, so filters are targeted
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Steps are generated from CONSTANT_ANSWER_SCALES in config/scales.R (single
# source of truth for scale name / column pattern / applicable datasets).
constant_and_binary <- mk_group(
  "Drop rows with constant responses",
  steps = build_scale_steps(CONSTANT_ANSWER_SCALES, step_constant_answers)
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ATTENTION CHECK FILTER ----
# PURPOSE: Remove participants who failed attention control items
# RATIONALE: Attention checks verify participants are reading questions carefully
#
# EXAMPLE: "Please select 'Strongly Agree' for this item"
#          Failing indicates inattentive responding
#
# SCALES WITH ATTENTION CHECKS:
#   FTOS, LPS, CFC, CAAS, SWLS, FTPQ, DMF, FTPtr, Jung, MH, IPS
#
# MECHANISM: Each scale has a column like "FTOS_x" marking failed attention check
#            step_check_control() removes rows where this column indicates failure
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
check_attention <- mk_group(
  "Check attention control items",
  steps = list(
    mk_step("FTOS", step_check_control()),                    # FTOS_x column
    mk_step("LPS", step_check_control(col = "LPS_x")),        # LPS_x column
    mk_step("CFC", step_check_control(col = "CFC_x")),        # Consideration of Future Consequences
    mk_step("CAAS", step_check_control(col = "CAAS_x")),      # Career Adapt-Abilities Scale
    mk_step("SWLS", step_check_control(col = "SWLS_x")),      # Satisfaction With Life Scale
    mk_step("FTPQ", step_check_control(col = "FTPQ_x")),      # Future Time Perspective Questionnaire
    mk_step("DMF", step_check_control(col = "DMF_x")),        # Decision Making Fluency
    mk_step("FTPtr", step_check_control(col = "FTPtr_x")),    # Future Time Perspective (training)
    mk_step("Jung", step_check_control(col = "Jung_x")),      # Jung Typology
    mk_step("MH", step_check_control(col = "MH_x")),          # Mental Health
    mk_step("IPS", step_check_control(col = "IPS_x"))         # Interpersonal Support
  )
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ZIGZAG PATTERN FILTER ----
# PURPOSE: Remove participants with alternating response patterns
# RATIONALE: Pattern like 1-7-1-7-1-7 or 2-6-2-6 indicates random/careless responding
#
# EXAMPLE: FTOS items answered: [1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1]
#          This is statistically unlikely and suggests inattention
#
# DETECTION: step_detect_zigzag() identifies systematic alternation in responses
#
# SCALES CHECKED: Applied to all major scales, tailored by dataset content
#   - Core: FTOS (v1, v2), LPS (v1, v2)
#   - BRPT: MLQ, AS, IPIP, HS
#   - China: MLQ, AS, CAAS, ESS, ESW
#   - US: MLQ, AS, GRIT
#   - Slovenia: DASS
#   - Italy: IT, DMF
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Steps are generated from ZIGZAG_SCALES in config/scales.R (single source
# of truth for scale name / column pattern / applicable datasets).
remove_zigzag <- mk_group(
  "Remove zigzag answers",
  steps = build_scale_steps(ZIGZAG_SCALES, step_detect_zigzag)
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SCALE PATTERNS: Which scales the atypical-pattern (Mahalanobis + Guttman)
# filter checks per dataset group. Patterns are looked up from SCALE_PATTERNS
# in config/scales.R (the single source of truth) rather than hand-copied
# here, so a pattern fix there automatically applies to this filter too.
scale_patterns_cn_us <- list(
  FTOS = SCALE_PATTERNS[["FTOS_v1"]],  # First-stage FTOS items
  LPS  = SCALE_PATTERNS[["LPS_v1"]],   # First-stage LPS items
  CAAS = SCALE_PATTERNS[["CAAS"]],     # Career Adapt-Abilities Scale
  DGI  = SCALE_PATTERNS[["DGI"]],      # DGI scale (with/without Psy_ prefix)
  MLQ  = SCALE_PATTERNS[["MLQ"]],      # Meaning in Life Questionnaire
  AS   = SCALE_PATTERNS[["AS"]],       # Authenticity Scale
  BRS  = SCALE_PATTERNS[["BRS"]],      # Life Satisfaction - Brief Resilience Scale (CN only)
  ESW  = SCALE_PATTERNS[["ESW"]],      # Existential Scale - Work (CN only)
  ESS  = SCALE_PATTERNS[["ESS"]],      # Existential Scale (CN only)
  FS   = SCALE_PATTERNS[["FS"]],       # Flourishing Scale (CN only)
  GRIT = SCALE_PATTERNS[["GRIT"]]      # Grit Scale (US only)
)

scale_patterns_it_brpt_si <- list(
  FTOS_v1 = SCALE_PATTERNS[["FTOS_v1"]],  # First-stage FTOS (IT_277273, BRPT_277273, SI_277273)
  FTOS_v2 = SCALE_PATTERNS[["FTOS_v2"]],  # Second-stage FTOS (IT_AUTO)
  LPS_v1  = SCALE_PATTERNS[["LPS_v1"]],   # First-stage LPS (IT_277273, BRPT_277273, SI_277273)
  LPS_v2  = SCALE_PATTERNS[["LPS_v2"]],   # Second-stage LPS (IT_AUTO)
  MLQ     = SCALE_PATTERNS[["MLQ"]],      # Meaning in Life Questionnaire (BRPT_277273, IT_AUTO)
  AS      = SCALE_PATTERNS[["AS"]],       # Authenticity Scale (BRPT_277273, IT_AUTO)
  IPIP    = SCALE_PATTERNS[["IPIP"]],     # Big Five personality (BRPT_277273 only)
  HS      = SCALE_PATTERNS[["HS"]],       # HS scale (BRPT_277273 only)
  DASS    = SCALE_PATTERNS[["DASS"]],     # Depression Anxiety Stress Scales (SI_277273 only)
  IT      = SCALE_PATTERNS[["IT"]],       # Italian Time Perspective (IT_277273, IT_AUTO)
  DMF     = SCALE_PATTERNS[["DMF"]]       # Decision Making Fluency (IT_277273, IT_AUTO)
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ATYPICAL RESPONSE PATTERN FILTER ----
# PURPOSE: Remove participants with atypical response patterns per scale
#
# CN/US STRATEGY:
#   - Mahalanobis distance per scale (p < .001)
#   - Guttman errors per scale (|z| > 2)
#   - Remove participants flagged in 2+ scales (Mahalanobis and/or Guttman)
#
# IT/BRPT/SI STRATEGY:
#   - Mahalanobis distance per scale (p < .001 AND M/df > 4.0); scales with
#     ≤ 50% missing items are included using available items only
#   - Guttman errors per scale (|z| > 2); complete responses only
#   - Remove participants flagged in > 50% of filled-in scales AND >= 2 scales
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
atypical_patterns <- mk_group(
  "Atypical response patterns",
  steps = list(
    mk_step(
      "Mahalanobis + Guttman (>=2 scales)",
      step_atypical_patterns(
        scale_patterns_cn_us,
        md_p = 0.001,
        g_z_thresh = 2,
        min_scales = 2,
        scale_flag_logic = "OR"
      ),
      datasets = c(us, cn)
    ),
    mk_step(
      "Mahalanobis (p<.001 AND M/df>4.0) + Guttman (>50% AND >=2 scales)",
      step_atypical_patterns(
        scale_patterns_it_brpt_si,
        md_p = 0.001,
        md_ratio_thresh = 4.0,
        g_z_thresh = 2,
        min_scales = 0.5,
        min_flags = 2,
        use_partial = FALSE,
        scale_flag_logic = "OR"
      ),
      datasets = c("IT_277273", "IT_AUTO", "BRPT_277273", "SI_277273")
    )
  )
)

# US EXTERNAL CHECK FILTER ----
# PURPOSE: Remove US participants not included in the external inclusion dataset
#
# STRATEGY: Match participants using a composite key from personal identifiers
#   (IdCode_1, IdCode_2, IdCode_3) and age. These answers are highly personal
#   (letters of names, mother's initial, birth month) so collisions are
#   extremely unlikely. Duplicate keys trigger a warning for manual review.
us_external_filter <- list(
  name = "Drop US participants not in external dataset",
  fn = step_keep_by_composite_id(
    external_us_df,
    id_cols = c("IdCode_1", "IdCode_2", "IdCode_3", "Age")
  ),
  datasets = us
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ASSEMBLE COMPLETE CLEANING PIPELINE ----
# Combines all cleaning steps in execution order
# Some steps are dataset-specific, handled by the pipeline framework
base_steps <- list(
  # Missing response filter
  filter_na,

  # Constant and binary pattern filter
  constant_and_binary,

  # Attention check filter
  # and short duration filter (<10 minutes for China/US datasets)
  check_attention,
  list(
    name = "Drop short submitted responses (<10 min)",
    fn = step_filter_min_duration(),
    datasets = cn_us_10_min                      # Only CN/US datasets with time concern
  ),
  
  # Zigzag pattern filter
  remove_zigzag,
  
  # Atypical response pattern detection (CN/US, and IT/BRPT/SI with different thresholds)
  atypical_patterns
)

steps <- c(
  base_steps,
  list(us_external_filter)
)

# VALIDATE: every dataset token referenced anywhere in `steps` (via DATASETS
# groupings or hardcoded literals like "IT_AUTO") should match at least one
# dataset actually present in this run. A token matching nothing means a
# filter is silently never applied to anyone - the same failure mode as the
# DATASETS$ch/DATASETS$cn key mismatch that used to make the China-specific
# QC filters a silent no-op.
assert_datasets_exist(
  collect_dataset_tokens(steps),
  names(updated_file_list),
  context = "02_clean.R steps"
)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MAIN PROCESSING LOOP ----
# Apply cleaning pipeline to all datasets
# For each processed file:
#   1. Load data
#   2. Run cleaning pipeline (applies applicable filters)
#   3. Build audit trail summary
#   4. Store summary for final report
#   5. Write cleaned file to DIR_CLEAN
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Initialize storage for all summaries
all_summaries <- list()

# Process each file in the processed directory
for (f in updated_file_list) {
  # Wrap in tryCatch to continue even if one file fails
  tryCatch(
    {
      # Load SPSS file
      df <- read_sav(f)

      # Extract filename without extension (e.g., "CN_277273")
      name <- file_path_sans_ext(basename(f))

      # Optional diagnostic checks (commented out)
      # Can be enabled to inspect specific columns before cleaning
      # if ("FTOS_x" %in% names(df)) {
      #  print_column_info(df, "FTOS_x")
      # }

      # Run the complete cleaning pipeline
      # Returns: cleaned dataframe + audit trail
      res <- run_cleaning_pipeline(df, name, steps)
      df_clean <- res$df_clean          # Cleaned data
      audit <- res$audit                # Audit trail (rows removed per step)

      # Build wide-format summary showing before/after/removed counts
      summary_wide <- build_wide_summary(
        name = name,                    # Dataset name
        df_initial = df,                # Original data (for initial N)
        df_final = df_clean,            # Cleaned data (for final N)
        audit = audit,                  # Step-by-step removals
      )

      # Store summary for this dataset
      all_summaries[[name]] <- summary_wide

      # Write cleaned data to clean directory
      # Filename: [dataset]_clean.sav
      write_sav(df_clean, file.path(DIR_CLEAN, paste0(name, "_clean.sav")))
      
      # Optional: Write individual summary CSV (commented out)
      # write_csv(summary_wide, paste0(name, "_summary.csv"))
      
      # Store original data in global environment (for manual inspection if needed)
      assign(paste0("df_", name), df, envir = .GlobalEnv)
    },
    error = function(e) {
      # Print error message but continue with next file
      message("Error while processing ", f, ": ", conditionMessage(e))
    }
  )
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# CREATE SUMMARY REPORT ----
# Combine all individual dataset summaries into one Excel file
# Shows initial N, final N, and removals at each step for every dataset
summary_all <- bind_rows(all_summaries)
summary_filename <- sprintf("clean_summary_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S"))
write_xlsx(summary_all, file.path(DIR_CLEAN, summary_filename))
message("Cleaning summary saved to: ", summary_filename)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# END OF 02_clean.R
# SUMMARY: This script has applied 7 filters (many with dataset-specific sub-steps)
#          to ~40 datasets, removing invalid responses while preserving audit trail.
#
# OUTPUTS CREATED:
#   - ~40 [dataset]_clean.sav files
#   - clean_summary_<timestamp>.xlsx (comprehensive audit trail)
#
# NEXT STEP: Run 03_merge.R to combine all cleaned datasets
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░