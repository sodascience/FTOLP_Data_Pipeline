# ============================================================================
# 02_clean.R - DATA QUALITY FILTERING PIPELINE
# ============================================================================
# PURPOSE: Apply systematic quality control filters to remove invalid responses
#          from processed survey data. Ensures only high-quality responses
#          proceed to final analysis.
#
# INPUTS:  Processed .sav files from DIR_PROCESSED (output of 01_split_raw.R)
#          - ~40+ country/language-specific datasets
#          - Each contains raw survey responses with minimal preprocessing
#
# OUTPUTS: Cleaned .sav files written to DIR_CLEAN
#          - [dataset]_clean.sav - Standard cleaned version
#          - [dataset]_clean_more_filters.sav - US datasets with additional filters
#          - clean_summary.xlsx - Audit trail showing how many responses removed at each step
#
# CLEANING STEPS (8 quality filters applied):
#   1. Missing Response: Remove rows with missing core scale data (FTOS, LPS)
#   2. Short Duration: Remove responses submitted too quickly (<10 min for some datasets)
#   3. Constant Answers: Remove rows where participant gave same answer to all items
#   4. Zigzag Patterns: Remove alternating response patterns (1-7-1-7-1-7...)
#   5. Mahalanobis Distance: Remove statistical outliers (multivariate outliers)
#   6. Guttman Errors: Remove response patterns inconsistent with scale structure
#   7. Attention Checks: Remove participants who failed attention control items
#   8. Age Filters: Remove participants outside target age range (US only)
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
# ============================================================================

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

# ============================================================================
# SETUP: Prepare output directory and load input files
# ============================================================================

# Create clean output directory if it doesn't exist
# recursive=TRUE ensures parent directories are created if needed
if (!dir.exists(DIR_CLEAN)) {
  dir.create(DIR_CLEAN, recursive = TRUE)
  message("Created directory: ", DIR_CLEAN)
}

# Get list of all processed SPSS files to clean
file_list <- list.files(
  path = DIR_PROCESSED,              # Directory containing processed files
  pattern = "\\.sav$",               # Only .sav files (SPSS format)
  full.names = TRUE                  # Return full paths (not just filenames)
)

# ============================================================================
# DATASET GROUPINGS: Define which datasets get which filters
# ============================================================================
# These groupings are loaded from config/paths.R
# They allow applying different filters to different subsets of data

br_pt <- DATASETS$br_pt                    # Brazil & Portugal datasets
ch <- DATASETS$ch                          # China datasets
us <- DATASETS$us                          # USA datasets
ch_us <- DATASETS$ch_us                    # Combined China & USA
ch_us_10_min <- DATASETS$ch_us_10_min      # Datasets to filter for <10 min duration
first_stage <- DATASETS$first_stage        # All first-stage surveys
first_stage_br_pt <- DATASETS$first_stage_br_pt  # Brazil/Portugal first-stage
first_stage_ch <- DATASETS$first_stage_ch  # China first-stage

# ============================================================================
# CLEANING STEP 1: MISSING RESPONSE FILTER
# ============================================================================
# PURPOSE: Remove participants who didn't answer core scales
# RATIONALE: Can't calculate scale scores without complete data
#
# FILTERS:
#   - FTOS_v1 missing: Remove if missing first-stage FTOS (except BR/PT)
#   - FTOS_v2 or LPS missing: Remove if missing second-stage scales
#   - FTOS_pilot missing: Remove if missing pilot FTOS
#   - FTOS or Psy_LOT missing: Remove if missing FTOS or LOT (BR/PT only)
#
# NOTE: Different datasets use different scale versions (v1, v2, pilot)
#       so need separate checks for each
# ============================================================================
step_1 <- mk_group("Missing response",
  steps = list(
    # Check 1: First-stage FTOS (FTOS_v1)
    # Skip Brazil/Portugal (they use different scale combination)
    mk_step(
      "FTOS_v1 missing",
      step_drop_na_block(),               # Default pattern checks FTOS_v1
      exclude = br_pt                     # Don't apply to BR/PT datasets
    ),
    
    # Check 2: Second-stage scales (FTOS_v2 or LPS_v2)
    # Remove if missing either scale (both needed for second-stage)
    mk_step(
      "FTOS_v2 or LPS missing",
      step_drop_na_block("^(FTOS_v2_\\d+|LPS_v2_\\d+)$"),  # Regex: FTOS_v2_1, LPS_v2_1, etc.
      exclude = c(br_pt, "IT_extra")      # Skip BR/PT and Italian extra
    ),
    
    # Check 3: Pilot FTOS
    # No exclusions - applies to all datasets with pilot data
    mk_step(
      "FTOS_pilot missing",
      step_drop_na_block("^FTOS_pilot_\\d+$")  # Regex: FTOS_pilot_1, FTOS_pilot_2, etc.
    ),
    
    # Check 4: FTOS or Psy_LOT (Brazil/Portugal specific)
    # BR/PT use different scale structure: FTOS (any version) + LOT scale
    mk_step(
      "FTOS or Psy_LOT missing",
      step_drop_na_block("^(Psy_LOT\\d+|FTOS_(?:pilot|v1|v2)_\\d+)$"),  # LOT or any FTOS version
      datasets = br_pt                    # ONLY apply to BR/PT datasets
    )
  )
)

# ============================================================================
# CLEANING STEP 2: CONSTANT ANSWER FILTER
# ============================================================================
# PURPOSE: Remove participants who gave the same answer to all items in a scale
# RATIONALE: Indicates participant wasn't reading questions (satisficing behavior)
#
# EXAMPLE: Responding "4" to all 15 FTOS items (straight-lining)
#
# SCALES CHECKED:
#   - Core scales: FTOS_v1, FTOS_v2, FTOS_pilot, DGI, LOT
#   - Country-specific: IPIP (BR/PT), LS (China), MLQ (BR/PT/CH/US), AS (BR/PT/CH/US), GRIT (US)
#
# NOTE: Different datasets have different scales, so filters are targeted
# ============================================================================
constant_and_binary <- mk_group(
  "Drop rows with constant responses",
  steps = list(
    # Core FTOS scales (different versions for different survey stages)
    mk_step("FTOS_v1", step_constant_answers()),  # First-stage FTOS (default pattern)
    
    mk_step(
      "FTOS_v2", 
      step_constant_answers("^FTOS_v2_\\d+$"),    # Second-stage FTOS
      datasets = c("IT_extra")                    # Only Italian extra dataset
    ),
    
    mk_step(
      "FTOS_pilot", 
      step_constant_answers("^FTOS_pilot_\\d+$")  # Pilot version FTOS
    ),
    
    # Brazil pilot scales: DGI and LOT (split from LoTeDGI)
    mk_step(
      "DGI", 
      step_constant_answers(col_pattern = "^Psy_DGI\\d+$")   # DGI items: Psy_DGI1, Psy_DGI2, etc.
    ),
    
    mk_step(
      "LOT", 
      step_constant_answers(col_pattern = "^Psy_LOT\\d+$")   # LOT items: Psy_LOT1, Psy_LOT2, etc.
    ),
    
    # Brazil/Portugal specific scales
    mk_step(
      "IPIP",
      step_constant_answers(col_pattern = "^IPIP_\\d+$"),    # Big Five personality inventory
      datasets = br_pt                                       # Only BR/PT datasets
    ),
    
    # China-specific scale
    mk_step(
      "LS",
      step_constant_answers(col_pattern = "^LS_BRS\\d+$"),   # Life Satisfaction - Brief Resilience Scale
      datasets = ch                                          # Only Chinese datasets
    ),
    
    # Multi-country scales (BR/PT, China, US)
    mk_step(
      "MLQ",
      step_constant_answers(col_pattern = "^MLQ_\\d+$"),     # Meaning in Life Questionnaire
      datasets = c(br_pt, ch_us)                             # BR/PT + China + US
    ),
    
    mk_step(
      "AS",
      step_constant_answers(col_pattern = "^AS_\\d+$"),      # Authenticity Scale
      datasets = c(br_pt, ch_us)                             # BR/PT + China + US
    ),
    
    # US-specific scale
    mk_step(
      "GRIT",
      step_constant_answers(col_pattern = "^GRIT_\\d+$"),    # Grit Scale (perseverance)
      datasets = us                                          # Only US datasets
    )
  )
)

# ============================================================================
# CLEANING STEP 3: ATTENTION CHECK FILTER
# ============================================================================
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
# ============================================================================
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

# ============================================================================
# CLEANING STEP 4: ZIGZAG PATTERN FILTER
# ============================================================================
# PURPOSE: Remove participants with alternating response patterns
# RATIONALE: Pattern like 1-7-1-7-1-7 or 2-6-2-6 indicates random/careless responding
#
# EXAMPLE: FTOS items answered: [1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1, 7, 1]
#          This is statistically unlikely and suggests inattention
#
# DETECTION: step_detect_zigzag() identifies systematic alternation in responses
#
# SCALES CHECKED: Applied to all major scales, tailored by dataset content
#   - Core: FTOS (v1, v2, pilot), LPS (v1, v2)
#   - BR/PT: MLQ, AS, IPIP
#   - China: MLQ, AS, CAAS, ESS, ESW
#   - US: MLQ, AS, GRIT
#   - Slovenia: DASS
#   - Italy: IT_IT, DMF
# ============================================================================
    mk_step(
      "FTOS1",
      step_detect_zigzag(col_pattern = "^FTOS_v1_\\d+$"),
      datasets = first_stage
    ),
    mk_step(
      "FTOS2",
      step_detect_zigzag(col_pattern = "^FTOS_v2_\\d+$"),
      datasets = c("IT_extra")
    ),
    mk_step(
      "LPS",
      step_detect_zigzag(col_pattern = "^LPS_v1_\\d+$"),
      datasets = first_stage
    ),
    mk_step(
      "LPS2",
      step_detect_zigzag(col_pattern = "^LPS_v2_\\d+$"),
      datasets = c("IT_extra")
    ),

    # --- BR/PT, US, CH: MLQ  (QSV in pilot), AS (EA in pilot) ---
    mk_step(
      "MLQ",
      step_detect_zigzag(col_pattern = "^MLQ_\\d+$"),
      datasets = c(first_stage_br_pt, us, first_stage_ch, "IT_extra")
    ),
    mk_step(
      "AS",
      step_detect_zigzag(col_pattern = "^AS_\\d+$"),
      datasets = c(first_stage_br_pt, us, first_stage_ch, "IT_extra")
    ),

    # --- BR/PT: IPIP and EASY ---
    mk_step(
      "IPIP",
      step_detect_zigzag(col_pattern = "^IPIP_\\d+$"),
      datasets = first_stage_br_pt
    ),
    # mk_step("EASY",      step_detect_zigzag(col_pattern = "^EASY_\\d+$"),
    #         datasets = br_pt),

    # --- CH: CAAS (complete), ESS, ESW ---
    mk_step(
      "CAAS",
      step_detect_zigzag(col_pattern = "^CAAS_\\d+$"),
      datasets = first_stage_ch
    ),
    mk_step(
      "ESS",
      step_detect_zigzag(col_pattern = "^ES_\\d+$"),
      datasets = first_stage_ch
    ),
    mk_step(
      "ESW",
      step_detect_zigzag(col_pattern = "^ESW_PS\\d+$"),
      datasets = first_stage_ch
    ),

    # --- US: GRIT ---
    mk_step(
      "GRIT",
      step_detect_zigzag(col_pattern = "^GRIT_\\d+$"),
      datasets = us
    ),

    # --- SL: DASS ---
    mk_step(
      "DASS",
      step_detect_zigzag(col_pattern = "^DASS_\\d+$"),
      datasets = c("SL_277273")
    ),

#   - Italy: IT_IT, DMF
# ============================================================================
remove_zigzag <- mk_group(
  "Remove zigzag answers",
  steps = list(
    # -------------------------------------------------------------------------
    # Core scales - All first-stage datasets
    # -------------------------------------------------------------------------
    mk_step(
      "FTOS1",
      step_detect_zigzag(col_pattern = "^FTOS_v1_\\d+$"),   # First-stage FTOS
      datasets = first_stage                                # All first-stage surveys
    ),
    
    mk_step(
      "FTOS2",
      step_detect_zigzag(col_pattern = "^FTOS_v2_\\d+$"),   # Second-stage FTOS
      datasets = c("IT_extra")                              # Italian extra only
    ),
    
    mk_step(
      "LPS",
      step_detect_zigzag(col_pattern = "^LPS_v1_\\d+$"),    # First-stage LPS
      datasets = first_stage                                # All first-stage surveys
    ),
    
    mk_step(
      "LPS2",
      step_detect_zigzag(col_pattern = "^LPS_v2_\\d+$"),    # Second-stage LPS
      datasets = c("IT_extra")                              # Italian extra only
    ),

    # -------------------------------------------------------------------------
    # Multi-country scales (BR/PT, US, China)
    # -------------------------------------------------------------------------
    mk_step(
      "MLQ",
      step_detect_zigzag(col_pattern = "^MLQ_\\d+$"),       # Meaning in Life Questionnaire
      datasets = c(first_stage_br_pt, us, first_stage_ch, "IT_extra")  # BR/PT + US + CH + IT
    ),
    
    mk_step(
      "AS",
      step_detect_zigzag(col_pattern = "^AS_\\d+$"),        # Authenticity Scale
      datasets = c(first_stage_br_pt, us, first_stage_ch, "IT_extra")  # BR/PT + US + CH + IT
    ),

    # -------------------------------------------------------------------------
    # Brazil/Portugal specific scales
    # -------------------------------------------------------------------------
    mk_step(
      "IPIP",
      step_detect_zigzag(col_pattern = "^IPIP_\\d+$"),      # Big Five personality (BR/PT only)
      datasets = first_stage_br_pt
    ),

    # -------------------------------------------------------------------------
    # China specific scales
    # -------------------------------------------------------------------------
    mk_step(
      "CAAS",
      step_detect_zigzag(col_pattern = "^CAAS_\\d+$"),      # Career Adapt-Abilities Scale (complete version)
      datasets = first_stage_ch
    ),
    
    mk_step(
      "ESS",
      step_detect_zigzag(col_pattern = "^ES_\\d+$"),        # Existential Scale
      datasets = first_stage_ch
    ),
    
    mk_step(
      "ESW",
      step_detect_zigzag(col_pattern = "^ESW_PS\\d+$"),     # Existential Scale - Work
      datasets = first_stage_ch
    ),

    # -------------------------------------------------------------------------
    # US specific scale
    # -------------------------------------------------------------------------
    mk_step(
      "GRIT",
      step_detect_zigzag(col_pattern = "^GRIT_\\d+$"),      # Grit Scale (perseverance)
      datasets = us
    ),

    # -------------------------------------------------------------------------
    # Slovenia specific scale
    # -------------------------------------------------------------------------
    mk_step(
      "DASS",
      step_detect_zigzag(col_pattern = "^DASS_\\d+$"),      # Depression Anxiety Stress Scales
      datasets = c("SL_277273")                             # Slovenia only
    ),

    # -------------------------------------------------------------------------
    # Italy specific scales
    # -------------------------------------------------------------------------
    mk_step(
      "IT",
      step_detect_zigzag(col_pattern = "^IT_\\d+$"),        # Italian Time Perspective scale
      datasets = c("IT_277273", "IT_extra")
    ),
    
    mk_step(
      "DMF",
      step_detect_zigzag(col_pattern = "^DMF_\\d+$"),       # Decision Making Fluency
      datasets = c("IT_277273", "IT_extra")
    )
  )
)

# ============================================================================
# SCALE PATTERNS: Define regex patterns for all scales
# ============================================================================
# Used by Mahalanobis and Guttman filters to identify scale columns
# Each pattern matches all items of a particular scale
# ============================================================================
scale_patterns_list <- list(
  FTOS = "^FTOS_v1_\\d+$",           # First-stage FTOS items
  FTOS_v2 = "^FTOS_v2_\\d+$",        # Second-stage FTOS items
  FTOS_pilot = "^FTOS_pilot_\\d+$",  # Pilot FTOS items
  LPS = "^LPS_v1_\\d+$",             # First-stage LPS items
  LPS_v2 = "^LPS_v2_\\d+$",          # Second-stage LPS items
  CAAS = "^CAAS_\\d+$",              # Career Adapt-Abilities Scale
  DGI = "^(Psy_)?DGI_?\\d+$",        # DGI scale (with/without Psy_ prefix)
  SWLS = "^SWLS_\\d+$",              # Satisfaction With Life Scale
  IT = "^IT_\\d+$",                  # Italian Time Perspective
  Pr = "^Pr_\\d+$",                  # Presence scale
  DMF = "^DMF_\\d+$",                # Decision Making Fluency
  MLQ = "^MLQ_\\d+$",                # Meaning in Life Questionnaire
  AS = "^AS_\\d+$",                  # Authenticity Scale
  LS = "^LS_BRS\\d+$",               # Life Satisfaction - Brief Resilience Scale
  ESW = "^ESW_PS\\d+$",              # Existential Scale - Work
  ESS = "^ES_\\d+$",                 # Existential Scale
  FS = "^FS_\\d+$",                  # Flourishing Scale
  GRIT = "^GRIT_\\d+$",              # Grit Scale
  IPIP = "^IPIP_\\d+$",              # Big Five personality (IPIP)
  LOT = "^Psy_LOT\\d+$"              # LOT scale (Life Orientation Test)
)

# ============================================================================
# CLEANING STEPS 5 & 6: MAHALANOBIS DISTANCE & GUTTMAN ERROR FILTERS
# ============================================================================
# PURPOSE: Remove statistical outliers and inconsistent response patterns
#
# MAHALANOBIS DISTANCE:
#   - Identifies multivariate outliers (responses far from group center)
#   - Example: Someone scoring 7 on all items when group average is 4
#   - Uses chi-square distribution to determine outlier threshold
#
# GUTTMAN ERRORS:
#   - Identifies response patterns inconsistent with scale structure
#   - Based on Item Response Theory (IRT) assumptions
#   - Example: Agreeing with hard items but disagreeing with easy items
#
# APPLICATION: Only applied to first-stage surveys (most comprehensive data)
# ============================================================================
mahalanobis_guttman <- mk_group("Mahalanobis/Guttman", steps = list(
  mk_step(
    "Mahalanobis",
    step_mahalanobis(scale_patterns_list),   # Check all scales for multivariate outliers
    datasets = first_stage                   # Only first-stage surveys
  ),
  
  mk_step(
    "Guttman", 
    step_guttman(scale_patterns_list),       # Check all scales for response inconsistency
    datasets = first_stage                   # Only first-stage surveys
  )
))

# ============================================================================
# ASSEMBLE COMPLETE CLEANING PIPELINE
# ============================================================================
# Combines all cleaning steps in execution order
# Some steps are dataset-specific, handled by the pipeline framework
# ============================================================================
steps <- list(
  # Step 1: Missing response filter
  step_1,
  
  # Step 2: Short duration filter (<10 minutes for China/US datasets)
  list(
    name = "Drop short submitted responses (<10 min)",
    fn = step_filter_min_duration(),
    datasets = ch_us_10_min                      # Only CH/US datasets with time concern
  ),
  
  # Step 3: Constant answer filter (removed - see commented example below)
  # list(
  #  name = "Remove foreigners",
  #  fn = step_remove_foreigners(),
  #  datasets = us
  # ),
  
  # Step 4: Age filter (removed - applied separately later for US)
  # mk_step(name = "Remove > 65", step_filter_age(), datasets = us),
  
  # Step 5: Constant and binary pattern filter
  constant_and_binary,
  
  # Step 6: Zigzag pattern filter
  remove_zigzag,
  
  # Step 7 & 8: Statistical outlier detection
  mahalanobis_guttman,
  
  # Step 9: Attention check filter
  check_attention
)

# ============================================================================
# MAIN PROCESSING LOOP: Apply cleaning pipeline to all datasets
# ============================================================================
# For each processed file:
#   1. Load data
#   2. Run cleaning pipeline (applies applicable filters)
#   3. Build audit trail summary
#   4. Write cleaned file to DIR_CLEAN
#   5. Keep summary for final report
# ============================================================================

# Initialize storage for all summaries
all_summaries <- list()

# Process each file in the processed directory
for (f in file_list) {
  # Wrap in tryCatch to continue even if one file fails
  tryCatch(
    {
      # Load SPSS file
      df <- read_sav(f)
      
      # Extract filename without extension (e.g., "CH_277273")
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

# ============================================================================
# CREATE SUMMARY REPORT
# ============================================================================
# Combine all individual dataset summaries into one Excel file
# Shows initial N, final N, and removals at each step for every dataset
# ============================================================================
summary_all <- bind_rows(all_summaries)
write_xlsx(summary_all, "clean_summary.xlsx")
message("Cleaning summary saved to: clean_summary.xlsx")

# ============================================================================
# US-SPECIFIC ADDITIONAL FILTERING
# ============================================================================
# PURPOSE: Create more restrictive versions of US datasets with additional filters
# RATIONALE: US datasets may require stricter inclusion criteria for some analyses
#
# ADDITIONAL FILTERS:
#   1. Remove foreigners: Exclude non-US nationals (for US-specific analyses)
#   2. Remove >65 years old: Exclude older adults (if targeting emerging/young adults)
#
# OUTPUT: [dataset]_clean_more_filters.sav (in addition to standard _clean.sav)
#
# NOTE: These filters are NOT applied to standard cleaned files, only to
#       "more_filters" versions. This preserves flexibility for different analyses.
# ============================================================================

# Define additional filter steps (US-specific)
us_additional_filters <- list(
  list(
    name = "Remove foreigners",
    fn = step_remove_foreigners(),         # Remove participants with non-US nationality
    datasets = us
  ),
  mk_step(
    name = "Remove > 65", 
    step_filter_age(),                     # Remove participants older than 65
    datasets = us
  )
)

# Apply additional filters to all US datasets
for (dataset_name in us) {
  tryCatch(
    {
      # Check if standard cleaned file exists
      clean_file <- file.path(DIR_CLEAN, paste0(dataset_name, "_clean.sav"))
      
      if (file.exists(clean_file)) {
        # Load the already-cleaned file
        df_clean <- read_sav(clean_file)

        # Apply additional US-specific filters
        # This creates a more restrictive subset
        res_more <- run_cleaning_pipeline(df_clean, dataset_name, us_additional_filters)
        df_more_filtered <- res_more$df_clean

        # Save with "_clean_more_filters" suffix
        # This preserves both versions for different analysis needs
        more_filters_file <- file.path(DIR_CLEAN, paste0(dataset_name, "_clean_more_filters.sav"))
        write_sav(df_more_filtered, more_filters_file)
        
        message("Created ", dataset_name, "_clean_more_filters.sav")
      } else {
        # Skip if standard cleaned file doesn't exist
        message("Clean file not found for ", dataset_name)
      }
    },
    error = function(e) {
      # Print error but continue with next US dataset
      message("Error while creating more filtered version for ", dataset_name, ": ", conditionMessage(e))
    }
  )
}

# ============================================================================
# END OF 02_clean.R
# ============================================================================
# SUMMARY: This script has applied 8+ quality filters to ~40 datasets,
#          removing invalid responses while preserving audit trail.
#
# OUTPUTS CREATED:
#   - ~40 [dataset]_clean.sav files (standard cleaning)
#   - Multiple [dataset]_clean_more_filters.sav files (US datasets with extra filters)
#   - clean_summary.xlsx (comprehensive audit trail)
#
# NEXT STEP: Run 03_merge_general.R to combine all cleaned datasets
# ============================================================================