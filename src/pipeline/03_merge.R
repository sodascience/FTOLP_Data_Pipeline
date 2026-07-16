# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# 03_merge.R - COMBINE ALL CLEANED DATASETS INTO SINGLE FILE
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Merge all cleaned country/language-specific datasets into one
#          comprehensive dataset for analysis. Handles type mismatches,
#          missing value codes, and data structure inconsistencies.
#
# INPUTS:  Cleaned .sav files from DIR_CLEAN (output of 02_clean.R)
#          - ~40 [dataset]_clean.sav files
#          - Each contains cleaned responses with SPSS labelled format
#
# OUTPUTS: Combined dataset, left in global environment as "merged_df" and
#          written to DIR_MERGED as merged_dataset.sav / .csv / .xlsx
#          - Single dataframe with all participants
#          - Standardized column types
#          - Consistent missing value coding
#          - SPSS-compatible labelled format preserved
#
# KEY CHALLENGES SOLVED:
#   1. Type mismatches: Same column stored as character in one dataset, numeric in another
#   2. Missing value codes: Different datasets use different NA codes (999, -99, etc.)
#   3. Label conflicts: Duplicate/conflicting value labels when merging datasets
#   4. Padding NAs: bind_rows() creates NAs for columns not present in all datasets
#
# PROCESSING STEPS:
#   1. Load all cleaned files
#   2. Standardize column types (categorical→character, numeric→numeric)
#   3. Label existing missing values with SPSS codes (990-999)
#   4. Normalize value labels pre-merge to avoid bind_rows() label-conflict warnings
#   5. Merge all datasets with bind_rows()
#   6. Label padding NAs (990 = "by design", i.e. variable not collected in that dataset)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load required libraries
library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(writexl)
library(lubridate)
library(here)

# Preparation ----
# Load configuration and helper functions
source(here::here("config", "paths.R"))
source(here::here("src", "utils", "merge_functions.R"))
source(here::here("src", "utils", "validation.R"))
source(here::here("config", "scales.R"))

# LOAD ALL CLEANED DATASETS
# Read all .sav files from clean directory and subfolders and store in named list
file_list <- list.files(
  path = DIR_CLEAN,                   # Clean directory (output from 02_clean.R)
  pattern = "\\.sav$",                # Only .sav files
  full.names = TRUE,                  # Full paths needed for reading
  recursive = TRUE                    # Include subdirectories
)

# Load all files into list of dataframes
dfs <- lapply(file_list, read_sav)

# Name each dataframe by its filename (without .sav extension)
# E.g., "CN_277273_clean.sav" becomes "CN_277273_clean"
names(dfs) <- gsub("\\.sav$", "", basename(file_list))

# Add source_dataset column to each dataframe
for (df_name in names(dfs)) {
  dfs[[df_name]]$source_dataset <- df_name
}

# Define SPSS missing value codes
# These will be applied to all numeric columns when merging
# NOTE: SPSS only allows up to 3 user-missing values per variable
# We use: 990 (by design), 991 (technical error), 999 (missing)
all_na_values <- c(990, 991, 999)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Rename variables based on data collection stage
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Gender: For pilot and first stage datasets, Gender_v1; others: Gender_v2
# Use the Datasets list from config/paths.R to identify which datasets belong to which stage
for (df_name in names(dfs)) {
  # if df_name starts with any of the datasets in first_stage or "BR_PILOT", rename Gender to Gender_v1,
  # else rename Gender to Gender_v2
  if (any(grepl(paste(DATASETS$first_stage, collapse = "|"), df_name)) || grepl("^BR_PILOT", df_name)) {
    dfs[[df_name]] <- dfs[[df_name]] %>%
      rename(Gender_v1 = Gender)
  } else {
    dfs[[df_name]] <- dfs[[df_name]] %>%
      rename(Gender_v2 = Gender)
  }
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Process Nationality, Citizen, and Origin variables ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Nationality means different things depending on the collection stage:
#
#   PILOT / FIRST_STAGE / "auto" datasets:
#     Nationality = country code with categorical labels → keep as character country name
#
#   SECOND_STAGE datasets (all others):
#     Nationality = citizen indicator (yes/no, labelled, may vary by language
#     and encoding: 0/1, 1/2, etc.) → decode using label text → Citizen (0/1)
#     yes = is a citizen (1), no = not a citizen (0)
#
# Origin = country where participant grew up → character country name

# Stage 1 auto datasets: have country-name Nationality (like first_stage)
# Stage 2 auto datasets (RU_AUTO_1, NL_AUTO, etc.) have yes/no Nationality like other second-stage datasets
FIRST_STAGE_AUTO <- c("IT_AUTO")

# Helper: TRUE if df_name belongs to pilot, first_stage, or Stage 1 auto groups
is_country_nationality <- function(df_name) {
  any(sapply(DATASETS$pilot,       function(p) grepl(p, df_name, fixed = TRUE))) ||
  any(sapply(DATASETS$first_stage, function(p) grepl(p, df_name, fixed = TRUE))) ||
  any(sapply(FIRST_STAGE_AUTO,     function(p) grepl(p, df_name, fixed = TRUE)))
}

# VALIDATE: every DATASETS$pilot / DATASETS$first_stage / FIRST_STAGE_AUTO entry
# should match at least one dataset actually loaded from DIR_CLEAN. A token
# matching nothing means is_country_nationality() silently misclassifies that
# dataset's stage - the same failure mode that broke Gender/Nationality
# routing for BR_PILOT before its filename casing was fixed to match
# DATASETS$pilot.
assert_datasets_exist(
  c(DATASETS$pilot, DATASETS$first_stage, FIRST_STAGE_AUTO),
  names(dfs),
  context = "03_merge.R pilot/first_stage/auto groupings",
  fixed = TRUE
)

# Multilingual pattern for "yes / is a citizen"
# Languages inferred from dataset name prefixes (CN, EN, ES, IT, BRPT, SI, NL)
# plus additional languages anticipated in second-stage data collection
citizen_yes_pattern <- paste0(
  "^(",
  "yes",            # English (EN)
  "|ja",            # German (DE), Dutch (NL), Slovenian (SI), Scandinavian
  "|s[i\u00ed\u00ec]", # Spanish s\u00ed (ES), Italian s\u00ec (IT), informal si
  "|sim",           # Portuguese (BRPT)
  "|oui",           # French (FR)
  "|да",            # Russian (RU), Bulgarian, Serbian/Macedonian Cyrillic
  "|tak",           # Polish (PL)
  "|так",            # Ukrainian Cyrillic
  "|ano",           # Czech (CZ)
  "|\u00e1no",      # Slovak
  "|da",            # Croatian (HR), Serbian Latin, Romanian (RO), Albanian
  "|evet",          # Turkish (TR)
  "|igen",          # Hungarian (HU)
  "|ναι",            # Greek (GR)
  "|نعم",            # Arabic (AR)
  "|\u662f",        # Chinese \u662f (CN)
  "|\u662f\u7684",  # Chinese \u662f\u7684
  "|\uc608",        # Korean formal \uc608
  "|\ub124",        # Korean informal \ub124
  "|כן",            # Hebrew (HE)
  "|ใช่",            # Thai (TH)
  "|हाँ",            # Hindi (HI)
  "|c\u00f3",       # Vietnamese (VI) c\u00f3
  "|kyll\u00e4",    # Finnish (FI) kyll\u00e4
  "|ta",            # various
  ")"
)

# Per-dataset regex patterns for deriving Citizen from decoded Nationality strings.
# Covers both the local-language label variants (as surveys were authored in the
# local language) and English equivalents (some SPSS files use English labels).
#
#   CN_*    : Chinese labels — Mainland China, HK, Macau, Taiwan
#   ES_*    : Spanish labels — España / Spain
#   IT_*    : Italian labels — Italia / Italy
#   BRPT_*  : Portuguese labels — Brasil / Brazil, Portugal (mixed pool, not separable)
#   SI_*    : Slovenian labels — Slovenija / Slovenia
#   US_*    : English labels — United States / USA
citizen_country_patterns <- list(
  CN    = paste0(
    "\u4e2d\u56fd\u5927\u9646|\u4e2d\u56fd|\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd",  # 中国大陆 / 中国 / 中华人民共和国
    "|\u9999\u6e2f",                           # 香港
    "|\u6fb3\u95e8|\u6fb3\u9580",               # 澳门 / 澳門
    "|\u53f0\u6e7e|\u53f0\u7063",               # 台湾 / 台灣
    "|Mainland China|Hong Kong|Macau|Macao|Taiwan|China"
  ),
  ES    = "Espa\u00f1a|Espana|Spain",            # España
  IT    = "Italia|Italy|Itália",
  BRPT  = "Brasil|Brazil|Portugal",
  SI    = "Slovenija|Slovenia",
  US    = paste0(
    "United States|United States of America|USA|U\\.S\\.A\\.",
    "|U\\.S\\.|America"
  )
)

# Helper: return the citizen pattern for a given df_name, or NULL if not found
get_citizen_pattern <- function(df_name) {
  if      (grepl("^CN",           df_name)) citizen_country_patterns[["CN"]]
  else if (grepl("^ES",           df_name)) citizen_country_patterns[["ES"]]
  else if (grepl("^IT",           df_name)) citizen_country_patterns[["IT"]]
  else if (grepl("^BRPT|^BR_PILOT", df_name)) citizen_country_patterns[["BRPT"]]
  else if (grepl("^SI",           df_name)) citizen_country_patterns[["SI"]]
  else if (grepl("^US",           df_name)) citizen_country_patterns[["US"]]
  else NULL
}

for (df_name in names(dfs)) {
  df <- dfs[[df_name]]

  # --- Nationality ---
  if ("Nationality" %in% names(df)) {
    nat_col <- df[["Nationality"]]

    if (is_country_nationality(df_name)) {
      # Pilot / first_stage / auto: country code → decode to country name string
      if (is.labelled(nat_col)) {
        df[["Nationality"]] <- as.character(as_factor(nat_col))
      } else {
        df[["Nationality"]] <- as.character(nat_col)
      }
      # Row-level NAs: variable present but participant did not answer → true missing
      df[["Nationality"]][is.na(df[["Nationality"]])] <- "999"

      # Derive Citizen from the decoded Nationality value:
      #   - "auto" datasets: all participants are local → Citizen = 1
      #   - other pilot/first_stage: match decoded country name against known patterns
      #   - Nationality = "999" (true missing) → Citizen = NA
      if (grepl("auto", df_name, ignore.case = TRUE)) {
        df[["Citizen"]] <- 1L
      } else {
        pattern <- get_citizen_pattern(df_name)
        if (!is.null(pattern)) {
          nat_decoded <- df[["Nationality"]]
          df[["Citizen"]] <- as.integer(
            grepl(pattern, nat_decoded, ignore.case = TRUE, perl = TRUE)
          )
          # True-missing Nationality → Citizen is also missing
          df[["Citizen"]][nat_decoded == "999"] <- NA_integer_
        } else {
          # VALIDATE: no citizen_country_patterns entry matched this dataset's
          # prefix - Citizen never gets created for these rows at all (not
          # even as NA), which is exactly how the dead "EN" branch behaved.
          warning(
            sprintf(
              "[03_merge.R get_citizen_pattern] No citizen pattern matched '%s' - Citizen will not be created for this dataset.",
              df_name
            ),
            call. = FALSE
          )
        }
      }
    } else {
      # Second-stage: citizen indicator → normalize to 0/1
      # Decode label text (handles multilingual labels) then match against "yes" pattern
      # 1 = citizen of the country, 0 = not a citizen
      label_strings <- as.character(as_factor(nat_col))
      citizen_vals <- as.integer(
        grepl(citizen_yes_pattern, trimws(label_strings), ignore.case = TRUE)
      )
      # Propagate R NAs (true missing responses)
      citizen_vals[is.na(label_strings)] <- NA_integer_
      df[["Citizen"]]     <- citizen_vals
      df[["Nationality"]] <- NULL
    }
  }

  # --- Origin ---
  if ("Origin" %in% names(df)) {
    origin_col <- df[["Origin"]]
    if (is.labelled(origin_col)) {
      df[["Origin"]] <- as.character(as_factor(origin_col))
    } else {
      df[["Origin"]] <- as.character(origin_col)
    }
    # Row-level NAs: variable present but participant did not answer → true missing
    df[["Origin"]][is.na(df[["Origin"]])] <- "999"
  }

  # Stage 1 auto datasets contain only local participants → Citizen = 1 unconditionally.
  # Stage 2 auto datasets derive Citizen from their yes/no Nationality column (handled above).
  if (any(sapply(FIRST_STAGE_AUTO, function(p) grepl(p, df_name, fixed = TRUE)))) {
    df[["Citizen"]] <- 1L
  }

  dfs[[df_name]] <- df
}

# VALIDATE: flag any dataset where Citizen ended up NA for every single row.
# This is the signature a silently-unmatched routing branch would leave behind
# (e.g. get_citizen_pattern() returning NULL for every row, or
# is_country_nationality() misclassifying the dataset's stage) - exactly what
# happened when the "EN" citizen-pattern branch went dead after EN_277273.sav/
# EN_999625.sav stopped being generated.
for (df_name in names(dfs)) {
  citizen_col <- dfs[[df_name]][["Citizen"]]
  if (!is.null(citizen_col) && nrow(dfs[[df_name]]) > 0 && all(is.na(citizen_col))) {
    warning(
      sprintf(
        "[03_merge.R Citizen check] Dataset '%s' has Citizen = NA for every row - likely an unmatched citizen-pattern or stage-routing branch.",
        df_name
      ),
      call. = FALSE
    )
  }
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Define relevant variables ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Define which columns should be treated as categorical (→ character)
categorical_cols <- c(
  "id",                          # Participant identifier (e.g., "CN_277273_123")
  "source_dataset",              # Source dataset filename (e.g., "CN_277273_clean")
  "Nationality",                 # Country name (from datasets with country-code Nationality)
  "Origin",                      # Country where participant grew up
  "ImmigrationCountry",          # Country of immigration
  "Sex",
  "Gender_v1",
  "Gender_v2",
  "Gender_other",                # Open-ended gender response
  "Gender_final"
)

# Define which columns should be treated as numeric (→ numeric)
# Scale items come from SCALE_PATTERNS (config/scales.R) - the same registry
# 02_clean.R's QC filters use - rather than a single hand-written FTOS/LPS-only
# regex. Before this fix, that regex meant every non-FTOS/LPS scale
# (CAAS, MLQ, AS, IPIP, GRIT, DASS, IT, DMF, Psy_DGI/Psy_LOT, HS, ES, ESW_PS,
# CAAS_S, Psy_CIPIP, ...) was silently dropped from the merged dataset by the
# select(any_of(relevant_cols)) step below, despite having been specifically
# QC-checked in 02_clean.R (or, for CAAS_S/Psy_CIPIP, despite being legitimate
# scale data never referenced by any filter at all).
all_cols <- unique(unlist(lapply(dfs, names)))
scale_item_cols <- unique(unlist(lapply(SCALE_PATTERNS, function(p) grep(p, all_cols, value = TRUE))))
numerical_cols <- c(
  scale_item_cols,
  # Demographics that should be numeric
  "Age",
  "Citizen",                     # 0/1 citizen indicator (1 = citizen, from second-stage datasets)
  "Immigration",                 # Immigration status indicator
  "ImmigrationTime_years",       # Years since immigration
  "ImmigrationTime_months"       # Months since immigration
)

# Combine all relevant columns into one vector for processing
relevant_cols <- c(categorical_cols, numerical_cols)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Remove unwanted columns ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Remove any columns that are not in the relevant_cols list
dfs <- lapply(dfs, function(df) {
  df %>% select(any_of(relevant_cols))
})

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Column Type Standardization ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PROBLEM: When merging datasets, bind_rows() fails if same column has
#          different types in different datasets
#
# EXAMPLE: Nationality stored as numeric (35) in one dataset, 
#          character ("China") in another → merge error
#
# SOLUTION: Scan all datasets, identify columns with mixed types,
#           standardize to single type before merging
#
# STRATEGY:
#   - Categorical columns (id, Nationality, Origin, etc.) → character
#   - Numeric columns (scale items, demographics) → numeric
#
# WHY: Preserves meaning while allowing merge
#      - Categorical: Labels preserved (e.g., "China" better than 35)
#      - Numeric: Allows calculation (e.g., age, scale scores)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Scan each column and standardize types if mixed
for (col_name in relevant_cols) {
  # Step 1: Identify what type this column has in each dataset
  col_types <- sapply(dfs, function(df) {
    if (col_name %in% names(df)) {
      # Check underlying type (strip labelled wrapper if present)
      if (is.labelled(df[[col_name]])) {
        class(as.vector(df[[col_name]]))[1]   # "numeric", "character", etc.
      } else {
        class(df[[col_name]])[1]
      }
    } else {
      NA_character_                            # Column not in this dataset
    }
  })
  
  # Remove NAs (datasets without this column)
  col_types <- col_types[!is.na(col_types)]
  
  # Step 2: If column has multiple types, standardize them
  if (length(unique(col_types)) > 1) {
    # Decide target type based on whether column is categorical
    target_type <- if (col_name %in% categorical_cols) "character" else "numeric"
    
    # Log the standardization
    message(sprintf("Standardizing column '%s' to %s (found types: %s)", 
                    col_name, target_type, paste(unique(col_types), collapse=", ")))
    
    # Step 3: Convert each dataset's version of this column to target type
    for (df_name in names(dfs)) {
      if (col_name %in% names(dfs[[df_name]])) {
        
        if (target_type == "character") {
          # CATEGORICAL → CHARACTER
          # Preserve label meanings: convert to factor first, then character
          # E.g., Nationality code 35 → "China" (via factor labels)
          if (is.labelled(dfs[[df_name]][[col_name]])) {
            dfs[[df_name]][[col_name]] <- as.character(as_factor(dfs[[df_name]][[col_name]]))
          } else {
            dfs[[df_name]][[col_name]] <- as.character(dfs[[df_name]][[col_name]])
          }
          
        } else {
          # NUMERIC → NUMERIC
          # Strip labels and convert to numeric
          # E.g., labelled(c(1,2,3), labels=c("Low"=1, "High"=3)) → c(1,2,3)
          # Suppress "NAs introduced by coercion" warnings (expected for non-numeric text)
          dfs[[df_name]][[col_name]] <- suppressWarnings(
            as.numeric(as.character(zap_labels(dfs[[df_name]][[col_name]])))
          )
        }
      }
    }
  }
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Label missing values ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PROBLEM: Datasets may have various missing value representations:
#          - R NAs
#          - -99, -999 (old SPSS convention)
#          - 999, 990 and 991 (our convention)
#          But not all are properly labeled for SPSS export
#
# SOLUTION: Apply SPSS labelled_spss() to mark missing value codes:
#           990 = "By design"
#           991 = "Technical error"
#           999 = "R NA (true missing data)"
#
# WHY: SPSS recognizes these as user-missing values, handles them correctly
#      in analyses (exclusion from calculations, listwise/pairwise deletion)
#
# MECHANISM:
#   1. Replace R NAs with 999
#   2. In the Dutch dataset, the item 6 of LPS needs to be set as 991 (missing due to technical error) up to participant 97.
#   3. Apply labelled_spss with all reason_codes
#   4. Mark codes 990-999 as SPSS user-missing values
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

for (df_name in names(dfs)) {
  df <- dfs[[df_name]]

  # Apply labelled_spss to all numeric columns
  df_labeled <- df %>%
    mutate(
      across(
        .cols = where(~ is.numeric(.x) || is.integer(.x)),  # Only numeric/integer columns
        .fns = ~ {
          # Step 1: Temporarily replace R NAs with 999 (nonresponse)
          # This ensures true R NAs get labeled (not left as unlabeled missing)
          column_data <- .x
          old_labels <- tryCatch(val_labels(column_data), error = function(e) NULL)
          column_data[is.na(column_data)] <- 999

          # Step 2: Technical error
          # 2. In the Dutch dataset, the item 6 of LPS needs to be set as 991
          # (missing due to technical error) up to participant 97.
          if (startsWith(df_name, "NL") && cur_column() == "LPS_v2_6") {
            # Parse participant IDs separated by _ (e.g., "NL_AUTO_123" → 123)
            ids <- dfs[[df_name]]$id
            participant_ids <- as.numeric(sub(".*_(\\d+)$", "\\1", ids))
            # Set 991 for participants with IDs up to 97
            column_data[participant_ids <= 97] <- 991
          }

          # Step 3: Create limited label set (only unique labels)
          # Start with the 3 missing value codes
          limited_labels <- c(
            "by_design" = 990,
            "technical_error" = 991,
            "missing" = 999
          )
          
          # Add existing labels for non-missing values (avoid duplicates)
          if (!is.null(old_labels)) {
            # Remove any old labels with values in the 990-999 range
            non_missing_labels <- old_labels[!(old_labels %in% 990:999)]
            # Remove any labels that have duplicate values
            limited_labels <- c(non_missing_labels, limited_labels)
            # Keep only unique values (first occurrence)
            limited_labels <- limited_labels[!duplicated(limited_labels)]
            # Also ensure unique names
            limited_labels <- limited_labels[!duplicated(names(limited_labels))]
          }

          # Step 4: Apply SPSS labelled format with limited missing value codes
          # na_values = only 3 codes (SPSS limit)
          # labels = value-to-label mapping
          labelled_spss(
            x = column_data,
            na_values = all_na_values,           # Only 990, 991, 999 (3 codes max)
            labels = limited_labels              # Labels for present values + missing codes
          )
        }
      )
    )

  # Save labeled dataset back to list
  dfs[[df_name]] <- df_labeled
}



# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PRE-MERGE LABEL NORMALISATION ----
# Prevent bind_rows() warnings caused by conflicting value labels:
#
#   1. Numeric scale items (FTOS_v2, LPS_v2): strip language-specific scale-point
#      labels (e.g. "disagree"=1 vs "en désaccord"=1), keeping only the
#      990/991/999 missing-value labels that are already uniform.
#
#   2. Categorical columns (Gender_v2, etc.): unconditionally convert to character
#      so bind_rows() never sees conflicting labelled-integer vectors.  The type-
#      standardisation loop above only fires on type conflicts, not label conflicts,
#      so labelled-integer columns with different language labels slip through.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
missing_only_labels <- c("by_design" = 990, "technical_error" = 991, "missing" = 999)

dfs <- lapply(dfs, function(df) {
  # 1. Strip scale-point labels from numeric scale columns
  for (col in intersect(numerical_cols, names(df))) {
    col_data <- df[[col]]
    if (is.labelled(col_data)) {
      na_vals <- attr(col_data, "na_values")
      df[[col]] <- labelled_spss(
        x          = unclass(col_data),
        na_values  = if (is.null(na_vals)) all_na_values else na_vals,
        labels     = missing_only_labels
      )
    }
  }
  # 2. Convert categorical columns to plain character
  for (col in intersect(categorical_cols, names(df))) {
    col_data <- df[[col]]
    if (!is.character(col_data)) {
      df[[col]] <- if (is.labelled(col_data)) as.character(as_factor(col_data))
                   else                        as.character(col_data)
    }
  }
  df
})

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# DEFINE FUNCTION: label_merge_NAs()
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Label padding NAs created by bind_rows() merge
#
# PROBLEM: When merging datasets with bind_rows(), columns not present in
#          all datasets get filled with NAs (padding)
#
# EXAMPLE: Dataset A has GRIT scale, Dataset B doesn't
#          After merge: Dataset B rows have NA for all GRIT columns
#          These NAs mean "scale not included", not "participant didn't answer"
#
# SOLUTION: Replace padding NAs with code 990 ("Missing by design")
#
# CRITICAL CHALLENGE: Haven package overrides is.na() for labelled vectors
#                     is.na(999) returns TRUE even though 999 is a numeric value
#                     This would incorrectly replace 999 codes with 990
#
# FIX: Temporarily remove labelled class with unclass()
#      This restores normal R is.na() behavior
#      Only true R NAs get replaced with 990
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
label_merge_NAs <- function(df, code_to_assign = 990) {
  # Get label for the code we're assigning
  assign_label <- names(reason_codes)[reason_codes == code_to_assign]

  df_labeled <- df %>%
    mutate(
      across(
        # Apply to numeric/integer/labelled columns (not character)
        .cols = where(is.numeric) | where(is.integer) | where(is.labelled),
        .fns = ~ {
          column_data <- .x
          
          # Safety check: Skip if column is actually character
          if (is.character(column_data)) {
            return(column_data)
          }

          # CRITICAL FIX: Temporarily remove labelled class
          # Haven's is.na() method treats code 999 as NA (by design)
          # unclass() restores normal R behavior: is.na(999) = FALSE
          # This lets us distinguish:
          #   - True R NA (padding from bind_rows) → replace with 990
          #   - Numeric 999 (existing missing code) → leave alone
          unclassed_data <- unclass(column_data)
          
          # Another safety check: ensure unclassed_data is numeric
          if (!is.numeric(unclassed_data)) {
            return(column_data)
          }

          # Replace ONLY true R NAs with code 990
          # if_else preserves numeric type (no coercion)
          column_data_replaced <- if_else(
            is.na(unclassed_data),               # TRUE only for R NA (not 999)
            as.numeric(code_to_assign),          # Replace with 990
            unclassed_data                       # Keep existing values (including 999)
          )

          # Get existing labels (only keep unique, non-missing value labels)
          old_labels <- tryCatch(val_labels(column_data), error = function(e) NULL)
          limited_labels <- c(
            "by_design" = 990,
            "technical_error" = 991,
            "missing" = 999
          )
          
          if (!is.null(old_labels)) {
            # Remove old labels with values in 990-999 range
            non_missing_labels <- old_labels[!(old_labels %in% 990:999)]
            limited_labels <- c(non_missing_labels, limited_labels)
            # Remove duplicates by value
            limited_labels <- limited_labels[!duplicated(limited_labels)]
            # Remove duplicates by name
            limited_labels <- limited_labels[!duplicated(names(limited_labels))]
          }

          # Re-apply labelled_spss class with all metadata
          # This maintains SPSS compatibility
          labelled_spss(
            x = column_data_replaced,
            na_values = all_na_values,           # Only 990, 991, 999 (3 codes max)
            labels = limited_labels              # Apply limited label set
          )
        }
      )
    )

  return(df_labeled)
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MERGE ALL DATASETS AND LABEL PADDING NAs
# Final step: Combine all datasets and handle merge-created NAs
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
merged_df <- label_merge_NAs(bind_rows(dfs))

# Replace padding NAs in character demographic columns with "990" (missing by design).
# Numeric padding NAs are already handled inside label_merge_NAs(); character columns
# need separate treatment because SPSS user-missing codes only apply to numeric variables.
# At this point any remaining NA in these columns must be a padding NA (the column did
# not exist in that source dataset) because within-dataset NAs were already marked "999".
char_demo_cols <- c("Nationality", "Origin")
for (col in char_demo_cols) {
  if (col %in% names(merged_df)) {
    merged_df[[col]][is.na(merged_df[[col]])] <- "990"
  }
}

# Rearrange columns: put id, info and demographic first, then scales
first_cols <- c("id", "source_dataset", "Nationality", "Citizen", "Origin", "ImmigrationCountry", "Sex", "Gender_v1", "Gender_v2", "Gender_other", "Gender_final", "Age")
first_cols <- first_cols[first_cols %in% names(merged_df)]
scale_cols <- setdiff(names(merged_df), first_cols)
merged_df <- merged_df %>% select(all_of(first_cols), all_of(scale_cols))

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SAVE COMBINED DATASET
# Write the combined dataset to SPSS (.sav file), CSV and EXCEL formats under merged directory
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
output_file_name <- file.path(DIR_MERGED, "merged_dataset")
write_sav(merged_df, paste0(output_file_name, ".sav"))
write_csv(merged_df, paste0(output_file_name, ".csv"))
write_xlsx(merged_df, paste0(output_file_name, ".xlsx"))