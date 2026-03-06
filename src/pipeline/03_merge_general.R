# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# 03_merge_general.R - COMBINE ALL CLEANED DATASETS INTO SINGLE FILE
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Merge all cleaned country/language-specific datasets into one
#          comprehensive dataset for analysis. Handles type mismatches,
#          missing value codes, and data structure inconsistencies.
#
# INPUTS:  Cleaned .sav files from DIR_CLEAN (output of 02_clean.R)
#          - ~40 [dataset]_clean.sav files
#          - Each contains cleaned responses with SPSS labelled format
#
# OUTPUTS: Combined dataset (in global environment as "merged_df")
#          - Single dataframe with all participants
#          - Standardized column types
#          - Consistent missing value coding
#          - SPSS-compatible labelled format preserved
#
# KEY CHALLENGES SOLVED:
#   1. Type mismatches: Same column stored as character in one dataset, numeric in another
#   2. Missing value codes: Different datasets use different NA codes (999, -99, etc.)
#   3. Adults columns: Wide format needs reshaping to long format
#   4. Label conflicts: Duplicate labels when merging datasets
#   5. Padding NAs: bind_rows() creates NAs for columns not present in all datasets
#
# PROCESSING STEPS:
#   1. Load all cleaned files
#   2. Fix "Adults" columns (reshape from wide to long format)
#   3. Standardize column types (categorical→character, numeric→numeric)
#   4. Label existing missing values with SPSS codes (990-999)
#   5. Merge all datasets with bind_rows()
#   6. Label padding NAs (990 = "Variable not included in this dataset")
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load required libraries
library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(readxl)
library(lubridate)
library(here)

# Preparation ----
# Load configuration and helper functions
source(here::here("config", "paths.R"))
source(here::here("src", "utils", "merge_functions.R"))

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
# E.g., "CH_277273_clean.sav" becomes "CH_277273_clean"
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
  # if df_name starts with any of the datasets in first_stage or "br_pilot", rename Gender to Gender_v1, 
  # else rename Gender to Gender_v2
  if (any(grepl(paste(DATASETS$first_stage, collapse = "|"), df_name)) || grepl("^br_pilot", df_name)) {
    dfs[[df_name]] <- dfs[[df_name]] %>%
      rename(Gender_v1 = Gender)
  } else {
    dfs[[df_name]] <- dfs[[df_name]] %>%
      rename(Gender_v2 = Gender)
  }
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Define relevant variables ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Define which columns should be treated as categorical (→ character)
categorical_cols <- c(
  "id",                          # Participant identifier (e.g., "CH_277273_123")
  "source_dataset",              # Source dataset filename (e.g., "CH_277273_clean")
  "Nationality",                 # Country (preserve label like "Brazil" not code 30)
  "Sex",
  "Gender_v1",
  "Gender_v2",
  "Gender_other",                # Open-ended gender response
  "Gender_final"
)

# Define which columns should be treated as numeric (→ numeric)
all_cols <- unique(unlist(lapply(dfs, names)))
numerical_cols <- c(
  # Scale items (e.g., FTOS_pilot_1, FTOS_v1_1, FTOS_v2_1, LPS_pilot_1, etc.)
  grep("^(FTOS|LPS)_(pilot|v1|v2)_\\d+$", all_cols, value = TRUE),
  # Demographics that should be numeric
  "Age"
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
          # 2. In the Dutch datasets, the item 6 of LPS needs to be set as 991 
          # (missing due to technical error) up to participant 97.
          if (startsWith(df_name, "NL") && cur_column() == "LPS_v2_6") {
            # Parse participant IDs separated by _ (e.g., "NL_277273_123" → 123)
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
          # Haven's is.na() method treats code 993 as NA (by design)
          # unclass() restores normal R behavior: is.na(993) = FALSE
          # This lets us distinguish:
          #   - True R NA (padding from bind_rows) → replace with 990
          #   - Numeric 993 (existing missing code) → leave alone
          unclassed_data <- unclass(column_data)
          
          # Another safety check: ensure unclassed_data is numeric
          if (!is.numeric(unclassed_data)) {
            return(column_data)
          }

          # Replace ONLY true R NAs with code 990
          # if_else preserves numeric type (no coercion)
          column_data_replaced <- if_else(
            is.na(unclassed_data),               # TRUE only for R NA (not 993)
            as.numeric(code_to_assign),          # Replace with 990
            unclassed_data                       # Keep existing values (including 993)
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
            na_values = all_na_values,           # Only 990, 991, 993 (3 codes max)
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

# Rearrange columns: put id, info and demographic first, then scales
first_cols <- c("id", "source_dataset", "Nationality", "Sex", "Gender_v1", "Gender_v2", "Gender_other", "Gender_final", "Age")
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