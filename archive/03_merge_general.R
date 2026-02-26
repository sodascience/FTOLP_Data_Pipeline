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
# ============================================================================
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

# ============================================================================
# FIX ADULTS COLUMNS: Reshape from Wide to Long Format
# ============================================================================
# PROBLEM: "Adults" columns use wide format with checkboxes:
#          Adults_mother, Adults_father, Adults_brother, etc. (1=yes, 0=no)
#          Multiple adults selected creates many columns with mostly zeros
#
# SOLUTION: Convert to long format with ranked adults:
#           adult_1 = "mother", adult_2 = "father", adult_3 = "brother"
#
# EXAMPLE TRANSFORMATION:
#   Before: Adults_mother=1, Adults_father=1, Adults_brother=0, Adults_sister=0
#   After:  adult_1="mother", adult_2="father"
#
# WHY: Long format is more efficient and easier to analyze
# ============================================================================

# Step 1: Extract datasets that have Adults columns
dfs_long_adults <- dfs |>
  # Keep only datasets with Adults_ columns
  keep(~ any(startsWith(names(.x), "Adults_"))) |>
  
  # Select just id and Adults_ columns for processing
  map(~ .x |>
    select(id, starts_with("Adults_")) |>
    
    # Fix Adults_brother encoding issue ("Y" → 1, "" → 0)
    # Other Adults_ columns already use 1/0, but brother uses Y/blank
    mutate(
      Adults_brother = case_when(
        Adults_brother == "Y" ~ 1,      # "Y" → 1
        Adults_brother == "" ~ 0,       # blank → 0
        TRUE ~ NA_real_                 # Other values → NA
      ),
    ))

# Step 2: Extract Adults_other column separately (open-ended text)
# This needs special handling because it's not a checkbox
df_other_info <- dfs_long_adults |>
  map(~ {
    if ("Adults_other" %in% names(.x)) {
      .x |> select(id, Adults_other) |> distinct()
    } else {
      NULL
    }
  }) |>
  compact() |>  # Remove NULL entries
  bind_rows() |>
  distinct(id, .keep_all = TRUE)      # Keep first occurrence per id

# Step 3: Reshape wide format to long format
df_wide_adults <- dfs_long_adults |>
  map(
    ~ .x |>
      # Pivot from wide to long: one row per selected adult type
      pivot_longer(
        cols = -c(id, starts_with("Adults_other")),  # All Adults_ columns except Adults_other
        names_to = "adult_type",       # Column names → "adult_type" column
        values_to = "selected"         # Cell values → "selected" column (1 or 0)
      ) |>
      
      # Keep only selected adults (where checkbox was checked)
      filter(selected == 1) |>
      
      # Clean up adult type names: "Adults_mother" → "mother"
      mutate(adult_type = str_remove(adult_type, "^Adults_"))
  ) |>
  
  # Combine all datasets
  bind_rows() |>
  
  # Rank adults by order (first selected = adult_1, second = adult_2, etc.)
  group_by(id) |>
  mutate(
    adult_rank = row_number(),         # 1, 2, 3, ...
    rank_name = paste0("adult_", adult_rank)  # "adult_1", "adult_2", "adult_3", ...
  ) |>
  ungroup() |>
  
  # Pivot back to wide format with ranked columns
  # Now: adult_1, adult_2, adult_3 instead of Adults_mother, Adults_father, etc.
  pivot_wider(
    id_cols = id,
    names_from = rank_name,            # adult_1, adult_2, adult_3
    values_from = adult_type           # "mother", "father", "brother"
  ) |>
  
  # Re-attach Adults_other column (open-ended responses)
  left_join(df_other_info, by = "id")

# Step 4: Replace Adults columns in original datasets with processed version
# Remove old wide-format Adults_ columns and merge in long-format versions
for (df_name in names(dfs)) {
  if (any(startsWith(names(dfs[[df_name]]), "Adults_"))) {
    # Remove all Adults_ columns
    dfs[[df_name]] <- dfs[[df_name]] |>
      select(-starts_with("Adults_"))
    
    # Merge in the processed long-format Adults data
    dfs[[df_name]] <- dfs[[df_name]] |>
      left_join(df_wide_adults, by = "id")
  }
}

# ============================================================================
# SCALE-DATASET ASSOCIATIONS (REFERENCE)
# ============================================================================
# Documents which scales appear in which datasets
# Used for understanding data structure, not for processing
# ============================================================================
scales_dataset_association <- list(
  "FTOS_pilot" = c("br_pilot"),                # Brazil pilot only
  "LPS_pilot" = c("br_pilot"),                 # Brazil pilot only
  "FTOS_v1" = c(""),                           # First-stage (most datasets)
  "LPS_v1" = c(""),                            # First-stage (most datasets)
  "GRIT" = c("US"),                            # US only
  "DASS" = c("SL"),                            # Slovenia only
  "MF" = c("HI"),                              # Hindi/India only
  "CAMS" = c(),                                # (not in current datasets)
  "PiL" = c("TK"),                             # Turkey only
  "PANAS" = c("MZ", "ES"),                     # Mozambique, Spain
  "SWLS" = c("MZ", "ES", "IT_extra", "NL_extra"),  # Multiple countries
  "FTPQ" = c("NL"),                            # Netherlands only
  "IT" = c("IT (first stage)"),                # Italy first-stage
  "Prospera" = c("IT (first stage)"),          # Italy first-stage
  "DMF" = c("IT (first stage)"),               # Italy first-stage
  "Ep" = c("MX"),                              # Mexico only
  "ZTPI" = c("RU"),                            # Russia only
  "CFC" = c("RU"),                             # Russia only
  "Asrus" = c("RU"),                           # Russia only
  "IPIP" = c("ID"),                            # Indonesia only
  "FTPtr" = c("TK"),                           # Turkey only
  "Jung" = c("MY"),                            # Malaysia only
  "LOC" = c("MY"),                             # Malaysia only
  "SH" = c("HI"),                              # Hindi/India only
  "MH" = c("HI"),                              # Hindi/India only
  "RFA" = c("HI"),                             # Hindi/India only
  "FM" = c("HI"),                              # Hindi/India only
  "FSL" = c("HI"),                             # Hindi/India only
  "IPS" = c("AR"),                             # Argentina only
  "DIDS" = c("SA"),                            # South Africa only
  "UMICS" = c("SA"),                           # South Africa only
  "SCCS" = c("SA"),                            # South Africa only
  "UPS" = c("SA")                              # South Africa only
)

# Define SPSS missing value codes
# These will be applied to all numeric columns when merging
# NOTE: SPSS only allows up to 3 user-missing values per variable
# We use: 990 (Variable not included), 991 (Technical/other), 993 (Did not respond)
all_na_values <- c(990, 991, 993)

# ============================================================================
# TYPE STANDARDIZATION: Fix Mixed Column Types
# ============================================================================
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
# ============================================================================

# Get all unique column names across all datasets
all_cols <- unique(unlist(lapply(dfs, names)))

# Define which columns should be treated as categorical (→ character)
# These columns contain categories/labels, not quantities
categorical_cols <- c(
  "id",                          # Participant identifier (e.g., "CH_277273_123")
  "Nationality",                 # Country (preserve label like "Brazil" not code 30)
  "Religion_christianother",     # Religious denomination (text)
  "Adults_brother",              # Adult type (after Y/blank fix)
  "Origin",                      # Origin/ethnicity (open-ended text)
  "Gender_other",                # Open-ended gender response
  "SexOrientation_other",        # Open-ended orientation response
  "Race_other",                  # Open-ended race response
  "Racems_other",                # Race other (Malaysia-specific)
  "Racenl_other",                # Race other (Netherlands-specific)
  "Occupation_other",            # Open-ended occupation response
  "occupation_other",            # Lowercase version of Occupation_other
  "Condition"                    # Experimental condition (text label)
)

# Scan each column and standardize types if mixed
for (col_name in all_cols) {
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

# ============================================================================
# LEGACY TYPE CONVERSION (Specific Cases)
# ============================================================================
# Original conversion loop kept for any additional edge cases
# Redundant with above loop for most columns, but kept for safety
# ============================================================================
for (df_name in names(dfs)) {
  df <- dfs[[df_name]]
  
  # Convert character columns to numeric if they should be numeric
  char_to_num_cols <- c("Nationality", "Religion_christianother", "Adults_brother")
  
  for (col in char_to_num_cols) {
    if (col %in% names(df)) {
      if (is.character(df[[col]]) || (is.labelled(df[[col]]) && is.character(as.vector(df[[col]])))) {
        # Remove labels first, then convert character to numeric
        df[[col]] <- as.numeric(as.character(zap_labels(df[[col]])))
      }
    }
  }
  
  # Save changes back to list
  dfs[[df_name]] <- df
}

# ============================================================================
# LABEL EXISTING MISSING VALUES
# ============================================================================
# PROBLEM: Datasets may have various missing value representations:
#          - R NAs
#          - -99, -999 (old SPSS convention)
#          - 999 (our convention)
#          But not all are properly labeled for SPSS export
#
# SOLUTION: Apply SPSS labelled_spss() to mark missing value codes:
#           990 = "Variable not included in this dataset"
#           991 = "Question not displayed to participant"
#           992 = "Participant chose not to answer"
#           999 = "R NA (true missing data)"
#
# WHY: SPSS recognizes these as user-missing values, handles them correctly
#      in analyses (exclusion from calculations, listwise/pairwise deletion)
#
# MECHANISM:
#   1. Temporarily replace R NAs with 999
#   2. Apply labelled_spss with all reason_codes
#   3. Mark codes 990-999 as SPSS user-missing values
# ============================================================================

for (df_name in names(dfs)) {
  df <- dfs[[df_name]]

  # Apply labelled_spss to all numeric columns
  df_labeled <- df %>%
    mutate(
      across(
        .cols = where(~ is.numeric(.x) || is.integer(.x)),  # Only numeric/integer columns
        .fns = ~ {
          # Skip non-numeric columns (character IDs, open-ended text, etc.)
          if (!is.numeric(.x) & !is.integer(.x)) {
            message(sprintf("Skipping non-numeric column: %s", cur_column()))
            return(.x)
          }

          # Step 1: Temporarily replace R NAs with 993 (nonresponse)
          # This ensures true R NAs get labeled (not left as unlabeled missing)
          column_data <- .x
          old_labels <- tryCatch(val_labels(column_data), error = function(e) NULL)
          column_data[is.na(column_data)] <- 993

          # Step 2: Create limited label set (only unique labels)
          # Start with the 3 missing value codes
          limited_labels <- c(
            "by_design" = 990,
            "technical_other" = 991,
            "did_not_respond" = 993
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

          # Step 3: Apply SPSS labelled format with limited missing value codes
          # na_values = only 3 codes (SPSS limit)
          # labels = value-to-label mapping
          labelled_spss(
            x = column_data,
            na_values = all_na_values,           # Only 990, 991, 993 (3 codes max)
            labels = limited_labels              # Labels for present values + missing codes
          )
        }
      )
    )

  # Save labeled dataset back to list
  dfs[[df_name]] <- df_labeled
}

# ============================================================================
# STANDARDIZE COLUMN NAMES (Fix SPSS Case-Insensitive Duplicates)
# ============================================================================
# PROBLEM: SPSS treats column names as case-insensitive
#          "Occupation_student" and "occupation_student" are duplicates in SPSS
#          "Name" and "name" are also duplicates
#
# SOLUTION: Standardize all column names to lowercase
#           Handle special cases where different columns have same lowercase name
# ============================================================================

for (df_name in names(dfs)) {
  df <- dfs[[df_name]]
  
  # Get original column names
  original_names <- names(df)
  
  # Convert to lowercase
  new_names <- tolower(original_names)
  
  # Check for duplicates after lowercasing
  if (any(duplicated(new_names))) {
    dupes <- new_names[duplicated(new_names)]
    message(sprintf("Dataset '%s' has duplicate column names after lowercasing: %s", 
                    df_name, paste(unique(dupes), collapse=", ")))
    
    # Handle Name/name conflict: keep lowercase 'name', rename uppercase 'Name' to 'participant_name'
    for (i in seq_along(original_names)) {
      if (original_names[i] == "Name" && "name" %in% original_names) {
        new_names[i] <- "participant_name"
      }
    }
    
    # If still duplicates, add suffix
    if (any(duplicated(new_names))) {
      new_names <- make.unique(new_names, sep = "_")
    }
  }
  
  # Apply new names
  names(df) <- new_names
  
  # Convert occupation_other to character to ensure consistency
  if ("occupation_other" %in% names(df)) {
    df$occupation_other <- as.character(df$occupation_other)
  }
  
  # Save back to list
  dfs[[df_name]] <- df
}

# Update all_cols with lowercase names
all_cols <- unique(unlist(lapply(dfs, names)))

# Update categorical_cols to lowercase
categorical_cols <- tolower(categorical_cols)

# ============================================================================
# DEFINE FUNCTION: label_merge_NAs()
# ============================================================================
# PURPOSE: Label padding NAs created by bind_rows() merge
#
# PROBLEM: When merging datasets with bind_rows(), columns not present in
#          all datasets get filled with NAs (padding)
#
# EXAMPLE: Dataset A has GRIT scale, Dataset B doesn't
#          After merge: Dataset B rows have NA for all GRIT columns
#          These NAs mean "scale not included", not "participant didn't answer"
#
# SOLUTION: Replace padding NAs with code 990 ("Variable not included")
#
# CRITICAL CHALLENGE: Haven package overrides is.na() for labelled vectors
#                     is.na(999) returns TRUE even though 999 is a numeric value
#                     This would incorrectly replace 999 codes with 990
#
# FIX: Temporarily remove labelled class with unclass()
#      This restores normal R is.na() behavior
#      Only true R NAs get replaced with 990
# ============================================================================
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
            "technical_other" = 991,
            "did_not_respond" = 993
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

# ============================================================================
# MERGE ALL DATASETS AND LABEL PADDING NAs
# ============================================================================
# Final step: Combine all datasets and handle merge-created NAs
# ============================================================================
merged_df <- label_merge_NAs(bind_rows(dfs))

# ============================================================================
# SAVE COMBINED DATASET
# ============================================================================
# Write the combined dataset to SPSS (.sav file), CSV and EXCEL formats under merged directory
# ============================================================================
output_file_name <- file.path(DIR_CLEAN, "merged", "merged_dataset")
write_sav(merged_df, paste0(output_file_name, ".sav"))
write_csv(merged_df, paste0(output_file_name, ".csv"))
write_xlsx(merged_df, paste0(output_file_name, ".xlsx"))