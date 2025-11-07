library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(readxl)
library(lubridate)
library(here)

# Load configuration
source(here::here("config", "paths.R"))
source(here::here("src", "utils", "merge_functions.R"))

# Load files from clean directory
file_list <- list.files(
  path = DIR_CLEAN,
  pattern = "\\.sav$",
  full.names = TRUE
)

dfs <- lapply(file_list, read_sav)

names(dfs) <- gsub("\\.sav$", "", basename(file_list))

# fix adults
dfs_long_adults <- dfs |>
  keep(~ any(startsWith(names(.x), "Adults_"))) |>
  map(~ .x |>
    select(id, starts_with("Adults_")) |>
    mutate(
      Adults_brother = case_when(Adults_brother == "Y" ~ 1, Adults_brother == "" ~ 0, TRUE ~ NA_real_),
    ))

df_other_info <- dfs_long_adults |>
  map(~ .x |>
    select(id, Adults_other) |>
    distinct()) |>
  bind_rows() |>
  distinct(id, .keep_all = TRUE)

df_wide_adults <- dfs_long_adults |>
  map(
    ~ .x |>
      pivot_longer(
        cols = -c(id, Adults_other),
        names_to = "adult_type",
        values_to = "selected"
      ) |>
      filter(selected == 1) |>
      mutate(adult_type = str_remove(adult_type, "^Adults_"))
  ) |>
  bind_rows() |>
  group_by(id) |>
  mutate(
    adult_rank = row_number(),
    rank_name = paste0("adult_", adult_rank)
  ) |>
  ungroup() |>
  pivot_wider(
    id_cols = id,
    names_from = rank_name,
    values_from = adult_type
  ) |>
  left_join(df_other_info, by = "id")

# scales
scales_dataset_association <- list(
  "FTOS_pilot" = c("br_pilot"),
  "LPS_pilot" = c("br_pilot"),
  "FTOS_v1" = c(""),
  "LPS_v1" = c(""),
  "GRIT" = c("US"),
  "DASS" = c("SL"),
  # "Nat" = c(), # unclear
  "MF" = c("HI"),
  "CAMS" = c(),
  # CAMS is not in the table
  "PiL" = c("TK"),
  # Other entries you might want to add from the table:
  "PANAS" = c("MZ", "ES"),
  "SWLS" = c("MZ", "ES", "IT_extra", "NL_extra"),
  "FTPQ" = c("NL"),
  "IT" = c("IT (first stage)"),
  "Prospera" = c("IT (first stage)"),
  "DMF" = c("IT (first stage)"),
  "Ep" = c("MX"),
  "ZTPI" = c("RU"),
  "CFC" = c("RU"),
  "Asrus" = c("RU"),
  "IPIP" = c("ID"),
  "FTPtr" = c("TK"),
  "Jung" = c("MY"),
  "LOC" = c("MY"),
  "SH" = c("HI"),
  "MH" = c("HI"),
  "RFA" = c("HI"),
  "FM" = c("HI"),
  # Already used for MF, but appears again in the SH, MH, RFA, FM, FSL group
  "FSL" = c("HI"),
  "IPS" = c("AR"),
  "DIDS" = c("SA"),
  "UMICS" = c("SA"),
  "SCCS" = c("SA"),
  "UPS" = c("SA")
)
all_na_values <- unname(reason_codes)

# Standardize column types before processing
# First, identify all columns across all datasets and their types
all_cols <- unique(unlist(lapply(dfs, names)))

# Define categorical columns that should be converted to character (not numeric)
categorical_cols <- c("id", "Nationality", "Religion_christianother", "Adults_brother", 
                      "Origin", "Gender_other", "SexOrientation_other", "Race_other",
                      "Racems_other", "Racenl_other", "Occupation_other", "Condition")

for (col_name in all_cols) {
  # Get the types of this column across all datasets that have it
  col_types <- sapply(dfs, function(df) {
    if (col_name %in% names(df)) {
      if (is.labelled(df[[col_name]])) {
        class(as.vector(df[[col_name]]))[1]
      } else {
        class(df[[col_name]])[1]
      }
    } else {
      NA_character_
    }
  })
  
  col_types <- col_types[!is.na(col_types)]
  
  # If there are mixed types (character and numeric), standardize them
  if (length(unique(col_types)) > 1) {
    # Categorical columns -> character, others -> numeric
    target_type <- if (col_name %in% categorical_cols) "character" else "numeric"
    
    message(sprintf("Standardizing column '%s' to %s (found types: %s)", 
                    col_name, target_type, paste(unique(col_types), collapse=", ")))
    
    for (df_name in names(dfs)) {
      if (col_name %in% names(dfs[[df_name]])) {
        if (target_type == "character") {
          # For character target: preserve labels if they exist, convert to factor then character
          if (is.labelled(dfs[[df_name]][[col_name]])) {
            dfs[[df_name]][[col_name]] <- as.character(as_factor(dfs[[df_name]][[col_name]]))
          } else {
            dfs[[df_name]][[col_name]] <- as.character(dfs[[df_name]][[col_name]])
          }
        } else {
          # For numeric target: strip labels and convert to numeric
          # Suppress expected "NAs introduced by coercion" warnings
          dfs[[df_name]][[col_name]] <- suppressWarnings(
            as.numeric(as.character(zap_labels(dfs[[df_name]][[col_name]])))
          )
        }
      }
    }
  }
}

# Original type conversion loop (kept for any additional specific cases)
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
  
  dfs[[df_name]] <- df
}

for (df_name in names(dfs)) {
  df <- dfs[[df_name]]

  df_labeled <- df %>%
    mutate(
      across(
        .cols = everything(),
        .fns = ~ {
          # 1. Skip if the column is entirely non-numeric (e.g., character ID columns)
          if (!is.numeric(.x) & !is.integer(.x)) {
            message(sprintf("Skipping non-numeric column: %s", cur_column()))
            return(.x)
          }

          # 2. Temporarily replace existing NAs with the specific numeric code
          column_data <- .x
          old_labels <- tryCatch(val_labels(column_data), error = function(e) NULL)
          column_data[is.na(column_data)] <- 999

          # 3. Merge labels, removing duplicates (keep reason_codes for duplicates)
          all_labels <- c(old_labels, reason_codes)
          all_labels <- all_labels[!duplicated(all_labels)]

          # 4. Use labelled_spss to mark ALL codes as user-missing
          labelled_spss(
            x = column_data,
            na_values = all_na_values,
            labels = all_labels
          )
        }
      )
    )

  dfs[[df_name]] <- df_labeled
}

label_merge_NAs <- function(df, code_to_assign = 990) {
  assign_label <- names(reason_codes)[reason_codes == code_to_assign]

  df_labeled <- df %>%
    mutate(
      across(
        .cols = where(is.numeric) | where(is.integer) | where(is.labelled),
        .fns = ~ {
          column_data <- .x
          
          # Skip if column is actually character (shouldn't happen but safety check)
          if (is.character(column_data)) {
            return(column_data)
          }

          # CRITICAL: Temporarily remove the labelled class to disable the haven is.na() method.
          # This allows us to isolate true R NAs (padding) from numeric codes (999).
          unclassed_data <- unclass(column_data)
          
          # Another safety check: ensure unclassed_data is numeric
          if (!is.numeric(unclassed_data)) {
            return(column_data)
          }

          # Use if_else to ONLY replace true NAs with the 990 code.
          column_data_replaced <- if_else(
            is.na(unclassed_data),
            as.numeric(code_to_assign),
            unclassed_data
          )

          # Re-apply the labelled_spss class and metadata
          labelled_spss(
            x = column_data_replaced,
            na_values = all_na_values,
            labels = reason_codes
          )
        }
      )
    )

  # message(sprintf("Successfully assigned new NAs as '%s' (%d) while preserving existing codes.",
  #                assign_label, code_to_assign))

  return(df_labeled)
}

combined_df_raw <- label_merge_NAs(bind_rows(dfs))
