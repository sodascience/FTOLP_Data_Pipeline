library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(readxl)
library(lubridate)

setwd("~/surfdrive/Narrating the Future (Bogdan)")
source("Bogdan R/merge_helper.R")

# load files
setwd("~/surfdrive/Narrating the Future (Bogdan)/LimeSurvey Processed/clean")

file_list <- list.files(
  path = "./",
  pattern = "\\.sav$",
  full.names = TRUE,
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

          # 3. Use labelled_spss to mark ALL codes as user-missing
          labelled_spss(
            x = column_data,
            na_values = all_na_values,
            labels = c(old_labels, reason_codes)
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

          # CRITICAL: Temporarily remove the labelled class to disable the haven is.na() method.
          # This allows us to isolate true R NAs (padding) from numeric codes (999).
          unclassed_data <- unclass(column_data)

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
