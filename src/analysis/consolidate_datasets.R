library(haven)
library(ggplot2)
library(dplyr)

# Load configuration
source(here::here("config", "paths.R"))

# Set working directory to processed data
setwd(DIR_PROCESSED)

file_list <- list.files(
  path = "./",
  pattern = "\\.sav$",
  full.names = TRUE
)

all_columns <- character()
dataset_columns <- list()
dataset_all_na <- list()
col_types_list <- list()

# helper that returns a compact, haven-aware type tag
type_tag <- function(x) {
  cls <- class(x)
  if (inherits(x, "labelled_spss") || inherits(x, "labelled")) {
    base <- if (is.integer(x)) "int" else if (is.double(x)) "dbl" else typeof(x)
    lbl <- if (inherits(x, "labelled_spss")) "lbl_spss" else "lbl"
    return(paste0(base, "(", lbl, ")"))
  }
  if ("POSIXct" %in% cls) {
    return("dttm")
  }
  if ("Date" %in% cls) {
    return("date")
  }
  if ("hms" %in% cls) {
    return("hms")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.character(x)) {
    return("chr")
  }
  if (is.integer(x)) {
    return("int")
  }
  if (is.double(x)) {
    return("dbl")
  }
  if (is.logical(x)) {
    return("lgl")
  }
  typeof(x)
}

for (file_path in file_list) {
  tryCatch(
    {
      data <- read_sav(file_path)
      cols <- names(data)
      dataset_columns[[file_path]] <- cols
      all_columns <- union(all_columns, cols)

      # all-NA mask
      na_mask <- vapply(data, function(x) all(is.na(x)), logical(1))
      dataset_all_na[[file_path]] <- names(na_mask)[na_mask]

      # types
      col_types_list[[file_path]] <- vapply(data, type_tag, character(1))
    },
    error = function(e) {
      message(paste("Skipping file due to error:", file_path, e))
      dataset_columns[[file_path]] <- NULL
      dataset_all_na[[file_path]] <- NULL
      col_types_list[[file_path]] <- NULL
    }
  )
}

dataset_names <- gsub("^\\./|\\.sav$", "", names(dataset_columns))

# existence table
overlap_table <- data.frame(
  matrix(FALSE, nrow = length(all_columns), ncol = length(dataset_names)),
  row.names = all_columns, check.names = FALSE
)
colnames(overlap_table) <- dataset_names

# all-NA table
all_na_table <- data.frame(
  matrix(FALSE, nrow = length(all_columns), ncol = length(dataset_names)),
  row.names = all_columns, check.names = FALSE
)
colnames(all_na_table) <- dataset_names

# types table
types_table <- data.frame(
  matrix(NA_character_, nrow = length(all_columns), ncol = length(dataset_names)),
  row.names = all_columns, check.names = FALSE
)
colnames(types_table) <- dataset_names

# fill all three
for (i in seq_along(dataset_columns)) {
  dataset_name <- dataset_names[i]
  cols_in_dataset <- dataset_columns[[i]]

  if (!is.null(cols_in_dataset)) {
    overlap_table[cols_in_dataset, dataset_name] <- TRUE
  }

  na_cols_in_dataset <- dataset_all_na[[i]]
  if (!is.null(na_cols_in_dataset) && length(na_cols_in_dataset) > 0) {
    present_and_all_na <- intersect(na_cols_in_dataset, cols_in_dataset)
    if (length(present_and_all_na) > 0) {
      all_na_table[present_and_all_na, dataset_name] <- TRUE
    }
  }

  types_in_dataset <- col_types_list[[i]]
  if (!is.null(types_in_dataset) && length(types_in_dataset) > 0) {
    # only fill rows that are in this dataset
    common <- intersect(names(types_in_dataset), all_columns)
    types_table[common, dataset_name] <- types_in_dataset[common]
  }
}

# render with ✓ / ✓✓ plus type tag
overlap_table_display <- as.data.frame(
  Map(function(exists_col, allna_col, type_col) {
    out <- ifelse(exists_col, paste0("✓ ", type_col), "")
    out[exists_col & allna_col] <- paste0("✓✓ ", type_col[exists_col & allna_col])
    out
  }, overlap_table, all_na_table, types_table),
  check.names = FALSE
)
rownames(overlap_table_display) <- all_columns

write.csv(overlap_table_display, "dataset_column_overlap.csv", row.names = TRUE)
