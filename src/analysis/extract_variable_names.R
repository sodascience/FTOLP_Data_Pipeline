# ============================================================================
# extract_variable_names.R - Extract column names from SPSS files
# ============================================================================
# PURPOSE: Read all SPSS files from LimeSurvey Processed directory, extract
#          column names from each file, and save them as separate sheets in
#          a single Excel file
#
# INPUT:  SPSS .sav files from DIR_SPLIT
# OUTPUT: Excel file with column names for each dataset (one sheet per file)
# ============================================================================

library(tidyverse)
library(haven)
library(writexl)
library(here)

# Load configuration
source(here::here("config", "paths.R"))

# ============================================================================
# GET LIST OF SPSS FILES
# ============================================================================

# Get all SPSS files from the processed directory
file_list <- list.files(
  path = DIR_SPLIT,
  pattern = "\\.sav$",
  full.names = TRUE
)

cat("Found", length(file_list), "SPSS files\n")

# ============================================================================
# EXTRACT COLUMN NAMES FROM EACH FILE
# ============================================================================

# Create a list to store dataframes (one per file)
# Each dataframe will have a "raw_variable" column with the variable names
variable_list <- list()

# Process each file
for (file_path in file_list) {
  # Extract filename without extension
  filename <- tools::file_path_sans_ext(basename(file_path))
  
  cat("Processing:", filename, "...\n")
  
  # Read the SPSS file
  tryCatch({
    df <- read_sav(file_path)
    
    # Extract column names
    col_names <- names(df)
    
    # Create a dataframe with column names and additional empty columns
    var_df <- tibble(
      raw_variable_name = col_names,
      keep_in_merged_dataset = NA_character_,
      final_variable_name = NA_character_,
      categories_or_range = NA_character_,
      missing_value_codes = NA_character_,
      other_notes = NA_character_
    )
    
    # Store in list with filename as key
    variable_list[[filename]] <- var_df
    
    cat("  - Extracted", length(col_names), "variables\n")
    
  }, error = function(e) {
    cat("  ERROR reading file:", conditionMessage(e), "\n")
  })
}

# ============================================================================
# SAVE TO EXCEL FILE
# ============================================================================

# Define output path
output_file <- file.path(DIR_SPLIT, "variable_names_by_dataset.xlsx")

cat("\nSaving to Excel file:", output_file, "\n")

# Write to Excel with each dataset as a separate sheet
write_xlsx(variable_list, output_file)

cat("✓ Complete! Created Excel file with", length(variable_list), "sheets\n")
cat("  Each sheet contains the column names for one dataset\n")
