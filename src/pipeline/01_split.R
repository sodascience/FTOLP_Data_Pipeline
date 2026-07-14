# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SCRIPT: 01_split.R -
# PURPOSE: Load raw LimeSurvey data, preprocess, split by country/language, 
#          and output to processed directory
# 
# INPUT:  Raw .sav files from DIR_RAW and DIR_EXTERNAL
# OUTPUT: Processed .sav files in DIR_SPLIT (split by country/dataset)
# 
# WORKFLOW:
#   1. Load raw survey data from LimeSurvey exports
#   2. Normalize column names across different survey versions
#   3. Split multi-country surveys by language/nationality
#   4. Add unique ID prefixes to track data source
#   5. Filter out test participants and incomplete responses
#   6. Save processed files for cleaning pipeline
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load required packages ----
library(tidyverse)   # Data manipulation and visualization
library(haven)       # Read/write SPSS .sav files
library(purrr)       # Functional programming tools
library(dplyr)       # Data manipulation
library(labelled)    # Handle labelled data from SPSS
library(readxl)      # Read Excel files
library(lubridate)   # Date/time manipulation
library(here)        # Project-relative paths

# Load configuration file ----
# Load configuration file containing directory paths (DIR_RAW, DIR_SPLIT)
source(here::here("config", "paths.R"))

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# HELPER FUNCTIONS ----
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Read SPSS file from main raw data directory
# Used for files directly exported from LimeSurvey
read_raw <- function(filename) {
  read_sav(file.path(DIR_RAW, filename))
}

# Write dataframe to processed directory
# Automatically removes columns that are entirely NA before saving
# Creates country-specific subdirectory if it doesn't exist
write_processed <- function(df, filename) {
  df <- df %>% select(where(~ !all(is.na(.))))  # Drop all-NA columns
  
  # Extract country code from filename (first part before underscore)
  # Examples: CN_277273.sav -> CN, IN_EN_824323.sav -> IN, BR_PILOT.sav -> BR
  base_name <- str_remove(filename, "\\.sav$")
  country_code <- str_to_upper(str_split(base_name, "_")[[1]][1])
  
  # Create country-specific directory if it doesn't exist
  country_dir <- file.path(DIR_SPLIT, country_code)
  if (!dir.exists(country_dir)) {
    dir.create(country_dir, recursive = TRUE)
    message(sprintf("Created directory: %s", country_dir))
  }
  
  # Save to country-specific directory
  output_path <- file.path(country_dir, filename)
  write_sav(df, output_path)
  message(sprintf("Saved: %s", output_path))
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Normalize and standardize column names across different survey versions
# This ensures consistency when merging datasets from different countries/languages
# 
# WHAT IT DOES:
#   - Removes redundant double prefixes (e.g., "IT_IT1" -> "IT_1", "CAAS_CAAS5" -> "CAAS_5")
#   - Standardizes scale abbreviations (e.g., "MiLQ" -> "MLQ", "DASS21_DASS" -> "DASS")
#   - Capitalizes demographic variable names (age -> Age, gender -> Gender)
#   - Standardizes control item names (uppercase X -> lowercase x)
#   - Filters out test participants (where Ref or Name exactly equals "test", case-insensitive)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
normalize_column_names <- function(df) {
  df %>%
    # Remove redundant double prefixes: "PREFIX_PREFIX##" -> "PREFIX_##"
    # Example: IT_IT1 -> IT_1, CAAS_CAAS5 -> CAAS_5
    rename_with(~ str_replace(.x, "^([A-Za-z]+)_\\1(\\d+)$", "\\1_\\2")) %>%
    
    # Standardize CAAS columns: "CAAS_SQ00#" -> "CAAS_#"
    rename_with(~ str_replace(.x, "^CAAS_SQ00(\\d+)$", "CAAS_\\1")) %>%
    
    # Standardize Prospera: "Prospera_Pr#" -> "Prospera_#"
    rename_with(~ str_replace(.x, "^Prospera_Pr(\\d+)$", "Prospera_\\1")) %>%
    
    # Standardize DASS21 scale: "DASS21_DASS#" -> "DASS_#"
    rename_with(~ str_replace(.x, "^DASS21_DASS(\\d+)$", "DASS_\\1")) %>%
    
    # Standardize ESS: "ESS_ES#" -> "ES_#"
    rename_with(~ str_replace(.x, "^ESS_ES(\\d+)$", "ES_\\1")) %>%
    
    # Standardize MiLQ (Meaning in Life Questionnaire): "MiLQ_#" -> "MLQ_#"
    rename_with(~ str_replace(.x, "^MiLQ_(\\d+)$", "MLQ_\\1")) %>%
    
    # Standardize EASY scale: "EASY_EASY##_##" -> "EASY_##_##"
    rename_with(~ str_replace(.x, "^EASY_EASY(\\d+)#(\\d+)$", "EASY_\\1#\\2")) %>%
    
    # Capitalize first letter of demographic columns (age -> Age, gender -> Gender, etc.)
    rename_with(~ sub("^(.)", "\\U\\1", ., perl = TRUE), .cols = any_of(c(
      "age", "nationality", "gender", "education", "origin"
    ))) %>%
    
    # Standardize Life Satisfaction scale: "LS_BRS#" -> "BRS_#"
    rename_with(~ str_replace(.x, "^LS_BRS(\\d+)$", "BRS_\\1")) %>%
    
    # Standardize control/attention check items: uppercase X -> lowercase x
    rename_with(~ str_replace(.x, "^CAAS_X$", "CAAS_x")) %>%
    rename_with(~ str_replace(.x, "^FTOS_X$", "FTOS_x")) %>%
    rename_with(~ str_replace(.x, "^LPS_X$", "LPS_x")) %>%
    
    # Standardize date column: "StartDate" -> "startdate"
    rename_with(~ str_replace(.x, "^StartDate$", "startdate")) %>%
    {
      result <- .
      # Filter out test participants based on Ref or Name columns
      # Test participants have Ref or Name that exactly equals "test" (case-insensitive)
      if ("Ref" %in% names(result)) {
        result <- result %>% filter(is.na(Ref) | !str_detect(tolower(Ref), "^test$"))
      }
      if ("Name" %in% names(result)) {
        result <- result %>% filter(is.na(Name) | !str_detect(tolower(Name), "^test$"))
      }
      result
    } %>%

    # Fix LPS goals column naming
    rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
    rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1"))
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# Rename LoTeDGI scale items for Brazil pilot dataset
# The LoTeDGI scale combines two separate scales that need to be split:
#   - DGI (Dispositional Goal Instrumentality): Odd items (1,3,5,7,9,11,13)
#   - LOT (Life Orientation Test): Even items (2,4,6,8,10,12)
# 
# INPUT: Items named LoTeDGI_SQ001 through LoTeDGI_SQ013
# OUTPUT: Renamed to Psy_DGI1-7 (odd) and Psy_LOT1-6 (even)
rename_scales_brazil_pilot <- function(df) {
  # Find all LoTeDGI columns (format: LoTeDGI_SQ###)
  cols <- grep("^LoTeDGI_SQ\\d{3}$", names(df), value = TRUE)
  if (!length(cols)) {
    return(df)  # No LoTeDGI columns found, return unchanged
  }

  # Extract numeric index (1-13) from column names
  idx <- as.integer(str_remove(cols, "^LoTeDGI_SQ0*"))

  # Determine new names based on odd/even pattern
  new_names <- ifelse(
    idx %% 2 == 1,
    # Odd items (1,3,5,7,9,11,13) -> DGI scale (1-7)
    paste0("Psy_DGI", (idx + 1) %/% 2),
    # Even items (2,4,6,8,10,12) -> LOT scale (1-6)
    paste0("Psy_LOT", idx %/% 2)
  )

  # Build rename map: new = old (only for valid indices 1..13)
  map <- setNames(cols, new_names)

  df %>% rename(!!!map)
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# brazil_pilot_fix_factors: Fix Factor Encoding Issue in Brazil Pilot Data
# PURPOSE: Corrects improperly encoded factor values in Brazil pilot dataset
#
# PROBLEM: Brazil pilot data has values stored as "A1", "A2", "A3" etc instead
#          of numeric 1, 2, 3. This happens due to LimeSurvey export quirks.
#
# SOLUTION: Convert factor levels to numeric (1, 2, 3...) while preserving:
#           - The SPSS labelled structure (for later SPSS export)
#           - Variable labels (question text metadata)
#           - The order of response levels
#
# AFFECTED SCALES: MLQ, CAAS, Psy_LOT, Psy_DGI, AS (all Likert-type scales)
#
# INPUT:  df - Data frame with factor-encoded columns (e.g., "A1", "A2", "A3")
# OUTPUT: df - Data frame with numeric values (1, 2, 3) but same label structure
#
# EXAMPLE TRANSFORMATION:
#   Before: Psy_DGI1 = c("A1", "A2", "A3") with labels 1="Strongly Disagree", 2="Disagree"...
#   After:  Psy_DGI1 = c(1, 2, 3) with labels 1="A1", 2="A2", 3="A3"
#           (Note: Text labels change to reflect factor levels, but numeric order preserved)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
brazil_pilot_fix_factors <- function(df) {
  # Find all columns matching scale names (MLQ, CAAS, LOT, DGI, AS)
  # value=TRUE returns column names (not indices)
  cols <- grep("MLQ|CAAS|Psy_LOT|Psy_DGI|MLQ|AS", names(df), value = TRUE)
  
  # Safety check: If no matching columns, return unchanged
  if (!length(cols)) {
    return(df)
  }

  result <- df %>%
    mutate(
      across(
        # Apply transformation to all scale columns
        .cols = all_of(cols),
        .fns = ~ {
          # Only process if column is labelled SPSS format
          if (is.labelled(.x)) {
            # STEP 1: Store metadata before transformation
            # current_labels = value-to-label mapping (e.g., 1="Strongly Disagree")
            current_labels <- val_labels(.x)
            # var_lbl = Question text (e.g., "I feel a sense of purpose")
            var_lbl <- var_label(.x)

            # STEP 2: Convert to factor to get unique levels
            # Example: c("A1", "A2", "A1", "A3") -> factor with levels("A1", "A2", "A3")
            x_chr <- as.character(.x)
            x_chr <- dplyr::na_if(x_chr, "")
            factor_vec <- factor(x_chr, levels = sort(unique(x_chr[!is.na(x_chr)])))

            # STEP 3: Convert factor to integers (1, 2, 3 based on level order)
            # This converts "A1"→1, "A2"→2, "A3"→3
            numeric_values <- as.integer(factor_vec)

            # STEP 4: Create new label mapping where numbers map to old factor levels
            # setNames(values, names) creates named vector
            # seq_along(levels) creates sequence 1,2,3... for each level
            # Result: c("1"="A1", "2"="A2", "3"="A3")
            new_labels_map <- setNames(
              seq_along(levels(factor_vec)),
              levels(factor_vec)
            )
            
            # STEP 5: Recreate labelled vector with numeric values but preserved structure
            # x = actual data values (now numeric 1,2,3 instead of "A1","A2","A3")
            # labels = new mapping (1="A1", 2="A2", etc)
            # label = preserve original question text
            new_labelled_vec <- labelled(
              x = numeric_values,
              labels = new_labels_map,
              label = var_lbl
            )

            return(new_labelled_vec)
          } else {
            # Skip columns that aren't labelled (shouldn't happen, but safety check)
            message(sprintf("Skipping non-labelled column: %s", cur_column()))
            return(.x)
          }
        }
      )
    )

  result
}

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MOZAMBIQUE ----
# SOURCE: Three separate second-stage survey batches from Mozambique
#         - 276893.sav: Online administration
#         - 429283.sav: Printed (paper) administration
#         - 827663.sav: Additional online batch
#
# SURVEY STAGE: All are "second stage" (shorter follow-up questionnaire)
# SCALE VERSIONS: FTOS_v2, LPS_v2 (second-stage scales)
#
# PROCESSING STEPS:
# 1. Load each batch separately
# 2. Create unique IDs with batch identifier prefix (MZ_BATCHNUMBER_originalID)
# 3. Mark printed vs online administration method
# 4. Combine all three batches
# 5. Standardize scale column names (FTOS_v2_1, LPS_v2_1, etc.)
# 6. Normalize column names (remove special chars, filter test participants)
# 7. Remove incomplete responses (lastpage = -1 means never submitted)
# 8. Write combined output: MZ.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load first batch (online administration)
df_mz_online <- read_raw("276893.sav") %>%
  mutate(id = str_c("MZ_276893_", id))  # Prefix: MZ_276893_1, MZ_276893_2, etc.

# Load second batch (printed/paper administration)
df_mz_printed <- read_raw("429283.sav") %>%
  mutate(id = str_c("MZ_429283_", id))  # Prefix: MZ_429283_1, MZ_429283_2, etc.

# Load third batch (additional online)
df_mz_3 <- read_raw("827663.sav") %>%
  mutate(id = str_c("MZ_827663_", id))  # Prefix: MZ_827663_1, MZ_827663_2, etc.

# Tag administration method (0=online, 1=printed)
df_mz_online$printed <- 0
df_mz_printed$printed <- 1
df_mz_3$printed <- 0

# Combine all three batches and process
df_mz <- bind_rows(df_mz_online, df_mz_printed, df_mz_3) %>%
  # Rename FTOS columns: "FTOS_1" -> "FTOS_v2_1" (mark as version 2)
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  
  # Rename LPS columns: "LPS_1" -> "LPS_v2_1" (mark as version 2)
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  
  # Apply standard column name normalization and remove test participants
  normalize_column_names() %>%
  
  # Remove incomplete responses (lastpage=-1 means survey never finished/submitted)
  filter(lastpage != -1)

# Write combined Mozambique dataset
write_processed(df_mz, "MZ.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MAIN SURVEY 1 ----
# MAIN SURVEY 1 (277273.sav) - MULTI-COUNTRY/LANGUAGE SPLITTING
# SOURCE: 277273.sav (large multi-language first-stage survey)
#         Contains participants from: China, Slovenia, Italy, Spain, English-speaking,
#         Portuguese (both European and Brazilian)
#
# SURVEY STAGE: First stage (full questionnaire)
# SCALE VERSIONS: FTOS_v1, LPS_v1 (first-stage scales)
#
# CHALLENGE: Single survey file contains mixed languages/countries due to:
#            - Participants selecting wrong language (e.g., selected Portuguese but answered in Chinese)
#            - Multiple language variants (Chinese simplified/traditional, PT vs PT-BR)
#
# SOLUTION: Split into 5 separate datasets based on language detection and nationality:
#           1. CN (China) - Chinese language/nationality/specific survey items
#           2. SI (Slovenia) - Slovenian language
#           3. IT (Italy) - Italian language
#           4. ES (Spain) - Spanish language
#           5. BR_PT (Brazil) - Portuguese, excluding all above + duplicate removal
#           English-speaking participants are not tied to a single country and
#           are excluded from BR_PT but not written to their own file.
#
# PROCESSING STRATEGY:
# 1. Load and standardize column names for first-stage scales
# 2. Define China-specific detection criteria (language, surveys, Unicode, nationality)
# 3. Extract IDs for each language group
# 4. Split data by language with additional quality filters
# 5. Handle Portuguese/Brazilian special cases (duplicates, conditions)
# 6. Output 5 separate country/language files
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load main survey 1 data
df_main1 <- read_raw("277273.sav")

# Standardize column names for first-stage scales
df_main1 <- df_main1 %>%
  # Rename FTOS columns: "FTOS_1" -> "FTOS_v1_1" (mark as version 1)
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v1_\\1")) %>%
  
  # Rename LPS columns: "LPS_1" -> "LPS_v1_1" (mark as version 1)
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v1_\\1")) %>%
  
  # Fix duplicate prefix: "FTOS_FTOS1" -> "FTOS_v1_1"
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  
  # Fix duplicate prefix: "LPS_LPS1" -> "LPS_v1_1"
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  
  # Apply standard normalization (column names + remove test participants)
  normalize_column_names() %>%
  
  # Convert ID to integer for filtering logic
  mutate(id = as.integer(id)) %>%
  
  # Remove incomplete responses
  filter(lastpage != -1)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# CHINA DETECTION: Identify Chinese participants using multiple criteria
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Define survey items that only appear in Chinese version
# These indicate participant saw Chinese questionnaire
china_surveys <- c(
  # Career Adapt-Abilities Scale (CAAS) items 7-25 (Chinese-specific range)
  paste0("CAAS_", 7:25),
  
  # Brief Resilience Scale (BRS) items 1-6
  paste0("BRS_", 1:6),
  
  # Existential Scale - Work (ESW_PS) items 1-22
  paste0("ESW_PS", 1:22),
  
  # Existential Scale (ES) items 1-14
  paste0("ES_", 1:14),
  
  # Job Search Scale (JSS) items 1-4
  paste0("JSS_", 1:4)
)

# Define Chinese language codes from LimeSurvey
china_languages <- c("zh-Hans", "zh-Hant-HK")  # Simplified & Traditional (Hong Kong)

# Define Chinese nationality codes: Macao=106, China=35, Hong Kong=79, Taiwan=162
china_nationalities <- c(106, 35, 79, 162)

# Extract IDs of Chinese participants using 5 criteria:
china_ids <- df_main1 %>%
  filter(
    # Criterion 1: Started survey in Chinese language
    startlanguage %in% china_languages |
    
    # Criterion 2: Name starts with "kc" (name pattern used by data collector)
    str_detect(coalesce(Name, ""), regex("^kc", ignore_case = TRUE)) |
    
    # Criterion 3: Has data in any China-specific survey items
    if_any(all_of(china_surveys), ~ !is.na(.)) |
    
    # Criterion 4: Contains Chinese Unicode characters (U+4E00 to U+9FFF range)
    # Checks all columns for any Chinese text
    if_any(everything(), ~ str_detect(
      coalesce(as.character(.), ""), "[\u4e00-\u9fff]"
    )) |
    
    # Criterion 5: Nationality is Chinese/Taiwanese/Hong Kong/Macao
    Nationality %in% china_nationalities
  ) %>%
  pull(id) %>%
  unique()

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# EXTRACT IDs FOR OTHER LANGUAGE GROUPS (based on startlanguage only)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Slovenia : Slovenian language
sl_ids <- df_main1 %>%
  filter(startlanguage == "sl") %>%
  pull(id)

# Italy: Italian language
it_ids <- df_main1 %>%
  filter(startlanguage == "it") %>%
  pull(id)

# Spain: Spanish language
es_ids <- df_main1 %>%
  filter(startlanguage == "es") %>%
  pull(id)

# English: English language (could be US, UK, India, etc.)
en_ids <- df_main1 %>%
  filter(startlanguage == "en") %>%
  pull(id)

# Combine all non-Portuguese IDs for later exclusion
non_br_pt_ids <- unique(c(china_ids, sl_ids, it_ids, es_ids, en_ids))

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# CREATE COUNTRY-SPECIFIC DATASETS WITH QUALITY FILTERS
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# CHINA dataset
df_main1_china <- df_main1 %>%
  filter(id %in% china_ids) %>%
  
  # Quality filter: Remove if GRIT items present (indicates wrong questionnaire version)
  filter(!if_any(GRIT_1:GRIT_10, ~ !is.na(.))) %>%
  
  # ID filter: Only include IDs < 10169 (specific batch cutoff)
  filter(id < 10169) %>%
  
  # Create new ID with country prefix
  mutate(id = str_c("CN_277273_", id))

# SLOVENIA dataset
df_sl <- df_main1 %>%
  filter(id %in% sl_ids) %>%

  # ID filter: Only include IDs < 10400 (specific batch cutoff)
  filter(id < 10400) %>%

  # Create new ID with country prefix
  mutate(id = str_c("SI_277273_", id))

# ITALY dataset (first data collection wave)
df_it <- df_main1 %>%
  filter(id %in% it_ids) %>%

  # Mark as first Italy data collection wave (vs later wave)
  mutate(id = str_c("IT_277273_", id), strategy = "data collection 1")

# SPAIN dataset
df_es <- df_main1 %>%
  filter(id %in% es_ids) %>%

  # Create new ID with country prefix
  mutate(id = str_c("ES_277273_", id))

# NOTE: English-language respondents (en_ids) are not tied to a single country
# (could be US, UK, India, etc.) and are not written to their own file — they
# are only used above to exclude them from the BR_PT dataset (non_br_pt_ids).

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PORTUGUESE/BRAZILIAN dataset - Complex handling with duplicate removal
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load list of duplicate IDs to exclude (stored in external file)
# IDs stored with offset, need to subtract 22200000 to match df_main1 IDs
df_br_pt_duplicate_ids <- read_sav(file.path(DIR_EXTERNAL, "duplicates_PT&BR.sav")) %>%
  mutate(id = id - 22200000) %>%
  pull(id)

# Load experimental condition assignments (for merging additional metadata)
df_br_pt_conditions <- read_sav(file.path(DIR_EXTERNAL, "conditions.sav"))

# PORTUGUESE/BRAZILIAN dataset (default: all remaining participants)
df_br_pt <- df_main1 %>%
  # Exclude anyone already assigned to other language/country + known duplicates
  filter(!(id %in% non_br_pt_ids | id %in% df_br_pt_duplicate_ids)) %>%
  
  # ID filter: Only include IDs < 9971 (specific batch cutoff)
  filter(id < 9971) %>%
  
  # Create new ID with BR_PT prefix
  mutate(id = str_c("BR_PT_277273_", id)) %>%
  
  # Merge experimental condition data (left join preserves all rows)
  left_join(df_br_pt_conditions %>% select(id, Condition), by = "id") %>%
  
  # Resolve condition conflicts: prioritize conditions.sav value if present
  # (Condition.y from conditions.sav, Condition.x from df_main1)
  mutate(Condition = if_else(!is.na(Condition.y) & Condition.y != "", Condition.y, Condition.x)) %>%
  
  # Remove temporary columns from join
  select(-Condition.x, -Condition.y)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# WRITE ALL 5 COUNTRY/LANGUAGE DATASETS
# (English respondents are not tied to a single country and are not written out)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
write_processed(df_main1_china, "CN_277273.sav")
write_processed(df_sl, "SI_277273.sav")
write_processed(df_it, "IT_277273.sav")
write_processed(df_es, "ES_277273.sav")
write_processed(df_br_pt, "BR_PT_277273.sav")


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# BRAZIL PILOT ----
# BRAZIL PILOT DATA (569687.sav) - Complex Merge with External Dataset
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SOURCE: Two data sources that need merging:
#         1. 569687.sav - Raw LimeSurvey export (pilot questionnaire responses)
#         2. "DataSet BR&PT.sav" - External file with FTOS/LPS scales + demographics
#
# SURVEY STAGE: Pilot study (testing questionnaire before main data collection)
# SCALE VERSIONS: FTOS_pilot, LPS_pilot (pilot versions of scales)
#
# CHALLENGE: Survey responses and scale data stored separately
#            Need to merge by ID and combine all relevant columns
#
# PROCESSING STEPS:
# 1. Load external dataset (DataSet BR&PT) and standardize scale names
# 2. Load pilot survey data and prepare for merge
# 3. Select relevant columns from external dataset (scales + demographics)
# 4. Merge datasets by ID
# 5. Rename scale columns to standard format (MLQ, AS)
# 6. Apply Brazil-specific transformations (scale splitting, factor fixing)
# 7. Filter incomplete responses and write output
#
# OUTPUT: BR_PILOT.sav (merged and processed pilot data)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# LOAD EXTERNAL DATASET: DataSet BR&PT (contains FTOS/LPS pilot scales)
df_dataset_brpt <- read_sav(file.path(DIR_EXTERNAL, "DataSet BR&PT.sav")) %>%
  # Standardize FTOS pilot column names: "FTOS_pilot1" -> "FTOS_pilot_1"
  rename_with(~ str_replace(.x, "^FTOS_pilot(\\d+)$", "FTOS_pilot_\\1")) %>%
  
  # Standardize LPS pilot column names: "LPS_pilot1" -> "LPS_pilot_1"
  rename_with(~ str_replace(.x, "^LPS_pilot(\\d+)$", "LPS_pilot_\\1"))

# LOAD PILOT SURVEY DATA (569687.sav)
df_brazil_pilot <- read_raw("569687.sav") %>%
  # Offset IDs to avoid conflicts with other datasets (add 11,100,000)
  mutate(id = id + 11100000) %>%
  
  # Remove unnecessary columns:
  # - QSD columns: Survey metadata not needed
  # - Cod columns: Temporary coding columns
  select(!starts_with("QSD") & !starts_with("Cod")) %>%
  
  # Rename email column to standard format (capital E)
  rename(Email = email)
  
  # NOTE: Alternative renaming approach (commented out):
  # Would rename FTOS_SQ001, FTOS_SQ01 columns to FTOS_pilot_ format
  # Not needed because external dataset already has proper names

# DEFINE SCALE COLUMNS TO EXTRACT FROM EXTERNAL DATASET
# These are the core scales stored in DataSet BR&PT
ftos_lps_cols <- c(
  paste0("FTOS_pilot_", 1:15),  # FTOS pilot items 1-15
  paste0("LPS_pilot_", 1:21)    # LPS pilot items 1-21
)

# SELECT RELEVANT COLUMNS FROM EXTERNAL DATASET
# Extract scales + demographic/background variables for merging
df_dataset_brpt <- df_dataset_brpt %>%
  select(
    # Merge key
    id,
    # Core pilot scales
    all_of(ftos_lps_cols),
    # Identity codes (anonymization)
    starts_with("IdCode_"),
    # Demographics
    Age,
    Sex,
    Gender,
    Gender_other,  # Open-ended gender response
    SexOrientation,
    SexOrientation_other,  # Open-ended orientation response
    starts_with("Race_"),  # Multiple race columns (check-all format)
    starts_with("Religion"),  # Religious affiliation
    Nationality,
    starts_with("Immigration"),  # Immigration status
    starts_with("Relationship"),  # Relationship status
    starts_with("Residence"),  # Residence information
    starts_with("Children"),  # Children information
    Education,
    
    # Household composition
    starts_with("Adults"),  # Number of adults in household
    starts_with("EducationAdult"),  # Education of other adults
    
    # Occupation/economic
    starts_with("Occupation"),
    starts_with("Family"),  # Family income
    FinancialDependency,
    
    # Mental health
    starts_with("Health"),
    starts_with("Psychiatric"),  # Psychiatric history
    starts_with("PsyDiagnosis"),  # Specific diagnoses
    starts_with("PsyTreatment")  # Treatment history
  )

# MERGE PILOT SURVEY WITH EXTERNAL DATASET
df_brazil_pilot_merged <- df_brazil_pilot %>%
  # Left join: Keep all pilot survey responses, add scales/demographics where available
  left_join(df_dataset_brpt, by = "id") %>%
  
  # Create new ID with Brazil pilot prefix
  mutate(id = str_c("BR_569687_", id)) %>%
  
  # Rename MLQ (Meaning in Life Questionnaire) columns:
  # "QSV_SQ001", "QSV_SQ01" -> "MLQ_1", "MLQ_2", etc.
  rename_with(
    .fn = ~ str_replace(., "QSV_SQ0{1,2}", "MLQ_"),
    .cols = starts_with("QSV_SQ00") | starts_with("QSV_SQ0")
  ) %>%
  
  # Rename AS (Authenticity Scale) columns:
  # "EA_SQ001", "EA_SQ01" -> "AS_1", "AS_2", etc.
  rename_with(
    .fn = ~ str_replace(., "EA_SQ0{1,2}", "AS_"),
    .cols = starts_with("EA_SQ00") | starts_with("EA_SQ0")
  ) %>%
  
  # Apply standard column name normalization and test participant removal
  normalize_column_names() %>%
  
  # Split LoTeDGI scale into DGI (odd items) and LOT (even items)
  rename_scales_brazil_pilot() %>%
  
  # Fix factor encoding issue (A1, A2, A3 -> 1, 2, 3)
  brazil_pilot_fix_factors() %>%
  
  # Remove incomplete responses
  filter(lastpage != -1)

# Write processed Brazil pilot dataset
write_processed(df_brazil_pilot_merged, "BR_PILOT.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# INDIA ----
# INDIA DATA (824323.sav)
# SOURCE: 824323.sav - English language survey in India
# SURVEY STAGE: Second stage (shorter follow-up questionnaire)
# SCALE VERSIONS: FTOS_v2, LPS_v2
# LANGUAGE: English
# OUTPUT: IN_EN_824323.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
df_india <- read_raw("824323.sav") %>%
  # Create ID with country/language prefix: IN_EN (English-India)
  mutate(id = str_c("IN_EN_824323_", id)) %>%
  
  # Standardize to version 2 scale names (second stage)
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  
  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_india, "IN_EN_824323.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ITALY ----
# ITALY DATA COLLECTION 3 (855796.sav)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SOURCE: 855796.sav - Italian language survey (third wave)
# SURVEY STAGE: Second stage (shorter follow-up questionnaire)
# SCALE VERSIONS: FTOS_v2, LPS_v2
# LANGUAGE: Italian
# NOTE: Third data collection wave for Italy (vs first wave in MAIN SURVEY 1,
#       vs second wave "IT_AUTO" in the EXTRA DATASETS / ITALY EXTRA section)
# OUTPUT: IT_855796.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
df_it3 <- read_raw("855796.sav") %>%
  # Create ID with country prefix, mark as data collection wave 3
  mutate(id = str_c("IT_855796_", id), strategy = "data collection 3") %>%
  
  # Standardize to version 2 scale names (second stage)
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  
  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_it3, "IT_855796.sav")


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# USA ----
# USA DATA COLLECTION 1 (868141.sav)
# SOURCE: 868141.sav - English language survey in USA
# SURVEY STAGE: First stage (full questionnaire)
# SCALE VERSIONS: FTOS_v1, LPS_v1
# LANGUAGE: English
# NOTE: Second wave of US data collection (vs Oregon = first wave)
# OUTPUT: US_868141.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
df_us1 <- read_raw("868141.sav") %>%
  # Create ID with country prefix, mark as data collection wave 2
  mutate(id = str_c("US_868141_", id), strategy = "data collection 2") %>%
  
  # Fix duplicate prefix in scale names: "FTOS_FTOS1" -> "FTOS_v1_1"
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  
  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_us1, "US_868141.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# MAIN SURVEY 2 ----
# MAIN SURVEY 2 (999625.sav) - Complex English/Hindi Splitting
# SOURCE: 999625.sav - Multi-language second-stage survey
# SURVEY STAGE: Second stage (shorter follow-up questionnaire)
# SCALE VERSIONS: FTOS_v2, LPS_v2
# LANGUAGES: English (en), Hindi (hi), plus multiple other languages
#
# CHALLENGE: Need to properly separate English vs Indian participants
#            Some participants selected wrong language or have ambiguous nationality
#
# SOLUTION: Complex 5-step splitting logic based on Caste field and Origin:
#   1. Split by initial language (en vs hi)
#   2. Move EN participants with "Caste" data to IN (Indian-specific field)
#   3. Move IN participants with FTOS responses but no Caste to EN
#   4. Identify Indian English speakers within the remaining EN pool -> IN_EN
#      (the remaining non-Indian English respondents are not tied to a single
#      country and are dropped rather than written to their own file)
#   5. Process remaining languages (Spanish, Portuguese, Chinese, etc.)
#
# OUTPUTS:
#   - IN_EN_999625.sav (English, Indian participants)
#   - IN_HI_999625.sav (Hindi, Indian participants)
#   - Multiple country-specific files (MX, RU, IL_AR, CN, IT, NL, etc.)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load and prepare main survey 2 data
df_main_2 <- read_raw("999625.sav") %>%
  # Filter incomplete responses FIRST (before splitting)
  filter(lastpage != -1) %>%
  
  # Standardize to version 2 scale names (second stage)
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  
  # Apply standard column name normalization
  normalize_column_names()

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# STEP 1: Initial split by language (English vs Hindi)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# English language participants (initial assignment)
df_EN_999625 <- df_main_2 %>%
  filter(startlanguage == "en")

# Hindi language participants (initial assignment)
df_IN_99625 <- df_main_2 %>%
  filter(startlanguage == "hi")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# STEP 2: Move EN participants with "Caste" data to IN dataset
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# REASONING: "Caste" is India-specific demographic field
#            If filled out, participant is Indian regardless of language selection

en_with_Caste <- df_EN_999625 %>%
  # Find EN participants who provided Caste information
  filter(!is.na(Caste)) %>%
  
  # Update demographics to reflect Indian nationality
  mutate(
    Nationality = 1,    # Code 1 = India
    Origin = NA         # Clear origin field (now handled by Nationality)
  )

# Remove these participants from EN dataset
df_EN_999625 <- df_EN_999625 %>%
  filter(is.na(Caste))

# Add them to IN dataset
df_IN_99625 <- bind_rows(df_IN_99625, en_with_Caste)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# STEP 3: Move IN participants with FTOS but no Caste to EN dataset
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# REASONING: If they answered FTOS (main scale) but not Caste (India-specific),
#            they likely are not Indian despite selecting Hindi language

# Find all FTOS column names for checking responses
ftos_cols <- names(df_IN_99625)[grep("^FTOS_", names(df_IN_99625))]

# Identify IN participants who answered FTOS but not Caste
in_with_ftos_no_Caste <- df_IN_99625 %>%
  filter(is.na(Caste) & if_any(all_of(ftos_cols), ~ !is.na(.)))

# ADDITIONAL FILTER: Remove anyone who answered SH1 (India-specific item)
in_with_ftos_no_Caste <- in_with_ftos_no_Caste %>%
  filter(is.na(SH1))

# Update these participants' demographics
in_with_ftos_no_Caste <- in_with_ftos_no_Caste %>%
  mutate(
    Nationality = 1,    # Keep Indian nationality
    Origin = NA         # Clear origin field
  )

# Remove these from IN dataset
df_IN_99625 <- df_IN_99625 %>%
  filter(!(is.na(Caste) & if_any(all_of(ftos_cols), ~ !is.na(.))))

# Add to EN dataset
df_EN_999625 <- bind_rows(df_EN_999625, in_with_ftos_no_Caste)

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# STEP 4: Identify Indian English speakers within the remaining EN pool -> IN_EN
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# NOTE: The remaining, non-Indian English respondents (id <= 1124, id >= 6680,
#       or Origin in {Canada, Asian, Indonesia, Malaysia}) are not tied to a
#       single country and are dropped here rather than written to their own file.

# Remove problematic participant ID 3729 (data quality issue)
df_EN_999625 <- df_EN_999625 %>%
  filter(id != 3729)

# Identify Indian English speakers: everyone NOT in the non-Indian
# ID ranges/Origin values below
in_en_new <- df_EN_999625 %>%
  filter(!(
    # Early batch (IDs 1-1124)
    id <= 1124 |
      # Later batch (IDs 6680+)
      id >= 6680 |
      # Specific non-Indian origins
      Origin %in% c("canada", "Asian", "Indonesia", "Malaysia", "MALAYSIA")
  )) %>%

  # Update demographics to reflect Indian nationality
  mutate(
    Nationality = 1,    # Code 1 = India
    Origin = NA         # Clear origin field
  )

# Create in_en dataset (Indian English speakers)
df_in_en_999625 <- in_en_new %>%
  mutate(id = str_c("IN_EN_999625_", id))

write_processed(df_in_en_999625, "IN_EN_999625.sav")

# Finalize IN dataset (Hindi speakers)
df_IN_99625 <- df_IN_99625 %>%
  mutate(id = str_c("IN_HI_999625_", id))

write_processed(df_IN_99625, "IN_HI_999625.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# STEP 5: Process remaining languages from Main Survey 2
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# LANGUAGES: Spanish (Mexico), Portuguese (Brazil), Chinese, Italian, Dutch,
#            Arabic, Indonesian, Malay, Russian, Turkish
# STRATEGY: Group by language/country, apply country-specific filters, output separately

df_main2_other <- df_main_2 %>%
  # Exclude English and Hindi (already processed above)
  filter(!(startlanguage == "hi" | startlanguage == "en")) %>%
  
  # Map language codes to country codes
  mutate(
    country = case_when(
      startlanguage == "es-MX" ~ "MX",        # Spanish (Mexico)
      startlanguage == "pt-BR" ~ "BR_PT",      # Portuguese (Brazil)
      startlanguage %in% c("zh-Hans", "zh-Hant-HK") ~ "CN",  # Chinese (both variants)
      startlanguage %in% c("ar", "AR") ~ "IL_AR", # Arabic -> Israeli Arabic (using AR code but marked as IL_AR dataset)
      startlanguage == "ms" ~ "MY",            # Malay -> Malaysia
      TRUE ~ toupper(startlanguage)           # Default: uppercase 2-letter code (IT, NL, etc.)
    ),

    # NOTE: This never matches — `country` is set to "IL_AR" above (never bare "IL"),
    # so `printed` is always NA_real_ here in practice.
    printed = if_else(country == "IL", 0, NA_real_)
  ) %>%
  
  # Apply country-specific ID filters (remove IDs above cutoffs for quality control)
  # Mexico: Remove IDs >= 6852
  filter(!(country == "MX" & id >= 6852)) %>%
  
  # Indonesia: Remove IDs >= 6676
  filter(!(country == "ID" & id >= 6676)) %>%
  
  # Turkey: Remove IDs >= 6321
  filter(!(country == "TR" & id >= 6321)) %>%
  
  # Create new IDs with country prefix
  mutate(
    id = str_c(country, "_999625_", id),
    
    # Mark data collection wave for Russia
    strategy = if_else(country == "RU", "data collection 1", NA_character_)
  ) %>%
  
  # Split into separate datasets by country
  group_split(country) %>%
  
  # Write each country dataset separately
  # walk() applies function to each element without returning results
  walk(~ {
    # Extract country code from current dataset
    this_country <- unique(.x$country)
    
    # Write file named: COUNTRY_999625.sav (e.g., MX_999625.sav, RU_999625.sav)
    write_processed(.x, glue::glue("{this_country}_999625.sav"))
  })

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# USA OREGON ----
# USA OREGON DATA (216254.sav)
# SOURCE: 216254.sav - English language survey from University of Oregon
# SURVEY STAGE: First stage (full questionnaire)
# SCALE VERSIONS: FTOS_v1, LPS_v1
# LANGUAGE: English
# NOTE: First wave of US data collection (vs 868141 = second wave)
# OUTPUT: US_216254.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
df_us_oregon <- read_raw("216254.sav") %>%
  # Create ID with country prefix, mark as data collection wave 1
  mutate(id = str_c("US_216254_", id), strategy = "data collection 1") %>%
  
  # Fix duplicate prefix in scale names: "FTOS_FTOS1" -> "FTOS_v1_1"
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  
  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_us_oregon, "US_216254.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# NOTE: Combined US dataset creation (US_all.sav) is commented out
# This would merge all US waves together, but is not currently used in pipeline
# US datasets remain separate by wave for now
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# df_us <- bind_rows(df_us1, df_us_oregon)
# write_processed(df_us, "US_all.sav")


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# EXTRA DATASETS
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SOURCE: Various manually curated datasets provided by collaborators
#         These are datasets collected separately or received after main surveys
#
# INCLUDED COUNTRIES:
#   - Netherlands (NL_AUTO): Extra second-stage data
#   - Russia (RU_AUTO_1): Two-source merge (main + Excel supplement)
#   - Israel (IL_AR): Printed paper survey
#   - South Africa (ZA_AUTO): Full dataset with special CAAS scale
#   - Italy (IT_AUTO): Second data collection wave
#   - Russia (RU_AUTO_2): Extra dataset from second data collection wave
#   - Slovakia (SK_AUTO): Full dataset with special LPS-goals column format
#.  - Serbia (RS_AUTO): Excel dataset with a few source-specific column renames
#
# NOTE: These datasets have varied formats and require special handling
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# NETHERLANDS ----
# NETHERLANDS EXTRA DATASET
# SOURCE: "Dataset NL.sav" (manually provided Dutch dataset)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2
# OUTPUT: NL_AUTO.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_nl_extra <- read_raw("Dataset NL.sav") %>%
  # Create ID with NL prefix
  mutate(id = str_c("NL_AUTO_", id)) %>%

  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_nl_extra, "NL_AUTO.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# RUSSIA ----
# RUSSIA EXTRA DATASET - Complex two-source merge
# SOURCE: Two data files that need merging:
#         1. "Dataset_15.08.2022, RU (1).sav" - Main SPSS dataset
#         2. "participants_rus.xlsx" - Excel supplement with additional participants
# SURVEY STAGE: Autonomous
# SCALE VERSIONS: FTOS_v2, LPS_v2
# OUTPUT: RU_AUTO_1.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Load main Russian dataset from SPSS file
df_ru_extra_main <- read_raw("Dataset_15.08.2022, RU (1).sav") %>%
  # Fix data type issues (some columns imported as character, need numeric)
  mutate(
    # Convert submitdate from character to proper datetime format
    submitdate = ymd_hm(submitdate),
    
    # Convert open-ended "other" fields to numeric (contains IDs, not text)
    Race_other = as.numeric(Race_other),
    Occupation_other = as.numeric(Occupation_other),
    
    # Convert LPS goals age columns to numeric
    # Uses across() to apply to multiple columns matching pattern
    across(starts_with("LPSgoals_goal") & ends_with("_age"), as.numeric)
  )

# Load supplementary Russian participants from Excel file
df_ru_extra_second <- read_excel(file.path(DIR_RAW, "participants_rus.xlsx"))

# Create IDs for Excel participants (starting from 101 to avoid conflicts)
df_ru_extra_second$id <- 101:(100 + nrow(df_ru_extra_second))

# Combine both Russian datasets
df_ru_extra <- bind_rows(df_ru_extra_main, df_ru_extra_second) %>%
  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  
  # Standard normalization (no lastpage filter - not all sources have this field)
  normalize_column_names() %>%
  
  # Mark as autonomous Russian data collection
  mutate(strategy = "autonomous")

write_processed(df_ru_extra, "RU_AUTO_1.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ISRAEL ----
# ISRAEL EXTRA DATASET
# SOURCE: "AR Autonomous.sav" (manually provided Israeli Arabic dataset)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2
# ADMINISTRATION: Printed paper survey (printed=1)
# NOTE: Uses Windows-1256 encoding (Arabic Windows codepage) with user_na flag
# OUTPUT: IL_AR_AUTO.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_il_ar_auto <- read_sav(
  file.path(DIR_RAW, "AR Autonomous.sav"),
  encoding = "windows-1256",
  user_na = TRUE
) %>%
  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Standard normalization
  normalize_column_names()

# Create IDs and mark as printed survey
df_il_ar_auto <- df_il_ar_auto %>%
  # Create IDs starting from 101 (avoid conflicts with other IL datasets)
  mutate(id = 101:(100 + nrow(df_il_ar_auto))) %>%

  # Add IL prefix to IDs
  mutate(id = str_c("IL_AR_AUTO_", id)) %>%

  # Mark as printed paper survey (1 = printed, 0 = online)
  mutate(printed = 1)

write_processed(df_il_ar_auto, "IL_AR_AUTO.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SOUTH AFRICA ----
# SOUTH AFRICA DATASET
# SOURCE: "Dataset SA [full].sav" (South African full dataset)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2, CAAS_S (special South African CAAS version)
# NOTE: Other scales were randomly assigned to participants
# OUTPUT: ZA_AUTO.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_sa <- read_raw("Dataset SA [full].sav") %>%
  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # SPECIAL: Rename CAAS to CAAS_S (South African version)
  # "CAAS_1" -> "CAAS_S_1" (distinguishes from standard CAAS)
  rename_with(~ str_replace(.x, "^CAAS_(\\d+)$", "CAAS_S_\\1")) %>%

  # Standard normalization
  normalize_column_names() %>%

  # Create ID with ZA prefix (South Africa = ZA)
  mutate(id = str_c("ZA_AUTO_", id))

write_processed(df_sa, "ZA_AUTO.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# ITALY EXTRA ----
# ITALY EXTRA DATASET (Second Wave)
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SOURCE: "IT Autonomous.sav" (second Italian data collection wave)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2
# NOTE: Second wave of extra Italian data (vs IT3 from Section 5)
# OUTPUT: IT_AUTO.sav
df_it2 <- read_raw("IT Autonomous.sav") %>%
  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Standard normalization
  normalize_column_names() %>%

  # Create ID with IT_AUTO prefix, mark as second wave
  mutate(id = str_c("IT_AUTO_", id), strategy = "data collection 2")

write_processed(df_it2, "IT_AUTO.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# RUSSIA Auto 2 ----
# RUSSIA Autonomous DATASET 2
# SOURCE: "RU autonomous 2.sav" (manually provided Russian dataset)
# SURVEY STAGE: Autonomous
# SCALE VERSIONS: FTOS_v2, LPS_v2
# OUTPUT: RU_AUTO_2.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_ru_auto_2 <- read_raw("RU autonomous 2.sav") %>%
  # Create ID with RU prefix
  mutate(id = str_c("RU_AUTO_2_", id)) %>%

  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Standard normalization and filter incomplete
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_ru_auto_2, "RU_AUTO_2.sav")

# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SLOVAKIA ----
# SLOVAKIA AUTONOMOUS DATASET
# SOURCE: "Slovakia autonomous.sav" (manually provided Slovak dataset)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2
# OUTPUT: SK_AUTO.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_sk_auto <- read_raw("Slovakia autonomous.sav") %>%
  # Create ID with SK prefix
  mutate(id = str_c("SK_AUTO_", id)) %>%

  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Fix LPS goals column naming
  rename_with(~ str_replace(.x, "^ciel_(\\d+)_1$", "LPSgoals\\1_content")) %>%
  rename_with(~ str_replace(.x, "^ciel_(\\d+)_2$", "LPSgoals\\1_age")) %>%

  # Standard normalization
  normalize_column_names()

write_processed(df_sk_auto, "SK_AUTO.sav")


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SERBIA ----
# SERBIA EXTRA DATASET
# SOURCE: "Serbia Autonomous.xlsx" (manually provided Serbian dataset)
# SURVEY STAGE: Second stage
# SCALE VERSIONS: FTOS_v2, LPS_v2
# OUTPUT: RS_AUTO.sav
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

df_rs_auto <- read_excel(file.path(DIR_RAW, "Serbia Autonomous.xlsx")) %>%
  # Create ID with RS prefix
  mutate(id = str_c("RS_AUTO_", row_number())) %>%


  # Standardize to version 2 scale names
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%

  # Rename education_sv to education and Time stamp to timestamp
  rename(education = education_sv, timestamp = `Time stamp`) %>%

  # Standard normalization
  normalize_column_names()

write_processed(df_rs_auto, "RS_AUTO.sav")


# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# END OF 01_split.R
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# SUMMARY: This script has processed ~20 raw survey files and split them into
#          ~40+ country/language-specific processed files, ready for cleaning.
#
# NEXT STEP: Run 02_clean.R to apply quality filters to processed datasets
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░