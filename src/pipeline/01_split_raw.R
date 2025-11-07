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

# Helper function to read from raw data directory
read_raw <- function(filename) {
  read_sav(file.path(DIR_RAW, filename))
}

# Helper function to read from extra raw data directory
read_extra_raw <- function(filename) {
  read_sav(file.path(DIR_EXTRA_RAW, filename))
}

# Helper function to write to processed data directory
write_processed <- function(df, filename) {
  df <- df %>% select(where(~ !all(is.na(.))))
  write_sav(df, file.path(DIR_PROCESSED, filename))
}

# Note: Using absolute paths with here() - no need to change working directory

# !! NOTE: condensing other surveys as well, assuming correspondence (i.e., IT_IT1 = IT1)
normalize_column_names <- function(df) {
  df %>%
    rename_with(~ str_replace(.x, "^([A-Za-z]+)_\\1(\\d+)$", "\\1_\\2")) %>%
    # rename_with(~ str_replace(.x, "^CAAS_CAAS(\\d+)$", "CAAS_\\1")) %>%
    rename_with(~ str_replace(.x, "^CAAS_SQ00(\\d+)$", "CAAS_\\1")) %>%
    rename_with(~ str_replace(.x, "^Prospera_Pr(\\d+)$", "Prospera_\\1")) %>%
    rename_with(~ str_replace(.x, "^DASS21_DASS(\\d+)$", "DASS_\\1")) %>%
    # rename_with(~ str_replace(.x, "^IT_IT(\\d+)$", "IT_\\1")) %>%
    rename_with(~ str_replace(.x, "^ESS_ES(\\d+)$", "ES_\\1")) %>%
    # rename_with(~ str_replace(.x, "^ES_ES(\\d+)$", "ES_\\1")) %>%
    rename_with(~ str_replace(.x, "^MiLQ_(\\d+)$", "MLQ_\\1")) %>%
    # rename_with(~ str_replace(.x, "^GRIT_GRIT(\\d+)$", "GRIT_\\1")) %>%
    rename_with(~ str_replace(.x, "^EASY_EASY(\\d+)#(\\d+)$", "EASY_\\1#\\2")) %>%
    rename_with(~ sub("^(.)", "\\U\\1", ., perl = TRUE), .cols = any_of(c(
      "age", "nationality", "gender", "education", "origin"
    ))) %>%
    rename_with(~ str_replace(.x, "^LS_BRS(\\d+)$", "BRS_\\1")) %>%
    rename_with(~ str_replace(.x, "^CAAS_X$", "CAAS_x")) %>%
    rename_with(~ str_replace(.x, "^FTOS_X$", "FTOS_x")) %>%
    rename_with(~ str_replace(.x, "^LPS_X$", "LPS_x")) %>%
    rename_with(~ str_replace(.x, "^StartDate$", "startdate")) %>%
    {
      result <- .
      # Filter out test participants if Ref or Name columns exist
      if ("Ref" %in% names(result)) {
        result <- result %>% filter(is.na(Ref) | !str_detect(tolower(Ref), "^test$"))
      }
      if ("Name" %in% names(result)) {
        result <- result %>% filter(is.na(Name) | !str_detect(tolower(Name), "^test$"))
      }
      result
    }
}

write_clean <- function(df, path) {
  df <- df %>% select(where(~ !all(is.na(.))))
  write_sav(df, path)
}

rename_scales_brazil_pilot <- function(df) {
  cols <- grep("^LoTeDGI_SQ\\d{3}$", names(df), value = TRUE)
  if (!length(cols)) {
    return(df)
  }

  idx <- as.integer(str_remove(cols, "^LoTeDGI_SQ0*")) # 1..13

  new_names <- ifelse(
    idx %% 2 == 1,
    # odd -> DGI 1..7
    paste0("Psy_DGI", (idx + 1) %/% 2),
    paste0("Psy_LOT", idx %/% 2) # even -> LOT 1..6
  )

  # Build rename map: new = old (only for valid indices 1..13)
  map <- setNames(cols, new_names)

  df %>% rename(!!!map)
}

# fix some factor values (i.e., A1 -> 1) in brazil pilot dataset
brazil_pilot_fix_factors <- function(df) {
  cols <- grep("MLQ|CAAS|Psy_LOT|Psy_DGI|MLQ|AS", names(df), value = TRUE)
  if (!length(cols)) {
    return(df)
  }

  df %>%
    mutate(
      across(
        .cols = all_of(cols),
        .fns = ~ {
          # Check if the column is a labelled vector
          if (is.labelled(.x)) {
            # Get the existing value labels and variable label
            current_labels <- val_labels(.x)
            var_lbl <- var_label(.x)

            factor_vec <- as.factor(.x)

            numeric_values <- as.integer(factor_vec)

            new_labels_map <- setNames(
              seq_along(levels(factor_vec)),
              levels(factor_vec)
            )
            new_labelled_vec <- labelled(
              x = numeric_values,
              labels = new_labels_map,
              label = var_lbl # Preserve the variable label if it existed
            )

            return(new_labelled_vec)
          } else {
            message(sprintf("Skipping non-labelled column: %s", cur_column()))
            return(.x)
          }
        }
      )
    )
}

# Mozambique (all second stage)
df_mz_online <- read_raw("276893.sav") %>%
  mutate(id = str_c("MZ_276893_", id))
df_mz_printed <- read_raw("429283.sav") %>%
  mutate(id = str_c("MZ_429283_", id))
df_mz_3 <- read_raw("827663.sav") %>%
  mutate(id = str_c("MZ_827663_", id))
df_mz_online$printed <- 0
df_mz_printed$printed <- 1
df_mz_3$printed <- 0

# mark second stage
df_mz <- bind_rows(df_mz_online, df_mz_printed, df_mz_3) %>%
  mutate(dataset = "MZ") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_mz, "MZ.sav")

# 277273 -> main (IT1, pt & pt-BR, SL, China simplified and traditional )
# participants for instance who selected portuguese but answered in chinese
# split it into all corresponding datasets (except european pt vs brazilian pt)
# chinese put them together
# all first stage
df_main1 <- read_raw("277273.sav")

# change column names
df_main1 <- df_main1 %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  mutate(id = as.integer(id)) %>%
  filter(lastpage != -1)

# separate china
china_surveys <- c(
  # CAAS_CAAS7 to CAAS_CAAS25
  paste0("CAAS_", 7:25),

  # LS_BRS1 to LS_BRS6
  paste0("BRS_", 1:6),

  # ESW_PS1 to ESW_PS22
  paste0("ESW_PS", 1:22),

  # ESS_ES1 to ESS_ES14
  paste0("ES_", 1:14),

  # JSS_JSS1 to JSS_JSS4
  paste0("JSS_", 1:4)
)

# nationalities: Macao = 106, China = 35, Hong Kong = 79, Taiwan = 162
china_languages <- c("zh-Hans", "zh-Hant-HK")
china_nationalities <- c(106, 35, 79, 162)

china_ids <- df_main1 %>%
  filter(
    startlanguage %in% china_languages |
      str_detect(coalesce(Name, ""), regex("^kc", ignore_case = TRUE)) |
      if_any(all_of(china_surveys), ~ !is.na(.)) |
      if_any(everything(), ~ str_detect(
        coalesce(as.character(.), ""), "[\u4e00-\u9fff]"
      )) |
      Nationality %in% china_nationalities
  ) %>%
  pull(id) %>%
  unique()

sl_ids <- df_main1 %>%
  filter(startlanguage == "sl") %>%
  pull(id)
it_ids <- df_main1 %>%
  filter(startlanguage == "it") %>%
  pull(id)
es_ids <- df_main1 %>%
  filter(startlanguage == "es") %>%
  pull(id)
en_ids <- df_main1 %>%
  filter(startlanguage == "en") %>%
  pull(id)

non_ptbr_ids <- unique(c(china_ids, sl_ids, it_ids, es_ids, en_ids))

df_main1_china <- df_main1 %>%
  filter(id %in% china_ids) %>%
  filter(!if_any(GRIT_1:GRIT_10, ~ !is.na(.))) %>%
  filter(id < 10169) %>%
  mutate(id = str_c("CH_277273_", id), dataset = "CH")

df_sl <- df_main1 %>%
  filter(id %in% sl_ids) %>%
  filter(id < 10400) %>%
  mutate(id = str_c("SL_277273_", id), dataset = "SL")
df_it <- df_main1 %>%
  filter(id %in% it_ids) %>%
  mutate(id = str_c("IT_277273_", id), strategy = "data collection 1", dataset = "IT")
df_es <- df_main1 %>%
  filter(id %in% es_ids) %>%
  mutate(id = str_c("ES_277273_", id), dataset = "ES")
df_en <- df_main1 %>%
  filter(id %in% en_ids) %>%
  mutate(id = str_c("EN_277273_", id), dataset = "Oth")

df_ptbr_duplicate_ids <- read_sav(file.path(DIR_RAW, "..", "duplicates_PT&BR.sav")) %>%
  mutate(id = id - 22200000) %>%
  pull(id)
df_ptbr_conditions <- read_sav(file.path(DIR_RAW, "..", "conditions.sav"))

df_ptbr <- df_main1 %>%
  filter(!(id %in% non_ptbr_ids | id %in% df_ptbr_duplicate_ids)) %>%
  filter(id < 9971) %>%
  mutate(id = str_c("PTBR_277273_", id), dataset = "PTBR") %>%
  left_join(df_ptbr_conditions %>% select(id, Condition), by = "id") %>%
  mutate(Condition = if_else(!is.na(Condition.y) & Condition.y != "", Condition.y, Condition.x)) %>%
  select(-Condition.x, -Condition.y)


write_processed(df_main1_china, "CH_277273.sav")
write_processed(df_sl, "SL_277273.sav")
write_processed(df_it, "IT_277273.sav")
write_processed(df_es, "ES_277273.sav")
write_processed(df_en, "EN_277273.sav")
write_processed(df_ptbr, "PTBR_277273.sav")

frequency_table <- df_main1 %>%
  # Convert to factor first
  mutate(nationality = as_factor(Nationality)) %>%
  # Count the occurrences of each label
  count(nationality, sort = TRUE) %>%
  # Optionally calculate the percentage
  mutate(percentage = n / sum(n) * 100)

# 569687.sav pilot only brazil
# merge with DataSet BR&PT
df_dataset_brpt <- read_sav(file.path(DIR_RAW, "..", "Datasets", "Brazil & Portugal", "DataSet BR&PT.sav")) %>%
  rename_with(~ str_replace(.x, "^FTOS_pilot(\\d+)$", "FTOS_pilot_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_pilot(\\d+)$", "LPS_pilot_\\1"))

df_brazil_pilot <- read_raw("569687.sav") %>%
  mutate(id = id + 11100000) %>%
  select(!starts_with("QSD") & !starts_with("Cod")) %>%
  rename(Email = email) # %>%
# rename_with(
# .fn = ~ str_replace(., "FTOS_SQ0{1,2}", "FTOS_pilot_"),
# .cols = starts_with("FTOS_SQ00") | starts_with("FTOS_SQ0")
# )

ftos_lps_cols <- c(paste0("FTOS_pilot_", 1:15), paste0("LPS_pilot_", 1:21))

df_dataset_brpt <- df_dataset_brpt %>%
  select(
    id,
    all_of(ftos_lps_cols),
    starts_with("IdCode_"),
    Age,
    Sex,
    Gender,
    Gender_other,
    SexOrientation,
    SexOrientation_other,
    starts_with("Race_"),
    starts_with("Religion"),
    Nationality,
    starts_with("Immigration"),
    starts_with("Relationship"),
    starts_with("Residence"),
    starts_with("Children"),
    Education,
    starts_with("Adults"),
    starts_with("EducationAdult"),
    starts_with("Occupation"),
    starts_with("Family"),
    FinancialDependency,
    starts_with("Health"),
    starts_with("Psychiatric"),
    starts_with("PsyDiagnosis"),
    starts_with("PsyTreatment")
  )


df_brazil_pilot_merged <- df_brazil_pilot %>%
  left_join(df_dataset_brpt, by = "id") %>%
  mutate(id = str_c("BR_569687_", id), dataset = "BR_pilot") %>%
  rename_with(
    .fn = ~ str_replace(., "QSV_SQ0{1,2}", "MLQ_"),
    .cols = starts_with("QSV_SQ00") | starts_with("QSV_SQ0")
  ) %>%
  rename_with(
    .fn = ~ str_replace(., "EA_SQ0{1,2}", "AS_"),
    .cols = starts_with("EA_SQ00") | starts_with("EA_SQ0")
  ) %>%
  normalize_column_names() %>%
  rename_scales_brazil_pilot() %>%
  brazil_pilot_fix_factors() %>%
  filter(lastpage != -1)

write_processed(df_brazil_pilot_merged, "br_pilot.sav")

# 824323.sav english india
# second stage
df_india <- read_raw("824323.sav") %>%
  mutate(id = str_c("EN_IN_824323_", id), dataset = "EN_I") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_india, "EN_IN_824323.sav")

# 855796.sav IT3 second stage
df_it3 <- read_raw("855796.sav") %>%
  mutate(id = str_c("IT_855796_", id), strategy = "data collection 3", dataset = "IT") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_it3, "IT_855796.sav")


# 868141.sav US1 (messy) first stage
df_us1 <- read_raw("868141.sav") %>%
  mutate(id = str_c("US_868141_", id), strategy = "data collection 2", dataset = "US") %>%
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_us1, "US_868141.sav")

# 999625.sav main second stage (multiple lang)
df_main_2 <- read_raw("999625.sav") %>%
  filter(lastpage != -1) %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names()

df_EN_999625 <- df_main_2 %>%
  filter(startlanguage == "en")

df_IN_99625 <- df_main_2 %>%
  filter(startlanguage == "hi")

# a) Move participants from EN_999625 who filled in "Caste" to IN_99625
en_with_Caste <- df_EN_999625 %>%
  filter(!is.na(Caste)) %>%
  mutate(
    Nationality = 1,
    Origin = NA
  )

df_EN_999625 <- df_EN_999625 %>%
  filter(is.na(Caste))

df_IN_99625 <- bind_rows(df_IN_99625, en_with_Caste)

# b) Move participants from IN_99625 who answered FTOS but not Caste to EN_999625
# First, identify participants who answered FTOS (check if any FTOS columns are not NA)
ftos_cols <- names(df_IN_99625)[grep("^FTOS_", names(df_IN_99625))]

in_with_ftos_no_Caste <- df_IN_99625 %>%
  filter(is.na(Caste) & if_any(all_of(ftos_cols), ~ !is.na(.)))

# Remove participant who answered SH1
in_with_ftos_no_Caste <- in_with_ftos_no_Caste %>%
  filter(is.na(SH1))

# Update these participants
in_with_ftos_no_Caste <- in_with_ftos_no_Caste %>%
  mutate(
    Nationality = 1,
    Origin = NA
  )

# Remove these from IN_99625
df_IN_99625 <- df_IN_99625 %>%
  filter(!(is.na(Caste) & if_any(all_of(ftos_cols), ~ !is.na(.))))

# Add to EN_999625
df_EN_999625 <- bind_rows(df_EN_999625, in_with_ftos_no_Caste)

# c) Split EN_999625 into EN_999625 and EN_IN_999625
# Remove participant id 3729
df_EN_999625 <- df_EN_999625 %>%
  filter(id != 3729)

# Identify participants for EN_999625
en_keep <- df_EN_999625 %>%
  filter(
    id <= 1124 |
      id >= 6680 |
      Origin %in% c("canada", "Asian", "Indonesia", "Malaysia", "MALAYSIA")
  )

# Remaining participants go to EN_IN_999625
en_in_new <- df_EN_999625 %>%
  filter(!(
    id <= 1124 |
      id >= 6680 |
      Origin %in% c("canada", "Asian", "Indonesia", "Malaysia", "MALAYSIA")
  )) %>%
  mutate(
    Nationality = 1,
    Origin = NA
  )

# Update df_EN_999625 to only keep specified participants
df_EN_999625 <- en_keep

# Create EN_IN_999625 dataset
df_EN_IN_999625 <- en_in_new %>%
  mutate(
    id = str_c("EN_IN_999625_", id),
    dataset = "EN_I"
  )

write_processed(df_EN_IN_999625, "EN_IN_999625.sav")

# Update EN_999625 with proper formatting
df_EN_999625 <- df_EN_999625 %>%
  mutate(
    id = str_c("EN_999625_", id),
    dataset = "Oth"
  )

write_processed(df_EN_999625, "EN_999625.sav")

# Update IN_99625 with proper formatting
df_IN_99625 <- df_IN_99625 %>%
  mutate(
    id = str_c("IN_999625_", id),
    dataset = "HI"
  )

write_processed(df_IN_99625, "IN_999625.sav")

df_main2_other <- df_main_2 %>%
  filter(!(startlanguage == "hi" | startlanguage == "en")) %>%
  mutate(
    country = case_when(
      startlanguage == "es-MX" ~ "MX",
      startlanguage == "pt-BR" ~ "PTBR",
      startlanguage %in% c("zh-Hans", "zh-Hant-HK") ~ "CH",
      TRUE ~ toupper(startlanguage)
    ),
    dataset = case_when(
      country == "AR" ~ "PS_IS",
      country == "ID" ~ "ID",
      country == "MS" ~ "MS",
      country == "MX" ~ "MX",
      country == "RU" ~ "RU",
      country == "TR" ~ "TK",
      country == "CH" ~ "Oth",
      country == "PTBR" ~ "Oth",
      country == "IT" ~ "Oth",
      country == "NL" ~ "Oth",
      TRUE ~ NA_character_
    ),
    printed = if_else(country == "AR", 0, NA_real_)
  ) %>%
  filter(!(country == "MX" & id >= 6852)) %>%
  filter(!(country == "ID" & id >= 6676)) %>%
  filter(!(country == "TR" & id >= 6321)) %>%
  mutate(
    id = str_c(country, "_999625_", id),
    strategy = if_else(country == "RU", "data collection 1", NA_character_)
  ) %>%
  group_split(country) %>%
  walk(~ {
    this_country <- unique(.x$country)
    write_processed(.x, glue::glue("{this_country}_999625.sav"))
  })

# 216254 also dataset from UOregon first stage
df_us_oregon <- read_raw("216254.sav") %>%
  mutate(id = str_c("US_216254_", id), strategy = "data collection 1", dataset = "US") %>%
  rename_with(~ str_replace(.x, "^FTOS_FTOS(\\d+)$", "FTOS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_LPS(\\d+)$", "LPS_v1_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_us_oregon, "US_216254.sav")

# df_us <- bind_rows(df_us1, df_us_oregon)
# write_processed(df_us, "US_all.sav")

# extra datasets (from extra raw data directory)
df_nl_extra <- read_extra_raw("Dataset NL.sav") %>%
  mutate(id = str_c("NL_extra_", id), dataset = "NL") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  filter(lastpage != -1)

write_processed(df_nl_extra, "NL_extra.sav")

df_ru_extra_main <- read_extra_raw("Dataset_15.08.2022, RU (1).sav") %>%
  mutate(
    submitdate = ymd_hm(submitdate),
    Race_other = as.numeric(Race_other),
    Occupation_other = as.numeric(Occupation_other),
    across(starts_with("LPSgoals_goal") &
      ends_with("_age"), as.numeric)
  )
df_ru_extra_second <- read_excel(file.path(DIR_EXTRA_RAW, "participants_rus.xlsx"))
df_ru_extra_second$id <- 101:(100 + nrow(df_ru_extra_second))

df_ru_extra <- bind_rows(df_ru_extra_main, df_ru_extra_second) %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names() %>%
  mutate(strategy = "data collection 2", dataset = "RU")

write_processed(df_ru_extra, "RU_extra.sav")

df_ar_extra <- read_sav(file.path(DIR_EXTRA_RAW, "Dataset AR.sav 16.4.2023.sav"), encoding = "latin1") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals_goal(\\d+_)", "LPSgoal\\1")) %>%
  rename_with(~ str_replace(.x, "^LPSgoals(\\d+_)", "LPSgoal\\1")) %>%
  normalize_column_names()

df_ar_extra <- df_ar_extra %>%
  mutate(id = 101:(100 + nrow(df_ar_extra))) %>%
  mutate(id = str_c("AR_extra_", id)) %>%
  mutate(dataset = "PS_IS") %>%
  mutate(printed = 1)


write_processed(df_ar_extra, "AR_extra.sav")

# south africa
# other scales randomly assigned
df_sa <- read_extra_raw("Dataset SA [full].sav") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  # special CAAS
  rename_with(~ str_replace(.x, "^CAAS_(\\d+)$", "CAAS_S_\\1")) %>%
  normalize_column_names() %>%
  mutate(id = str_c("SA_extra_", id), dataset = "SA")

write_processed(df_sa, "SA_extra.sav")

# italy extra
df_it2 <- read_extra_raw("Dataset IT2.sav") %>%
  rename_with(~ str_replace(.x, "^FTOS_(\\d+)$", "FTOS_v2_\\1")) %>%
  rename_with(~ str_replace(.x, "^LPS_(\\d+)$", "LPS_v2_\\1")) %>%
  normalize_column_names() %>%
  mutate(id = str_c("IT_extra", id), strategy = "data collection 2", dataset = "IT")

write_processed(df_it2, "IT_extra.sav")
