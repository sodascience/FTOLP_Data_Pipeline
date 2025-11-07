library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(tools)
library(writexl)

setwd("~/surfdrive/Narrating the Future (Bogdan)")
source("Bogdan R/utils_cleaning.R")

# load files
setwd("~/surfdrive/Narrating the Future (Bogdan)/LimeSurvey Processed")

file_list <- list.files(
  path = "./",
  pattern = "\\.sav$",
  full.names = TRUE,
)

br_pt <- c("br_pilot", "PTBR_277273", "PTBR_999625")
ch <- c("CH_277273", "CH_999625")
us <- c("US_all", "US_216254", "US_868141")
ch_us <- c(ch, us)
ch_us_10_min <- c("CH_277273", "US_868141")
first_stage <- c(
  "CH_277273",
  "EN_277273",
  "ES_277273",
  "IT_277273",
  "PTBR_277273",
  "SL_277273",
  "US_all",
  "IT_extra",
  "US_216254",
  "US_868141"
)

first_stage_br_pt <- c("PTBR_277273")
first_stage_ch <- c("CH_277273")

# AS (BR, PT, CH, US), MiLQ (BR, PT, CH, US), CIPIP (BR, PT), LS (CH), Grit (US)

# step 1
step_1 <- mk_group("Missing response",
  steps = list(
    mk_step(
      "FTOS_v1 missing",
      step_drop_na_block(),
      exclude = br_pt # skip on br_pt
    ),
    mk_step(
      "FTOS_v2 or LPS missing",
      step_drop_na_block("^(FTOS_v2_\\d+|LPS_v2_\\d+)$"),
      exclude = c(br_pt, "IT_extra") # skip on br_pt
    ),
    mk_step(
      "FTOS_pilot missing",
      step_drop_na_block("^FTOS_pilot_\\d+$")
      # no gating
    ),
    mk_step(
      "FTOS or Psy_LOT missing",
      step_drop_na_block("^(Psy_LOT\\d+|FTOS_(?:pilot|v1|v2)_\\d+)$"),
      datasets = br_pt # run only on br_pt
    )
  )
)

constant_and_binary <- mk_group(
  "Drop rows with constant responses",
  steps = list(
    mk_step("FTOS_v1", step_constant_answers()),
    mk_step("FTOS_v2", step_constant_answers("^FTOS_v2_\\d+$"), datasets = c("IT_extra")),
    mk_step("FTOS_pilot", step_constant_answers("^FTOS_pilot_\\d+$")),
    mk_step("DGI", step_constant_answers(col_pattern = "^Psy_DGI\\d+$")),
    mk_step("LOT", step_constant_answers(col_pattern = "^Psy_LOT\\d+$")),
    # mk_step(
    #  "LoTeDGI",
    #  step_constant_answers(col_pattern = "^LoTeDGI_SQ\\d+$")
    # ),
    # mk_step("EA",             step_constant_answers(col_pattern = "^EA_SQ\\d+$")),
    # mk_step("QSV",            step_constant_answers(col_pattern = "^QSV_SQ\\d+$")),
    mk_step(
      "IPIP",
      step_constant_answers(col_pattern = "^IPIP_\\d+$"),
      datasets = br_pt
    ),
    mk_step(
      "LS",
      step_constant_answers(col_pattern = "^LS_BRS\\d+$"),
      datasets = ch
    ),
    mk_step(
      "MLQ",
      step_constant_answers(col_pattern = "^MLQ_\\d+$"),
      datasets = c(br_pt, ch_us)
    ),
    mk_step(
      "AS",
      step_constant_answers(col_pattern = "^AS_\\d+$"),
      datasets = c(br_pt, ch_us)
    ),
    mk_step(
      "GRIT",
      step_constant_answers(col_pattern = "^GRIT_\\d+$"),
      datasets = us
    )
  )
)

check_attention <- mk_group(
  "Check attention control items",
  steps = list(
    mk_step("FTOS", step_check_control()),
    mk_step("LPS", step_check_control(col = "LPS_x")),
    mk_step("CFC", step_check_control(col = "CFC_x")),
    mk_step("CAAS", step_check_control(col = "CAAS_x")),
    mk_step("SWLS", step_check_control(col = "SWLS_x")),
    mk_step("FTPQ", step_check_control(col = "FTPQ_x")),
    mk_step("DMF", step_check_control(col = "DMF_x")),
    mk_step("FTPtr", step_check_control(col = "FTPtr_x")),
    mk_step("Jung", step_check_control(col = "Jung_x")),
    mk_step("MH", step_check_control(col = "MH_x")),
    mk_step("IPS", step_check_control(col = "IPS_x"))
  )
)

remove_zigzag <- mk_group(
  "Remove zigzag answers",
  steps = list(
    # --- All datasets ---
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

    # --- IT: IT_IT, DMF ---
    mk_step(
      "IT",
      step_detect_zigzag(col_pattern = "^IT_\\d+$"),
      datasets = c("IT_277273", "IT_extra")
    ),
    mk_step(
      "DMF",
      step_detect_zigzag(col_pattern = "^DMF_\\d+$"),
      datasets = c("IT_277273", "IT_extra")
    )
  )
)

scale_patterns_list <- list(
  FTOS = "^FTOS_v1_\\d+$",
  FTOS_v2 = "^FTOS_v2_\\d+$",
  FTOS_pilot = "^FTOS_pilot_\\d+$",
  LPS = "^LPS_v1_\\d+$",
  LPS_v2 = "^LPS_v2_\\d+$",
  CAAS = "^CAAS_\\d+$",
  DGI = "^(Psy_)?DGI_?\\d+$",
  SWLS = "^SWLS_\\d+$",
  IT = "^IT_\\d+$",
  Pr = "^Pr_\\d+$",
  DMF = "^DMF_\\d+$",
  MLQ = "^MLQ_\\d+$",
  AS = "^AS_\\d+$",
  LS = "^LS_BRS\\d+$",
  ESW = "^ESW_PS\\d+$",
  ESS = "^ES_\\d+$",
  FS = "^FS_\\d+$",
  GRIT = "^GRIT_\\d+$",
  IPIP = "^IPIP_\\d+$",
  LOT = "^Psy_LOT\\d+$"
)

mahalanobis_guttman <- mk_group("Mahalanobis/Guttman", steps = list(
  mk_step(
    "Mahalanobis",
    step_mahalanobis(scale_patterns_list),
    datasets = first_stage
  ),
  mk_step("Guttman", step_guttman(scale_patterns_list), datasets = first_stage)
))

steps <- list(
  step_1,
  list(
    name = "Drop short submitted responses (<10 min)",
    fn = step_filter_min_duration(),
    datasets = ch_us_10_min
  ),
  # list(
  #  name = "Remove foreigners",
  #  fn = step_remove_foreigners(),
  #  datasets = us
  # ),
  # mk_step(name = "Remove > 65", step_filter_age(), datasets = us),
  constant_and_binary,
  remove_zigzag,
  mahalanobis_guttman,
  check_attention
)

all_summaries <- list()


for (f in file_list) {
  tryCatch(
    {
      df <- read_sav(f)
      name <- file_path_sans_ext(basename(f))

      # potential checks here
      # if ("FTOS_x" %in% names(df)) {
      #  print_column_info(df, "FTOS_x")
      # }

      res <- run_cleaning_pipeline(df, name, steps)
      df_clean <- res$df_clean
      audit <- res$audit


      summary_wide <- build_wide_summary(
        name = name,
        df_initial = df,
        df_final = df_clean,
        audit = audit,
      )

      all_summaries[[name]] <- summary_wide

      write_sav(df_clean, file.path("clean", paste0(name, "_clean.sav")))
      # write_csv(summary_wide, paste0(name, "_summary.csv"))
      assign(paste0("df_", name), df, envir = .GlobalEnv)
    },
    error = function(e) {
      message("Error while processing ", f, ": ", conditionMessage(e))
    }
  )
}

summary_all <- bind_rows(all_summaries)
write_xlsx(summary_all, "clean_summary.xlsx")

# Create additional filtered versions for US datasets
us_additional_filters <- list(
  list(
    name = "Remove foreigners",
    fn = step_remove_foreigners(),
    datasets = us
  ),
  mk_step(name = "Remove > 65", step_filter_age(), datasets = us)
)

for (dataset_name in us) {
  tryCatch(
    {
      # Load the already cleaned file
      clean_file <- file.path("clean", paste0(dataset_name, "_clean.sav"))
      if (file.exists(clean_file)) {
        df_clean <- read_sav(clean_file)

        # Apply additional filters
        res_more <- run_cleaning_pipeline(df_clean, dataset_name, us_additional_filters)
        df_more_filtered <- res_more$df_clean

        # Save with _clean_more_filters suffix
        write_sav(df_more_filtered, file.path("clean", paste0(dataset_name, "_clean_more_filters.sav")))
        message("Created ", dataset_name, "_clean_more_filters.sav")
      } else {
        message("Clean file not found for ", dataset_name)
      }
    },
    error = function(e) {
      message("Error while creating more filtered version for ", dataset_name, ": ", conditionMessage(e))
    }
  )
}
