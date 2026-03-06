# Configuration file for directory paths
# Modify these paths according to your local setup

# Base project directory
PROJECT_ROOT <- "~/Library/CloudStorage/Nextcloud-6161138@soliscom.uu.nl@surfdrive.surf.nl/ftolp_data"

# Data directories
DIR_RAW <- file.path(PROJECT_ROOT, "raw")
DIR_SPLIT <- file.path(PROJECT_ROOT, "split")
DIR_CLEAN <- file.path(PROJECT_ROOT, "clean")
DIR_EXTERNAL <- file.path(PROJECT_ROOT, "external")
DIR_MERGED <- file.path(PROJECT_ROOT, "merged")

# Dataset groupings
DATASETS <- list(
  br_pt = c("br_pilot", "BR_PT_277273", "BR_PT_999625"),
  ch = c("CH_277273", "CH_999625"),
  us = c("US_216254", "US_868141"),
  ch_us = c("CH_277273", "CH_999625", "US_216254", "US_868141"),
  ch_us_10_min = c("CH_277273", "US_868141"),
  pilot = c("br_pilot"),
  first_stage = c(
    "CH_277273", "EN_277273", "ES_277273", "IT_277273",
    "BR_PT_277273", "SL_277273", "IT_extra",
    "US_216254", "US_868141"
  ),
  first_stage_br_pt = c("BR_PT_277273"),
  first_stage_ch = c("CH_277273"),
  first_stage_df = c("EN_277273", "ES_277273", "IT_277273", "BR_PT_277273", "SL_277273")
)