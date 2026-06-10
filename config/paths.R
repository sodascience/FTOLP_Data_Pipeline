# Configuration file for directory paths
# Modify these paths according to your local setup

# Base data directory
DATA_ROOT <- "~/Library/CloudStorage/Nextcloud-6161138@soliscom.uu.nl@surfdrive.surf.nl/ftolp_data"

# Data directories
DIR_RAW <- file.path(DATA_ROOT, "raw")
DIR_SPLIT <- file.path(DATA_ROOT, "split")
DIR_CLEAN <- file.path(DATA_ROOT, "clean")
DIR_EXTERNAL <- file.path(DATA_ROOT, "external")
DIR_MERGED <- file.path(DATA_ROOT, "merged")

# Dataset groupings
DATASETS <- list(
  br_pt = c("br_pilot", "BR_PT_277273"),
  ch = c("CH_277273"),
  us = c("US_216254", "US_868141"),
  ch_us_10_min = c("CH_277273", "US_868141"),
  pilot = c("br_pilot"),
  # first-stage datasets (all except IT_auto, which uses second-stage LPS and FTOS short scales)
  first_stage = c(
    "CH_277273", "IT_277273",
    "BR_PT_277273", "SL_277273",  "US_216254", "US_868141"
  ),
  first_stage_br_pt = c("BR_PT_277273"),
  # list of datasets to remove
  datasets_to_remove = c("NL_999625", "BR_PT_999625", "CH_999625", "EN_277273", "EN_999625", "ES_277273", "IT_999625", "MS_999625")
)