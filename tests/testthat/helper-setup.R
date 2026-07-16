# Sourced automatically by testthat before any test file runs (testthat's
# helper-*.R convention). Loads config + the pure-function utility files.
# None of this touches real survey data - it only defines paths and
# functions. config/paths.R just does string assignments (DATA_ROOT etc.);
# it does not require DATA_ROOT to actually exist on disk.
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(haven)
  library(labelled)
  library(purrr)
})

source(here::here("config", "paths.R"))
source(here::here("src", "utils", "cleaning_functions.R"))
source(here::here("src", "utils", "validation.R"))
source(here::here("src", "utils", "merge_functions.R"))
source(here::here("config", "scales.R"))  # depends on mk_step() from cleaning_functions.R
source(here::here("config", "translations.R"))

# DEFENSE IN DEPTH: neutralize the real data-directory paths for the whole
# test session, in addition to the safe-extraction helpers in
# helper-safe-sourcing.R. DATASETS/DATA_ROOT stay as sourced from the real
# config (tests need real dataset-name groupings like DATASETS$first_stage),
# but DIR_RAW/DIR_SPLIT/DIR_CLEAN/DIR_MERGED point at a directory that does
# not exist, so any test code path that unexpectedly tries real file I/O
# fails loudly instead of silently touching production data - this is
# exactly the mistake that overwrote the real DIR_SPLIT once already while
# developing this test suite.
.test_sandbox_dir <- file.path(tempdir(), "ftolp_tests_do_not_use_real_data")
DIR_RAW      <- file.path(.test_sandbox_dir, "raw")
DIR_SPLIT    <- file.path(.test_sandbox_dir, "split")
DIR_CLEAN    <- file.path(.test_sandbox_dir, "clean")
DIR_EXTERNAL <- file.path(.test_sandbox_dir, "external")
DIR_MERGED   <- file.path(.test_sandbox_dir, "merged")
