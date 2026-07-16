# Entry point for the test suite. Run from the project root with:
#   Rscript -e "testthat::test_dir('tests/testthat')"
# or source this file from RStudio/R with the working directory set to the
# project root (here::here() resolves paths relative to the .Rproj file).
#
# See tests/testthat/helper-safe-sourcing.R for why 01_split.R/02_clean.R/
# 03_merge.R are never sourced directly: they read/write real survey data
# from DATA_ROOT, so tests extract just the functions/objects under test
# instead of running the scripts.
library(testthat)
library(here)

test_dir(here::here("tests", "testthat"))
