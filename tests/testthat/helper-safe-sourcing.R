# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# helper-safe-sourcing.R
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# 01_split.R, 02_clean.R and 03_merge.R are monolithic scripts: alongside the
# functions/objects we want to unit test, they immediately execute code that
# reads real raw/cleaned survey data from DATA_ROOT (a Nextcloud-synced path
# that DOES exist on this machine) and writes real output files. Sourcing
# them directly in a test would re-run the real pipeline against real data -
# which is exactly what happened once already while developing this test
# suite: `source("01_split.R", local = env)` ran to completion and
# overwrote every file in the real DIR_SPLIT.
#
# These helpers extract ONLY the specific line ranges that define the
# functions/objects under test, using text markers (not line numbers, which
# drift) to find the boundaries, and eval them in an isolated environment.
# Any line that performs real file I/O within an otherwise-safe range is
# skipped and replaced with a small mock.
#
# IMPORTANT: if the marker text below is edited in the pipeline scripts,
# these helpers will raise a clear error pointing at the missing marker
# rather than silently extracting the wrong range or nothing at all.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

.read_pipeline_lines <- function(script) {
  readLines(here::here("src", "pipeline", script), warn = FALSE)
}

.find_marker <- function(lines, pattern, script, what) {
  idx <- which(grepl(pattern, lines))[1]
  if (is.na(idx)) {
    stop(sprintf(
      "Could not find marker '%s' (%s) in %s - the test helper extraction in helper-safe-sourcing.R needs updating.",
      pattern, what, script
    ), call. = FALSE)
  }
  idx
}

.find_closing_brace_after <- function(lines, from_idx, script, what) {
  rel <- which(grepl("^\\}$", lines[(from_idx + 1):length(lines)]))[1]
  if (is.na(rel)) {
    stop(sprintf(
      "Could not find closing '}' after %s in %s - the test helper extraction needs updating.",
      what, script
    ), call. = FALSE)
  }
  from_idx + rel
}

# 01_split.R: everything before the "# MOZAMBIQUE ----" section banner is
# pure function definitions (read_raw, write_processed,
# normalize_column_names, rename_scales_brazil_pilot,
# brazil_pilot_fix_factors). Everything from MOZAMBIQUE onward reads real
# raw files.
load_split_functions <- function() {
  lines <- .read_pipeline_lines("01_split.R")
  boundary <- .find_marker(lines, "^# MOZAMBIQUE", "01_split.R", "start of executable pipeline code")

  env <- new.env(parent = globalenv())
  eval(parse(text = lines[seq_len(boundary - 1)]), envir = env)
  env
}

# 02_clean.R: everything up to "# MAIN PROCESSING LOOP ----" defines the
# cleaning steps (filter_na, constant_and_binary, check_attention,
# remove_zigzag, atypical_patterns, base_steps, steps, dataset groupings).
# The one real-I/O line in that range (loading the external US inclusion
# file) is skipped and replaced with a small empty mock - tests that need a
# populated external_us_df build their own via step_keep_by_composite_id()
# directly instead of going through this loader.
load_clean_steps <- function() {
  lines <- .read_pipeline_lines("02_clean.R")
  boundary <- .find_marker(lines, "^# MAIN PROCESSING LOOP", "02_clean.R", "start of main processing loop")
  is_real_io_line <- grepl("^external_us_df <- read_sav\\(", lines)

  code_lines <- lines[seq_len(boundary - 1)]
  code_lines <- code_lines[!is_real_io_line[seq_len(boundary - 1)]]

  env <- new.env(parent = globalenv())
  env$external_us_file <- "mocked - not read from disk in tests"
  env$external_us_df <- tibble::tibble(
    IdCode_1 = character(0), IdCode_2 = character(0),
    IdCode_3 = character(0), Age = numeric(0)
  )
  suppressWarnings(eval(parse(text = code_lines), envir = env))
  env
}

# 03_merge.R: the Nationality/Citizen routing logic (FIRST_STAGE_AUTO,
# is_country_nationality(), citizen_yes_pattern, citizen_country_patterns,
# get_citizen_pattern()) is self-contained and safe to extract - it doesn't
# read any files. It does contain one assert_datasets_exist() validation
# call that references `dfs`, which we pre-seed as an empty list so the
# extraction doesn't error (that call just emits a harmless warning against
# an empty dataset list in this context).
load_merge_nationality_functions <- function() {
  lines <- .read_pipeline_lines("03_merge.R")
  start <- .find_marker(lines, "^FIRST_STAGE_AUTO <- ", "03_merge.R", "start of nationality routing logic")
  fn_start <- .find_marker(lines, "^get_citizen_pattern <- function", "03_merge.R", "get_citizen_pattern()")
  end <- .find_closing_brace_after(lines, fn_start, "03_merge.R", "get_citizen_pattern()")

  env <- new.env(parent = globalenv())
  env$dfs <- list()
  suppressWarnings(eval(parse(text = lines[start:end]), envir = env))
  env
}

# 03_merge.R: label_merge_NAs() is a self-contained function (only needs
# reason_codes, already loaded from merge_functions.R via helper-setup.R).
load_merge_nas_labeler <- function() {
  lines <- .read_pipeline_lines("03_merge.R")
  start <- .find_marker(lines, "^label_merge_NAs <- function", "03_merge.R", "label_merge_NAs()")
  end <- .find_closing_brace_after(lines, start, "03_merge.R", "label_merge_NAs()")

  env <- new.env(parent = globalenv())
  env$all_na_values <- c(990, 991, 999)
  eval(parse(text = lines[start:end]), envir = env)
  env
}

# 03_merge.R: extract_lps_goals() is a self-contained function (takes a `dfs`
# list as its only input, does no file I/O) - safe to extract directly.
load_lps_goals_extractor <- function() {
  lines <- .read_pipeline_lines("03_merge.R")
  start <- .find_marker(lines, "^extract_lps_goals <- function", "03_merge.R", "extract_lps_goals()")
  end <- .find_closing_brace_after(lines, start, "03_merge.R", "extract_lps_goals()")

  env <- new.env(parent = globalenv())
  eval(parse(text = lines[start:end]), envir = env)
  env
}
