test_that("normalize_column_names collapses redundant double prefixes", {
  env <- load_split_functions()
  df <- tibble(IT_IT1 = 1, CAAS_CAAS5 = 2, id = "x")
  out <- env$normalize_column_names(df)
  expect_true(all(c("IT_1", "CAAS_5") %in% names(out)))
})

test_that("normalize_column_names standardizes known scale abbreviations", {
  env <- load_split_functions()
  df <- tibble(
    CAAS_SQ001 = 1, Prospera_Pr2 = 2, DASS21_DASS3 = 3,
    ESS_ES4 = 4, MiLQ_5 = 5, LS_BRS6 = 6, id = "x"
  )
  out <- env$normalize_column_names(df)
  expect_true(all(c("CAAS_1", "Prospera_2", "DASS_3", "ES_4", "MLQ_5", "BRS_6") %in% names(out)))
})

test_that("normalize_column_names capitalizes demographic columns and fixes control-item/date casing", {
  env <- load_split_functions()
  df <- tibble(
    age = 30, gender = 1, education = 2, origin = "x", nationality = "y",
    CAAS_X = 1, FTOS_X = 1, LPS_X = 1, StartDate = "2024-01-01", id = "x"
  )
  out <- env$normalize_column_names(df)
  expect_true(all(c(
    "Age", "Gender", "Education", "Origin", "Nationality",
    "CAAS_x", "FTOS_x", "LPS_x", "startdate"
  ) %in% names(out)))
})

test_that("normalize_column_names fixes LPS goals column naming", {
  env <- load_split_functions()
  df <- tibble(LPSgoals_goal1_content = "a", LPSgoals2_age = 30, id = "x")
  out <- env$normalize_column_names(df)
  expect_true(all(c("LPSgoal1_content", "LPSgoal2_age") %in% names(out)))
})

test_that("normalize_column_names drops rows where Ref/Name EXACTLY equals 'test' but keeps near-matches", {
  # Regression test: the comments used to (wrongly) say this filter matched
  # on "contains 'test'"; the actual regex is anchored (^test$), so a Ref
  # like "testperson" or "beta tester" must survive.
  env <- load_split_functions()
  df <- tibble(
    id = c("a", "b", "c", "d"),
    Ref = c("test", "TEST", "testperson", NA),
    FTOS_v1_1 = c(1, 2, 3, 4)
  )
  out <- env$normalize_column_names(df)
  expect_equal(sort(out$id), c("c", "d"))
})

test_that("write_processed creates a subdirectory named after the token before the first underscore", {
  env <- load_split_functions()
  tmp <- tempfile("dir_split_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  env$DIR_SPLIT <- tmp

  df <- tibble(id = "BRPT_277273_1", FTOS_v1_1 = 3)
  env$write_processed(df, "BRPT_277273.sav")

  # Regression test for the BR_PT -> BRPT rename: BRPT_277273.sav must land
  # in its own BRPT/ folder now, not share BR/ with BR_PILOT.sav.
  expect_true(file.exists(file.path(tmp, "BRPT", "BRPT_277273.sav")))
})

test_that("write_processed drops all-NA columns before saving", {
  env <- load_split_functions()
  tmp <- tempfile("dir_split_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  env$DIR_SPLIT <- tmp

  df <- tibble(id = "CN_277273_1", FTOS_v1_1 = 3, all_na_col = NA)
  env$write_processed(df, "CN_277273.sav")

  written <- read_sav(file.path(tmp, "CN", "CN_277273.sav"))
  expect_false("all_na_col" %in% names(written))
})

test_that("rename_scales_brazil_pilot splits LoTeDGI into Psy_DGI (odd items) and Psy_LOT (even items)", {
  env <- load_split_functions()
  cols <- as.list(1:13)
  names(cols) <- sprintf("LoTeDGI_SQ%03d", 1:13)
  df <- as_tibble(cols)
  out <- env$rename_scales_brazil_pilot(df)
  expect_true(all(paste0("Psy_DGI", 1:7) %in% names(out)))
  expect_true(all(paste0("Psy_LOT", 1:6) %in% names(out)))
  expect_equal(nrow(out), 1)
})

test_that("rename_scales_brazil_pilot is a no-op when no LoTeDGI columns are present", {
  env <- load_split_functions()
  df <- tibble(FTOS_v1_1 = 1, id = "x")
  out <- env$rename_scales_brazil_pilot(df)
  expect_equal(names(out), names(df))
})

test_that("brazil_pilot_fix_factors converts A1/A2/A3-style factor levels to numeric while preserving the question label", {
  # Mimics the real LimeSurvey export quirk: data stored as character
  # "A1"/"A2"/"A3" but with numeric-keyed value labels attached.
  col <- structure(
    c("A1", "A2", "A3", "A1"),
    label = "I feel a sense of purpose",
    labels = c("Strongly Disagree" = 1, "Disagree" = 2, "Neutral" = 3),
    class = "haven_labelled"
  )
  df <- tibble(Psy_DGI1 = col)

  env <- load_split_functions()
  out <- env$brazil_pilot_fix_factors(df)

  expect_equal(as.numeric(out$Psy_DGI1), c(1, 2, 3, 1))
  expect_equal(attr(out$Psy_DGI1, "label"), "I feel a sense of purpose")
})

test_that("rename_scale_version tags FTOS/LPS columns with the given survey-stage version", {
  env <- load_split_functions()
  df <- tibble(FTOS_1 = 1, LPS_2 = 2, id = "x")
  out <- env$rename_scale_version(df, "v2")
  expect_true(all(c("FTOS_v2_1", "LPS_v2_2") %in% names(out)))
})

test_that("rename_scale_version fixes the FTOS_FTOS#/LPS_LPS# duplicate-prefix export quirk only when asked", {
  env <- load_split_functions()
  df <- tibble(FTOS_FTOS1 = 1, LPS_LPS2 = 2, id = "x")

  out_off <- env$rename_scale_version(df, "v1", fix_duplicate_prefix = FALSE)
  expect_true(all(c("FTOS_FTOS1", "LPS_LPS2") %in% names(out_off))) # untouched

  out_on <- env$rename_scale_version(df, "v1", fix_duplicate_prefix = TRUE)
  expect_true(all(c("FTOS_v1_1", "LPS_v1_2") %in% names(out_on)))
})

test_that("process_dataset chains id-prefixing, extra_mutate, scale renaming, normalization and lastpage filtering in order", {
  env <- load_split_functions()
  tmp <- tempfile("dir_split_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  env$DIR_SPLIT <- tmp

  fixture <- tibble(
    id = c(1, 2), FTOS_1 = c(3, 4), LPS_1 = c(5, 6), lastpage = c(1, -1)
  )
  fake_read <- function(filename) fixture

  out <- env$process_dataset(
    raw_file = "fake.sav", id_prefix = "ZZ_1_", scale_version = "v2",
    output_file = "ZZ_1.sav",
    extra_mutate = \(df) df %>% dplyr::mutate(strategy = "wave 1"),
    read_fn = fake_read
  )

  expect_equal(out$id, "ZZ_1_1")              # id-prefixed, then lastpage=-1 row dropped
  expect_true(all(c("FTOS_v2_1", "LPS_v2_1") %in% names(out)))
  expect_equal(out$strategy, "wave 1")
  expect_true(file.exists(file.path(tmp, "ZZ", "ZZ_1.sav")))
})

test_that("process_dataset applies pre_normalize_steps between scale renaming and normalization", {
  env <- load_split_functions()
  tmp <- tempfile("dir_split_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  env$DIR_SPLIT <- tmp

  fixture <- tibble(id = 1, CAAS_1 = 5, lastpage = 1)
  fake_read <- function(filename) fixture

  out <- env$process_dataset(
    raw_file = "fake.sav", id_prefix = "ZZ_", scale_version = "v2",
    output_file = "ZZ.sav",
    pre_normalize_steps = \(df) df %>% dplyr::rename_with(~ stringr::str_replace(.x, "^CAAS_(\\d+)$", "CAAS_S_\\1")),
    read_fn = fake_read
  )

  expect_true("CAAS_S_1" %in% names(out))
})

test_that("process_dataset skips the lastpage filter when filter_incomplete = FALSE", {
  env <- load_split_functions()
  tmp <- tempfile("dir_split_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  env$DIR_SPLIT <- tmp

  fixture <- tibble(id = c(1, 2), FTOS_1 = c(3, 4), lastpage = c(1, -1))
  fake_read <- function(filename) fixture

  out <- env$process_dataset(
    raw_file = "fake.sav", id_prefix = "ZZ_", scale_version = "v2",
    output_file = "ZZ.sav", filter_incomplete = FALSE, read_fn = fake_read
  )

  expect_equal(nrow(out), 2) # the lastpage=-1 row is kept
})
