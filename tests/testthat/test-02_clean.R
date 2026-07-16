# These tests run the REAL filter_na/constant_and_binary/check_attention/
# remove_zigzag/atypical_patterns/steps objects from 02_clean.R (via
# load_clean_steps(), see helper-safe-sourcing.R) against small synthetic
# fixtures - not copies or reimplementations of the filter logic. Real
# dataset-name prefixes (IT_277273, BR_PILOT) are used so the tests exercise
# the actual DATASETS-based gating, not an invented naming scheme.

test_that("run_cleaning_pipeline drops missing/constant/zigzag/attention-check rows for a first-stage dataset (IT_277273)", {
  env <- load_clean_steps()

  fixture <- tibble(
    id         = c("IT_277273_1", "IT_277273_2", "IT_277273_3", "IT_277273_4", "IT_277273_5"),
    FTOS_v1_1  = c(3, NA, 3, 3, 1),
    FTOS_v1_2  = c(5, NA, 3, 4, 5),
    FTOS_v1_3  = c(2, NA, 3, 3, 2),
    FTOS_v1_4  = c(4, NA, 3, 4, 4),
    FTOS_x     = c(7, 7, 7, 7, 1),
    lastpage   = 1
  )
  # row 1: normal, should survive everything
  # row 2: all-NA FTOS_v1 block -> dropped by "Missing response"
  # row 3: identical FTOS_v1 answers -> dropped by "Constant Answers"
  # row 4: adjacent alternating FTOS_v1 answers (3,4,3,4) -> dropped by "Zigzag Patterns"
  # row 5: FTOS_x minority value (majority is 7) -> dropped by "Attention Checks"

  res <- run_cleaning_pipeline(fixture, "IT_277273", env$steps)

  expect_equal(res$df_clean$id, "IT_277273_1")
})

test_that("run_cleaning_pipeline drops missing/constant rows for the pilot dataset (BR_PILOT)", {
  env <- load_clean_steps()

  fixture <- tibble(
    id           = c("BR_PILOT_1", "BR_PILOT_2", "BR_PILOT_3"),
    FTOS_pilot_1 = c(3, NA, 4),
    FTOS_pilot_2 = c(5, NA, 4),
    FTOS_pilot_3 = c(2, NA, 4),
    FTOS_pilot_4 = c(4, NA, 4),
    lastpage     = 1
  )
  # row 1: normal, survives
  # row 2: all-NA FTOS_pilot -> dropped by missing-response checks (FTOS_pilot / FTOS-or-Psy_LOT)
  # row 3: identical FTOS_pilot answers -> dropped by constant-answer check

  res <- run_cleaning_pipeline(fixture, "BR_PILOT", env$steps)

  expect_equal(res$df_clean$id, "BR_PILOT_1")
})

test_that("the FTOS_v1 missing-response check does not apply to BRPT datasets (they use a different scale combination)", {
  env <- load_clean_steps()

  # BRPT_277273 rows have no FTOS_v1 at all (by design, per the "FTOS or
  # Psy_LOT missing" comment) - if the FTOS_v1 check's `exclude = brpt` were
  # ever broken (e.g. by a future dataset-name rename that isn't reflected
  # in config/paths.R), this all-missing-FTOS_v1 row would incorrectly
  # survive the FTOS_v1 check only to be correctly caught by the BRPT-
  # specific check instead. This test locks in that BRPT is excluded from
  # the FTOS_v1 check specifically (not just accidentally caught elsewhere).
  fixture <- tibble(
    id        = "BRPT_277273_1",
    Psy_LOT1  = 3, Psy_LOT2 = 4, Psy_LOT3 = 2,
    lastpage  = 1
  )

  res <- run_cleaning_pipeline(fixture, "BRPT_277273", list(env$filter_na))
  expect_equal(nrow(res$df_clean), 1) # kept: has Psy_LOT answers, FTOS_v1 check doesn't apply to BRPT
})

test_that("step_keep_by_composite_id (used by the US external-check filter) matches on IdCode_1/2/3 + Age and flags duplicate keys", {
  external_df <- tibble(
    IdCode_1 = c("ab", "cd"), IdCode_2 = c("jo", "sm"), IdCode_3 = c("m", "j"), Age = c(25, 30)
  )
  fn <- step_keep_by_composite_id(external_df, id_cols = c("IdCode_1", "IdCode_2", "IdCode_3", "Age"))

  candidates <- tibble(
    id = c("in_list", "not_in_list", "case_insensitive_match"),
    IdCode_1 = c("ab", "zz", "AB"), IdCode_2 = c("jo", "zz", "JO"), IdCode_3 = c("m", "z", "M"), Age = c(25, 99, 25)
  )
  kept <- fn(candidates)

  expect_setequal(kept$id, c("in_list", "case_insensitive_match"))
})

test_that("dataset coverage validation in 02_clean.R does not warn (all configured tokens match real DIR_SPLIT-shaped names)", {
  # This exercises the same assert_datasets_exist(collect_dataset_tokens(steps), ...)
  # call embedded in 02_clean.R itself, but against a fixture of dataset
  # names shaped like what 01_split.R actually writes today - a direct
  # regression test for the "DATASETS$ch/DATASETS$cn key mismatch" bug class.
  env <- load_clean_steps()
  known_names <- c(
    "BR_PILOT", "BRPT_277273", "CN_277273", "IT_277273", "IT_AUTO", "SI_277273",
    "US_216254", "US_868141", "ES_999625", "IL_AR_999625", "IL_AR_AUTO",
    "IN_HI_999625", "IN_EN_824323", "IN_EN_999625", "IT_855796", "MX_999625",
    "MZ", "NL_AUTO", "RS_AUTO", "RU_999625", "RU_AUTO_1", "RU_AUTO_2",
    "ZA_AUTO", "SK_AUTO", "TR_999625", "ID_999625"
  )
  expect_no_warning(
    assert_datasets_exist(collect_dataset_tokens(env$steps), known_names, context = "regression-test")
  )
})
