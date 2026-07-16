# These tests run the REAL is_country_nationality()/get_citizen_pattern()/
# label_merge_NAs() functions from 03_merge.R (via load_merge_nationality_functions()
# / load_merge_nas_labeler(), see helper-safe-sourcing.R), not reimplementations.

test_that("is_country_nationality correctly routes each stage: pilot, first_stage, first-stage-auto, second-stage, second-stage-auto", {
  env <- load_merge_nationality_functions()

  # pilot
  expect_true(env$is_country_nationality("BR_PILOT_clean"))
  # first_stage
  expect_true(env$is_country_nationality("IT_277273_clean"))
  expect_true(env$is_country_nationality("BRPT_277273_clean"))
  # Stage 1 ("first-stage") auto: has country-name Nationality like first_stage
  expect_true(env$is_country_nationality("IT_AUTO_clean"))
  # second-stage: NOT country-name Nationality (yes/no citizen indicator instead)
  expect_false(env$is_country_nationality("ES_999625_clean"))
  expect_false(env$is_country_nationality("IN_HI_999625_clean"))
  # Stage 2 auto: also NOT country-name Nationality, despite containing "AUTO"
  expect_false(env$is_country_nationality("RU_AUTO_1_clean"))
  expect_false(env$is_country_nationality("NL_AUTO_clean"))
})

test_that("get_citizen_pattern routes BR_PILOT to the same pattern as BRPT (regression test for the case-mismatch fix)", {
  # Before BR_PILOT.sav was capitalized to match DATASETS$pilot, and before
  # get_citizen_pattern()'s fallback was fixed from "^br_" to "^BR_PILOT",
  # this returned NULL - Citizen was silently never computed for the pilot
  # dataset.
  env <- load_merge_nationality_functions()

  brpt_pattern  <- env$get_citizen_pattern("BRPT_277273_clean")
  pilot_pattern <- env$get_citizen_pattern("BR_PILOT_clean")

  expect_false(is.null(brpt_pattern))
  expect_identical(pilot_pattern, brpt_pattern)
})

test_that("get_citizen_pattern matches each configured country prefix and returns NULL for an unmatched one", {
  env <- load_merge_nationality_functions()

  expect_false(is.null(env$get_citizen_pattern("CN_277273_clean")))
  expect_false(is.null(env$get_citizen_pattern("ES_999625_clean")))
  expect_false(is.null(env$get_citizen_pattern("IT_277273_clean")))
  expect_false(is.null(env$get_citizen_pattern("SI_277273_clean")))
  expect_false(is.null(env$get_citizen_pattern("US_868141_clean")))

  # Regression test for the dead "EN" branch bug class: an unmatched prefix
  # must return NULL (and 03_merge.R's own inline check warns on this - see
  # test-validation.R / the "get_citizen_pattern" warning added in Phase 1).
  expect_null(env$get_citizen_pattern("ZZ_UNKNOWN_clean"))
  expect_null(env$get_citizen_pattern("EN_277273_clean"))
})

test_that("citizen_country_patterns has no dead 'EN' entry (removed after EN_277273/EN_999625 stopped being generated)", {
  env <- load_merge_nationality_functions()
  expect_false("EN" %in% names(env$citizen_country_patterns))
})

test_that("label_merge_NAs replaces only genuine R NAs with 990, leaving existing 990/991/999 codes untouched", {
  env <- load_merge_nas_labeler()

  # A labelled_spss column simulating a merged column: one row already coded
  # 991 (technical error), one already 999 (true missing), one genuine R NA
  # (padding introduced by bind_rows() because this column didn't exist in
  # that row's source dataset), and one real answered value.
  col <- labelled_spss(
    c(3, 991, 999, NA),
    labels = c(by_design = 990, technical_error = 991, missing = 999),
    na_values = c(990, 991, 999)
  )
  df <- tibble(x = col)

  out <- env$label_merge_NAs(df)
  values <- as.numeric(unclass(out$x))

  expect_equal(values, c(3, 991, 999, 990))
  # the SPSS value labels should still be attached, just re-applied
  expect_true(is.labelled(out$x))
})

test_that("label_merge_NAs leaves character columns untouched", {
  env <- load_merge_nas_labeler()
  df <- tibble(x = c("a", NA, "b"))
  out <- env$label_merge_NAs(df)
  expect_equal(out$x, c("a", NA, "b"))
})

test_that("reason_codes has exactly the 3 SPSS-allowed missing-value codes (990/991/999), not a stray 993 or extra codes", {
  # Regression test for the "993" typo found in label_merge_NAs()'s comments
  # this session - reason_codes is the actual source of truth those
  # comments should have matched.
  expect_equal(unname(reason_codes[["by_design"]]), 990)
  expect_equal(unname(reason_codes[["tech_error"]]), 991)
  expect_equal(unname(reason_codes[["missing"]]), 999)
  expect_length(reason_codes, 3)
})

test_that("numerical_cols' scale-item derivation (SCALE_PATTERNS matched against all_cols) covers every scale, not just FTOS/LPS", {
  # Regression test for the bug found this session: numerical_cols used to be
  # built from a single hand-written "^(FTOS|LPS)_(pilot|v1|v2)_\\d+$" regex,
  # so select(any_of(relevant_cols)) silently dropped every other scale
  # (CAAS, MLQ, AS, IPIP, GRIT, DASS, IT, DMF, Psy_DGI/Psy_LOT, HS, ES,
  # ESW_PS, CAAS_S, Psy_CIPIP, ...) from the merged dataset. This mirrors the
  # exact derivation now in 03_merge.R.
  all_cols <- c(
    "id", "FTOS_v1_1", "LPS_v1_1", "CAAS_1", "CAAS_S_1", "MLQ_1", "AS_1",
    "IPIP_1", "GRIT_1", "DASS_1", "IT_1", "DMF_1", "Psy_DGI1", "Psy_LOT1",
    "HS_1", "ES_1", "ESW_PS1", "BRS_1", "FS_SES1", "Psy_CIPIP1",
    "FTOS_x", "LPS_x", "CAAS_x", # attention-check columns, must NOT be swept in
    "SomeUnrelatedColumn"
  )
  scale_item_cols <- unique(unlist(lapply(SCALE_PATTERNS, function(p) grep(p, all_cols, value = TRUE))))

  expected <- setdiff(all_cols, c("id", "FTOS_x", "LPS_x", "CAAS_x", "SomeUnrelatedColumn"))
  expect_setequal(scale_item_cols, expected)
})
