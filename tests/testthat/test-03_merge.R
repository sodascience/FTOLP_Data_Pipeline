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

test_that("derive_source_country extracts the country code (token before the first underscore) from source_dataset", {
  env <- load_derive_source_country()
  out <- env$derive_source_country(c(
    "CN_277273_clean", "BRPT_277273_clean", "MZ_clean", "RU_AUTO_1_clean", "RU_AUTO_2_clean"
  ))
  expect_equal(out, c("CN", "BRPT", "MZ", "RU", "RU"))
})

test_that("derive_source_country collapses language-suffixed datasets to the bare country code", {
  env <- load_derive_source_country()
  out <- env$derive_source_country(c("IL_AR_999625_clean", "IL_AR_AUTO_clean", "IN_HI_999625_clean", "IN_EN_824323_clean"))
  expect_equal(out, c("IL", "IL", "IN", "IN"))
})

test_that("derive_source_country maps BR_PILOT to 'BR' (its country), not 'BR_PILOT'", {
  env <- load_derive_source_country()
  expect_equal(env$derive_source_country("BR_PILOT_clean"), "BR")
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

test_that("extract_lps_goals pulls id + LPSgoal*_content/_age columns, skipping datasets with none", {
  env <- load_lps_goals_extractor()
  dfs <- list(
    A = tibble(
      id = "A_1", source_dataset = "A", FTOS_v1_1 = 3,
      LPSgoal1_content = "become a doctor", LPSgoal1_age = 30
    ),
    B = tibble(id = "B_1", source_dataset = "B", FTOS_v1_1 = 4) # no LPSgoal columns at all
  )
  out <- env$extract_lps_goals(dfs)

  expect_equal(nrow(out), 1) # dataset B contributes no row
  expect_setequal(names(out), c("id", "source_dataset", "LPSgoal1_content", "LPSgoal1_age"))
  expect_equal(out$id, "A_1")
  expect_false("FTOS_v1_1" %in% names(out))
})

test_that("extract_lps_goals standardizes types across datasets so bind_rows doesn't error (regression: a dataset with a numeric id/age column used to break bind_rows)", {
  env <- load_lps_goals_extractor()
  dfs <- list(
    CHAR_DS = tibble(
      id = "CHAR_1", source_dataset = "CHAR_DS",
      LPSgoal1_content = "learn to code", LPSgoal1_age = "25"
    ),
    NUMERIC_DS = tibble(
      id = 2, source_dataset = "NUMERIC_DS", # id stored as numeric
      LPSgoal1_content = "travel", LPSgoal1_age = 40 # age already numeric
    )
  )
  out <- env$extract_lps_goals(dfs)

  expect_equal(nrow(out), 2)
  expect_type(out$id, "character")
  expect_type(out$LPSgoal1_content, "character")
  expect_type(out$LPSgoal1_age, "double")
  expect_setequal(out$id, c("CHAR_1", "2"))
  expect_setequal(out$LPSgoal1_age, c(25, 40))
})

test_that("extract_lps_goals unions differing goal-column sets across datasets and pads the rest with NA", {
  env <- load_lps_goals_extractor()
  dfs <- list(
    FEW = tibble(id = "F_1", source_dataset = "FEW", LPSgoal1_content = "a", LPSgoal1_age = 20),
    MANY = tibble(
      id = "M_1", source_dataset = "MANY",
      LPSgoal1_content = "b", LPSgoal1_age = 22,
      LPSgoal2_content = "c", LPSgoal2_age = 24
    )
  )
  out <- env$extract_lps_goals(dfs)

  expect_setequal(names(out), c("id", "source_dataset", "LPSgoal1_content", "LPSgoal1_age", "LPSgoal2_content", "LPSgoal2_age"))
  expect_true(is.na(out$LPSgoal2_content[out$id == "F_1"]))
})

test_that("extract_lps_goals returns an empty (zero-row) tibble, not an error, when no dataset has LPSgoal columns", {
  env <- load_lps_goals_extractor()
  dfs <- list(A = tibble(id = "A_1", source_dataset = "A", FTOS_v1_1 = 1))
  out <- env$extract_lps_goals(dfs)
  expect_equal(nrow(out), 0)
})

test_that("the Occupation_other normalization step converts character free text to a 0/1 flag without touching numeric datasets", {
  # Regression test: Occupation_other is character free text ("please
  # specify") in most datasets but already a 0/1 numeric flag in a few
  # (IT_AUTO, RU_AUTO_1/2, ZA_AUTO) - the generic type-standardization step
  # would otherwise convert character text to NA via as.numeric(), silently
  # losing the "selected other" signal.
  dfs <- list(
    TEXT_DS = tibble(id = c("A", "B", "C"), Occupation_other = c("teacher", "", NA)),
    NUMERIC_DS = tibble(id = "D", Occupation_other = 1),
    NO_COL_DS = tibble(id = "E", FTOS_v1_1 = 3)
  )
  out <- load_occupation_other_normalizer(dfs)
  expect_equal(out$TEXT_DS$Occupation_other, c(1, 0, 0))
  expect_equal(out$NUMERIC_DS$Occupation_other, 1) # untouched, already numeric
  expect_false("Occupation_other" %in% names(out$NO_COL_DS)) # untouched, absent
})

test_that("char_demo_cols pads every categorical_cols column (minus id/source_dataset) to '990', not a hand-picked subset", {
  # Regression test: Sex/Gender_v1/Gender_v2/Gender_other/ImmigrationCountry
  # used to be left as "" (haven's SPSS round-trip of a padding NA) instead
  # of "990" like Nationality/Origin, for datasets that never collected them.
  # char_demo_cols is now derived from categorical_cols so a newly added
  # categorical column can't be forgotten here again.
  env <- load_char_demo_cols()
  expect_setequal(
    env$char_demo_cols,
    c("Nationality", "Origin", "ImmigrationCountry", "Sex", "Gender_v1", "Gender_v2", "Gender_other",
      "Education_RS", "Occupation_SK")
  )
  expect_false(any(c("id", "source_dataset") %in% env$char_demo_cols))
})

test_that("labelled_categorical_to_character renders missing-value codes as their numeric string, not the English label", {
  # Regression test: genuine non-response in a categorical column used to
  # decode to the literal word "missing" (from as_factor()'s label text for
  # 999) instead of "999", inconsistent with how numeric columns and
  # char_demo_cols' post-merge padding both display missing codes.
  env <- load_labelled_categorical_to_character()
  col_data <- labelled_spss(
    c(1, 2, 999, 990, 991),
    labels = c("Mulher" = 1, "Homem" = 2, "by_design" = 990, "technical_error" = 991, "missing" = 999),
    na_values = c(990, 991, 999)
  )
  out <- env$labelled_categorical_to_character(col_data)
  expect_equal(out, c("Mulher", "Homem", "999", "990", "991"))
})

test_that("labelled_categorical_to_character leaves real value labels untouched", {
  env <- load_labelled_categorical_to_character()
  col_data <- labelled_spss(
    c(1, 2, 666),
    labels = c("Mulher" = 1, "Homem" = 2, "other" = 666),
    na_values = c(990, 991, 999)
  )
  out <- env$labelled_categorical_to_character(col_data)
  expect_equal(out, c("Mulher", "Homem", "other"))
})

test_that("normalize_categorical_column codes blank ('' or NA) responses in an already-character column as '999'", {
  # Regression test: Gender_other (an open-ended free-text field) is plain
  # character from the raw data, so it never passes through the
  # labelled-numeric missing-value step - a blank response there used to
  # stay a literal "" instead of being coded "999" like every other
  # categorical column's genuine non-response.
  env <- load_normalize_categorical_column()
  out <- env$normalize_categorical_column(c("Non-binary", "", NA, "Agender"))
  expect_equal(out, c("Non-binary", "999", "999", "Agender"))
})

test_that("normalize_categorical_column still routes labelled columns through labelled_categorical_to_character", {
  env <- load_normalize_categorical_column()
  col_data <- labelled_spss(
    c(1, 999),
    labels = c("Mulher" = 1, "missing" = 999),
    na_values = c(990, 991, 999)
  )
  out <- env$normalize_categorical_column(col_data)
  expect_equal(out, c("Mulher", "999"))
})

test_that("the categorical translation step in 03_merge.R translates Sex/Gender_*/Nationality/Origin/ImmigrationCountry and keeps '<column>_original', while leaving Gender_other untouched", {
  merged_df <- tibble(
    id = "A_1", source_dataset = "A",
    Sex = "Mulher", Gender_v1 = "Homem", Gender_v2 = "990",
    Nationality = "Brasil", Origin = "999", ImmigrationCountry = "no",
    Gender_other = "Non-binary"
  )
  out <- load_categorical_translation_step(merged_df)

  expect_equal(out$Sex, "Female")
  expect_equal(out$Sex_original, "Mulher")
  expect_equal(out$Gender_v1, "Male")
  expect_equal(out$Gender_v2, "990") # missing code, untouched
  expect_equal(out$Nationality, "Brazil")
  expect_equal(out$Origin, "999") # missing code, untouched
  expect_equal(out$ImmigrationCountry, "no") # not a recognized country, passes through
  expect_equal(out$ImmigrationCountry_original, "no")

  expect_false("Gender_other_original" %in% names(out))
  expect_equal(out$Gender_other, "Non-binary")
})
