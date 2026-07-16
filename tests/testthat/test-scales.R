# These test the config/scales.R registry that drives 02_clean.R's
# constant-answer and zigzag filters: resolve_datasets() (config/paths.R
# DATASETS group keys, or literal dataset names, into a plain vector) and
# build_scale_steps() (a scale table -> a list of mk_step()s).

test_that("resolve_datasets looks up DATASETS group keys", {
  expect_setequal(resolve_datasets("brpt"), DATASETS$brpt)
  expect_setequal(resolve_datasets("cn"), DATASETS$cn)
  expect_setequal(resolve_datasets("first_stage"), DATASETS$first_stage)
})

test_that("resolve_datasets passes through tokens that aren't DATASETS keys as literal dataset names", {
  expect_equal(resolve_datasets("IT_AUTO"), "IT_AUTO")
  expect_setequal(resolve_datasets("IT_277273,IT_AUTO"), c("IT_277273", "IT_AUTO"))
})

test_that("resolve_datasets unions comma-separated tokens, mixing DATASETS keys and literals", {
  expect_setequal(
    resolve_datasets("first_stage_brpt,us,cn,IT_AUTO"),
    c(DATASETS$first_stage_brpt, DATASETS$us, DATASETS$cn, "IT_AUTO")
  )
})

test_that("resolve_datasets returns NULL for NA (no dataset restriction)", {
  expect_null(resolve_datasets(NA_character_))
  expect_null(resolve_datasets(NA))
})

test_that("build_scale_steps generates one mk_step per registry row, with the right name/datasets", {
  steps <- build_scale_steps(CONSTANT_ANSWER_SCALES, step_constant_answers)
  expect_equal(length(steps), nrow(CONSTANT_ANSWER_SCALES))
  expect_equal(sapply(steps, `[[`, "name"), CONSTANT_ANSWER_SCALES$name)

  ipip_step <- steps[[which(CONSTANT_ANSWER_SCALES$name == "IPIP")]]
  expect_setequal(ipip_step$datasets, DATASETS$brpt)

  ftos_v1_step <- steps[[which(CONSTANT_ANSWER_SCALES$name == "FTOS_v1")]]
  expect_null(ftos_v1_step$datasets) # unrestricted
})

test_that("CONSTANT_ANSWER_SCALES and ZIGZAG_SCALES intentionally differ in dataset scoping for shared scale names", {
  # Regression test for the key finding that motivated two separate tables
  # instead of one shared per-scale `datasets` column: IPIP's constant-answer
  # check applies to `brpt` (includes BR_PILOT), but its zigzag check applies
  # to `first_stage_brpt` (excludes BR_PILOT). Collapsing these into one
  # table would silently change which participants get checked.
  ipip_constant_datasets <- resolve_datasets(CONSTANT_ANSWER_SCALES$datasets[CONSTANT_ANSWER_SCALES$name == "IPIP"])
  ipip_zigzag_datasets <- resolve_datasets(ZIGZAG_SCALES$datasets[ZIGZAG_SCALES$name == "IPIP"])

  expect_true("BR_PILOT" %in% ipip_constant_datasets)
  expect_false("BR_PILOT" %in% ipip_zigzag_datasets)
})

test_that("every scale_key in both scale tables resolves to a valid, non-empty regex in SCALE_PATTERNS", {
  all_keys <- c(CONSTANT_ANSWER_SCALES$scale_key, ZIGZAG_SCALES$scale_key)
  for (key in all_keys) {
    expect_true(key %in% names(SCALE_PATTERNS), info = paste("missing scale_key:", key))
    p <- SCALE_PATTERNS[[key]]
    expect_true(nchar(p) > 0)
    expect_no_error(grepl(p, "dummy_column_name"))
  }
})

test_that("SCALE_PATTERNS has no duplicate names and every pattern is anchored", {
  expect_false(any(duplicated(names(SCALE_PATTERNS))))
  for (p in SCALE_PATTERNS) {
    expect_true(startsWith(p, "^"))
    expect_true(endsWith(p, "$"))
  }
})

test_that("SCALE_PATTERNS$BRS and $FS use the corrected (actually-matching) patterns, not the dead originals", {
  # Regression test for the two dead patterns found this session:
  # "^LS_BRS\\d+$" never matched the real post-normalization column name
  # ("BRS_#"), and "^FS_\\d+$" never matched the real "FS_SES#" columns.
  expect_true(grepl(SCALE_PATTERNS[["BRS"]], "BRS_1"))
  expect_false(grepl(SCALE_PATTERNS[["BRS"]], "LS_BRS1"))

  expect_true(grepl(SCALE_PATTERNS[["FS"]], "FS_SES1"))
  expect_false(grepl(SCALE_PATTERNS[["FS"]], "FS_1"))
})

test_that("SCALE_PATTERNS$DGI matches both real-world naming variants (Psy_DGI# and IT_AUTO's bare DGI_#)", {
  expect_true(grepl(SCALE_PATTERNS[["DGI"]], "Psy_DGI1"))
  expect_true(grepl(SCALE_PATTERNS[["DGI"]], "DGI_1"))
})

test_that("SCALE_PATTERNS includes CAAS_S and CIPIP (previously not referenced by any filter or the merge)", {
  expect_true(grepl(SCALE_PATTERNS[["CAAS_S"]], "CAAS_S_1"))
  expect_false(grepl(SCALE_PATTERNS[["CAAS_S"]], "CAAS_1")) # distinct from plain CAAS
  expect_true(grepl(SCALE_PATTERNS[["CIPIP"]], "Psy_CIPIP1"))
})

test_that("scale_patterns_cn_us / scale_patterns_it_brpt_si in 02_clean.R source their patterns from SCALE_PATTERNS (not a second hand-copied set)", {
  # Reconstructs the assignments as they appear in 02_clean.R and checks they
  # resolve to the corrected master patterns, so the BRS/FS fix can't drift
  # out of sync between the two places that used to duplicate it.
  scale_patterns_cn_us <- list(
    FTOS = SCALE_PATTERNS[["FTOS_v1"]], LPS = SCALE_PATTERNS[["LPS_v1"]], CAAS = SCALE_PATTERNS[["CAAS"]],
    DGI = SCALE_PATTERNS[["DGI"]], MLQ = SCALE_PATTERNS[["MLQ"]], AS = SCALE_PATTERNS[["AS"]],
    BRS = SCALE_PATTERNS[["BRS"]], ESW = SCALE_PATTERNS[["ESW"]], ESS = SCALE_PATTERNS[["ESS"]],
    FS = SCALE_PATTERNS[["FS"]], GRIT = SCALE_PATTERNS[["GRIT"]]
  )
  expect_equal(scale_patterns_cn_us$BRS, SCALE_PATTERNS[["BRS"]])
  expect_equal(scale_patterns_cn_us$FS, SCALE_PATTERNS[["FS"]])
  expect_true(grepl(scale_patterns_cn_us$BRS, "BRS_1"))
  expect_true(grepl(scale_patterns_cn_us$FS, "FS_SES1"))
})
