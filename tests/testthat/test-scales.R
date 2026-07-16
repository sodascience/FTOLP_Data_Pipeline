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

test_that("every col_pattern in both scale tables is a valid, non-empty regex", {
  all_patterns <- c(CONSTANT_ANSWER_SCALES$col_pattern, ZIGZAG_SCALES$col_pattern)
  for (p in all_patterns) {
    expect_true(nchar(p) > 0)
    expect_no_error(grepl(p, "dummy_column_name"))
  }
})
