test_that("collect_dataset_tokens walks nested mk_group/mk_step structures and bare lists", {
  fake_steps <- list(
    mk_group("g1", steps = list(
      mk_step("s1", identity, datasets = c("A", "B")),
      mk_step("s2", identity, exclude = "C")
    )),
    list(name = "bare_step", fn = identity, datasets = "D") # e.g. short-duration/us_external filters
  )
  tokens <- collect_dataset_tokens(fake_steps)
  expect_setequal(tokens, c("A", "B", "C", "D"))
})

test_that("assert_datasets_exist warns when a token matches nothing", {
  expect_warning(
    assert_datasets_exist(c("A", "MISSING"), c("A_clean", "B_clean"), context = "unit-test", fixed = TRUE),
    "MISSING"
  )
})

test_that("assert_datasets_exist is silent when every token matches", {
  expect_no_warning(
    assert_datasets_exist(c("A", "B"), c("A_clean", "B_clean"), context = "unit-test", fixed = TRUE)
  )
})

test_that("assert_datasets_exist regex mode (fixed = FALSE) matches like .matches() in cleaning_functions.R", {
  expect_no_warning(
    assert_datasets_exist("^IT_AUTO$", c("IT_AUTO", "CN_277273"), context = "unit-test", fixed = FALSE)
  )
  expect_warning(
    assert_datasets_exist("^ZZ_AUTO$", c("IT_AUTO", "CN_277273"), context = "unit-test", fixed = FALSE),
    "ZZ_AUTO"
  )
})

test_that("real DATASETS groupings in config/paths.R are internally non-empty (sanity check)", {
  # Not a full coverage check (that needs real split/clean files) - just
  # guards against an empty/typo'd DATASETS list silently passing everything.
  for (key in c("brpt", "cn", "us", "cn_us_10_min", "pilot", "first_stage", "first_stage_brpt", "datasets_to_remove")) {
    expect_true(key %in% names(DATASETS), info = paste("DATASETS should have a", key, "entry"))
    expect_gt(length(DATASETS[[key]]), 0)
  }
})
