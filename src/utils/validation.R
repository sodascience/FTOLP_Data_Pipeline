# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# validation.R - Loud-failure checks for dataset-name matching
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
# PURPOSE: Catch silent mismatches between configured dataset-name tokens
#          (DATASETS groupings from config/paths.R, or hardcoded literals like
#          "IT_AUTO" scattered through the pipeline scripts) and the datasets
#          that actually exist at runtime.
#
# WHY THIS EXISTS: A token that matches nothing is a bug that otherwise fails
#          silently - a filter quietly never applies to anyone, a branch
#          quietly never fires - rather than raising an error. This exact
#          failure mode has bitten this pipeline more than once: the
#          DATASETS$ch / DATASETS$cn key mismatch in 02_clean.R, the dead
#          "EN" branch in 03_merge.R's get_citizen_pattern(), and the
#          br_pilot / BR_PILOT case mismatch across all three scripts all
#          would have been caught immediately by the checks below.
# ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

# Warn if any token in `tokens` matches none of `known_names`.
#
# `fixed` controls the matching semantics and should match whatever the
# calling script actually uses to test these same tokens against dataset
# names:
#   - fixed = FALSE (default): tokens are treated as regex patterns, tested
#     with grepl(token, name, perl = TRUE) - matches cleaning_functions.R's
#     own .matches() used to gate mk_step()/mk_group().
#   - fixed = TRUE: tokens are treated as literal substrings, tested with
#     grepl(token, name, fixed = TRUE) - matches 03_merge.R's
#     is_country_nationality() / get_citizen_pattern() matching style.
assert_datasets_exist <- function(tokens, known_names, context, fixed = FALSE) {
  tokens <- unique(tokens)
  if (length(tokens) == 0) {
    return(invisible(character(0)))
  }

  matched <- vapply(tokens, function(tok) {
    any(grepl(tok, known_names, fixed = fixed, perl = !fixed))
  }, logical(1))

  missing <- tokens[!matched]

  if (length(missing) > 0) {
    warning(
      sprintf(
        "[%s] The following dataset token(s) matched NO dataset in this run:\n  %s\n  Known datasets (%d): %s",
        context,
        paste(missing, collapse = ", "),
        length(known_names),
        paste(sort(known_names), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(missing)
}
