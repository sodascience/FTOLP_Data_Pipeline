library(dplyr)
library(purrr)
library(haven)
library(labelled)

# missing values codes
reason_codes <- c(
  "by_design"       = 990,
  "unknown_missing" = 991,
  "tech_error"      = 992,
  "refused"         = 993,
  "dubious"         = 994,
  "nonresponse"     = 995,
  "not_applicable"  = 999
)

# get label schema
get_schema <- function(df, cols = names(df)) {
  setNames(lapply(cols, function(col) {
    x <- df[[col]]
    list(
      name       = col,
      var_label  = attr(x, "label", exact = TRUE),
      val_labels = tryCatch(labelled::val_labels(x), error = function(e) NULL),
      na_values  = tryCatch(labelled::na_values(x), error = function(e) NULL),
      na_range   = tryCatch(labelled::na_range(x), error = function(e) NULL),
      class      = class(x)
    )
  }), cols)
}

apply_schema_from <- function(x_target, x_source) {
  src_labels <- labelled::val_labels(x_source)
  out <- labelled_spss(
    suppressWarnings(as.numeric(x_target)),
    labels = src_labels,
    na_values = labelled::na_values(x_source),
    na_range = labelled::na_range(x_source)
  )
  attr(out, "label") <- attr(x_source, "label", exact = TRUE)
  out
}


# make sure reason codes are in val_labels
augment_with_reasons <- function(val_labels) {
  if (is.null(val_labels)) val_labels <- c()
  # don’t duplicate labels if user already has a 9xx label
  add <- reason_codes[setdiff(names(reason_codes), names(val_labels))]
  c(val_labels, add)
}

coerce_column_to_schema <- function(x, meta, nrows, fill_code_if_missing = reason_codes[["by_design"]],
                                    na_to_nonresponse = FALSE) {
  val_labs <- augment_with_reasons(meta$val_labels)
  # We standardize on labelled_spss + na_range 990–999
  # (SPSS will treat all 990–999 as user-missing but keep the labels.)

  if (missing(x) || is.null(x)) {
    # column absent -> all "by design"
    v <- rep(fill_code_if_missing, nrows)
    v <- labelled_spss(v, labels = val_labs, na_range = c(990, 999))
    attr(v, "label") <- meta$var_label
    return(v)
  }

  # Column exists: coerce to numeric, preserve observed values
  # (If it's already labelled_spss, val_labels will be reattached/merged.)
  y <- suppressWarnings(as.numeric(x))

  if (na_to_nonresponse) {
    # Make blank/system-NA explicit as 999
    y[is.na(y)] <- reason_codes[["nonresponse"]]
  }

  y <- labelled_spss(y, labels = val_labs, na_range = c(990, 999))
  attr(y, "label") <- meta$var_label
  y
}

# --- 4) Coerce an entire df to a schema (add missing columns, align labels) ---
coerce_df_to_schema <- function(df, schema, na_to_nonresponse = FALSE) {
  n <- nrow(df)
  out <- lapply(schema, function(meta) {
    col <- meta$name
    exists <- col %in% names(df)
    x <- if (exists) df[[col]] else NULL
    coerce_column_to_schema(x, meta, nrows = n, na_to_nonresponse = na_to_nonresponse)
  })
  as_tibble(out)
}

bind_to_general <- function(dfs, reference_df_name = NULL, na_to_nonresponse = FALSE) {
  # Pick a reference for schema: if provided, use it; else pick the df with the most columns
  if (!is.null(reference_df_name)) {
    ref <- dfs[[reference_df_name]]
  } else {
    sizes <- sapply(dfs, function(x) ncol(x))
    ref <- dfs[[which.max(sizes)]]
  }

  # If B/D/E share the canonical structure, using B (or any of B/D/E) is fine:
  schema <- get_schema(ref)

  # Coerce each df to the schema
  coerced <- imap(dfs, ~ coerce_df_to_schema(.x, schema, na_to_nonresponse = na_to_nonresponse) %>%
    mutate(.source = .y)) # optional provenance column

  # Row-bind (all have same columns now)
  bind_rows(coerced)
}

check_same_schema <- function(...) {
  lst <- list(...)
  nms <- lapply(lst, names)
  stopifnot(length(unique(nms)) == 1) # same names

  # (Light check) compare variable labels for the first df vs others
  lab <- function(df) sapply(df, attr, which = "label", exact = TRUE)
  ref <- lab(lst[[1]])
  for (k in 2:length(lst)) {
    ok <- identical(ref, lab(lst[[k]]))
    if (!ok) warning(sprintf("Variable labels differ for dataset %d", k))
  }
}
