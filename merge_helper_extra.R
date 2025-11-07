library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(readxl)
library(lubridate)

setwd("~/surfdrive/Narrating the Future (Bogdan)/LimeSurvey Processed")

df1 <- read_sav("US_868141.sav")
df2 <- read_sav("US_216254.sav")

compare_dfs_compact <- function(x, y, ignore_order = FALSE, na_equal = TRUE, short_circuit = TRUE) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  cls_str <- function(z) paste(class(z), collapse = "|")
  get_varlab <- function(v) { lb <- attr(v, "label", exact = TRUE); if (is.null(lb)) NA_character_ else as.character(lb) }
  
  x_names <- names(x)
  y_names <- names(y)
  
  # 1) ncol
  ncol_equal <- ncol(x) == ncol(y)
  if (short_circuit && !ncol_equal) {
    return(list(
      ok = FALSE,
      ncol_equal = FALSE,
      ncol_x = ncol(x),
      ncol_y = ncol(y),
      names_equal = NA,
      missing_in_y = character(),
      extra_in_y = character(),
      order_equal = NA,
      type_mismatch = character(),
      var_label_mismatch = character(),
      value_label_mismatch = character()
    ))
  }
  
  # 2) names
  if (ignore_order) {
    names_equal <- setequal(x_names, y_names)
    missing_in_y <- setdiff(x_names, y_names)
    extra_in_y   <- setdiff(y_names, x_names)
    order_equal  <- NA
  } else {
    names_equal <- identical(x_names, y_names)
    missing_in_y <- setdiff(x_names, y_names)
    extra_in_y   <- setdiff(y_names, x_names)
    order_equal  <- if (setequal(x_names, y_names)) names_equal else NA
  }
  
  common <- intersect(x_names, y_names)
  x_common <- x[common]
  y_common <- y[common]
  
  if (short_circuit && (!names_equal && (length(missing_in_y) + length(extra_in_y) > 0))) {
    return(list(
      ok = FALSE,
      ncol_equal = ncol_equal,
      ncol_x = ncol(x),
      ncol_y = ncol(y),
      names_equal = FALSE,
      missing_in_y = missing_in_y,
      extra_in_y = extra_in_y,
      order_equal = order_equal,
      type_mismatch = character(),
      var_label_mismatch = character(),
      value_label_mismatch = character()
    ))
  }
  
  # 3) types
  type_mismatch <- vapply(common, function(nm) {
    cx <- cls_str(x_common[[nm]]); cy <- cls_str(y_common[[nm]])
    if (identical(cx, cy)) "" else sprintf("%s: x<%s> vs y<%s>", nm, cx, cy)
  }, character(1))
  type_mismatch <- type_mismatch[type_mismatch != ""]
  
  # 4) variable labels
  var_label_mismatch <- vapply(common, function(nm) {
    vx <- get_varlab(x_common[[nm]])
    vy <- get_varlab(y_common[[nm]])
    same <- if (na_equal) (identical(vx, vy) || (is.na(vx) && is.na(vy))) else identical(vx, vy)
    if (isTRUE(same)) "" else nm
  }, character(1))
  var_label_mismatch <- var_label_mismatch[var_label_mismatch != ""]
  
  # 5) value labels / levels
  value_label_mismatch <- vapply(common, function(nm) {
    vx <- x_common[[nm]]; vy <- y_common[[nm]]
    is_lbl_x <- inherits(vx, "haven_labelled") || inherits(vx, "labelled") || inherits(vx, "labelled_spss")
    is_lbl_y <- inherits(vy, "haven_labelled") || inherits(vy, "labelled") || inherits(vy, "labelled_spss")
    
    if (is.factor(vx) && is.factor(vy)) {
      if (identical(levels(vx), levels(vy))) "" else nm
    } else if (is_lbl_x && is_lbl_y) {
      nx <- attr(vx, "labels", TRUE); ny <- attr(vy, "labels", TRUE)
      norm <- function(z) {
        if (is.null(z)) return(NA_character_)
        paste(sprintf("%s=%s", names(z), as.character(unname(z))), collapse = ";")
      }
      sx <- norm(nx); sy <- norm(ny)
      same <- if (na_equal) (identical(sx, sy) || (is.na(sx) && is.na(sy))) else identical(sx, sy)
      if (same) "" else nm
    } else {
      if ((is.factor(vx) != is.factor(vy)) || (is_lbl_x != is_lbl_y)) nm else ""
    }
  }, character(1))
  value_label_mismatch <- value_label_mismatch[value_label_mismatch != ""]
  
  # Correct R version of the ternary:
  names_ok <- if (ignore_order) setequal(x_names, y_names) else identical(x_names, y_names)
  
  ok <- ncol_equal &&
    names_ok &&
    length(type_mismatch) == 0 &&
    length(var_label_mismatch) == 0 &&
    length(value_label_mismatch) == 0
  
  list(
    ok = ok,
    ncol_equal = ncol_equal,
    ncol_x = ncol(x),
    ncol_y = ncol(y),
    names_equal = names_ok,
    missing_in_y = missing_in_y,
    extra_in_y = extra_in_y,
    order_equal = order_equal,
    type_mismatch = unname(type_mismatch),
    var_label_mismatch = unname(var_label_mismatch),
    value_label_mismatch = unname(value_label_mismatch)
  )
}


print_compare_summary <- function(res) {
  if (res$ok) {
    cat("Data frames match.\n")
    return(invisible(NULL))
  }
  if (!res$ncol_equal) {
    cat(sprintf("col mismatch: x=%d, y=%d\n", res$ncol_x, res$ncol_y))
    return(invisible(NULL))  # early exit; further diffs are noisy
  }
  if (!isTRUE(res$names_equal)) {
    if (length(res$missing_in_y)) cat("in x not y: ", paste(res$missing_in_y, collapse=", "), "\n", sep="")
    if (length(res$extra_in_y))   cat("in y not x: ", paste(res$extra_in_y, collapse=", "), "\n", sep="")
    if (!is.na(res$order_equal) && !res$order_equal) cat("same set but different order.\n")
    return(invisible(NULL))  # early exit by default
  }
  if (length(res$type_mismatch)) {
    cat("type mismatches:\n  ", paste(res$type_mismatch, collapse="\n  "), "\n", sep="")
  }
  if (length(res$var_label_mismatch)) {
    cat("variable label mismatches (attr 'label'):\n  ",
        paste(res$var_label_mismatch, collapse=", "), "\n", sep="")
  }
  if (length(res$value_label_mismatch)) {
    cat("value label mismatches (factor levels / haven labels):\n  ",
        paste(res$value_label_mismatch, collapse=", "), "\n", sep="")
  }
}

# compare_dfs(df1, df2)
# 
# df_US <- bind_rows(df1, df2)
# 
# df_US_filtered <- df_US %>%
#   filter(Age >= 85 | Nationality != 46)
# 
# write_sav(df_US_filtered, "US_filtered.sav")

summarize_overlap <- function(x, y, max_show = 25) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  xn <- names(x); yn <- names(y)
  common   <- intersect(xn, yn)
  only_x   <- setdiff(xn, yn)
  only_y   <- setdiff(yn, xn)
  
  cat(sprintf("common: %d | only_in_x: %d | only_in_y: %d\n",
              length(common), length(only_x), length(only_y)))
  
  if (length(only_x))
    cat("only_in_x: ", paste(utils::head(only_x, max_show), collapse=", "),
        if (length(only_x) > max_show) " ...", "\n", sep = "")
  if (length(only_y))
    cat("only_in_y: ", paste(utils::head(only_y, max_show), collapse=", "),
        if (length(only_y) > max_show) " ...", "\n", sep = "")
  invisible(list(common = common, only_in_x = only_x, only_in_y = only_y))
}

project_common <- function(x, y, ignore_order = TRUE) {
  xn <- names(x); yn <- names(y)
  common <- if (ignore_order) sort(intersect(xn, yn)) else intersect(xn, yn)
  list(
    x_common = x[common],
    y_common = y[common],
    common   = common
  )
}


align_to_union <- function(x, y) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  all_names <- union(names(x), names(y))
  
  na_like <- function(proto, n) {
    if (inherits(proto, "haven_labelled") || inherits(proto, "labelled") || inherits(proto, "labelled_spss")) {
      # keep labels & label attr
      lab  <- attr(proto, "labels", TRUE)
      lb   <- attr(proto, "label",  TRUE)
      out  <- rep(NA_real_, n)
      class(out) <- class(proto)
      attr(out, "labels") <- lab
      attr(out, "label")  <- lb
      return(out)
    } else if (is.factor(proto)) {
      return(factor(rep(NA_character_, n), levels = levels(proto)))
    } else if (is.character(proto)) {
      return(rep(NA_character_, n))
    } else if (is.integer(proto)) {
      return(rep(NA_integer_, n))
    } else if (is.numeric(proto)) {
      return(rep(NA_real_, n))
    } else if (is.logical(proto)) {
      return(rep(NA, n))
    } else {
      # fallback: plain NA
      return(rep(NA, n))
    }
  }
  
  add_missing_cols <- function(df, template_df) {
    miss <- setdiff(names(template_df), names(df))
    if (length(miss)) {
      for (nm in miss) {
        df[[nm]] <- na_like(template_df[[nm]], nrow(df))
      }
    }
    df[all_names]  # order columns by union order
  }
  
  x2 <- add_missing_cols(x, y)
  y2 <- add_missing_cols(y, x)
  
  list(x_aligned = x2, y_aligned = y2,
       only_in_x = setdiff(names(x), names(y)),
       only_in_y = setdiff(names(y), names(x)),
       common    = intersect(names(x), names(y)))
}

can_safely_overlap <- function(x, y, check_levels = TRUE, check_haven_labels = TRUE) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  common <- intersect(names(x), names(y))
  
  same_class <- vapply(common, function(nm) {
    identical(class(x[[nm]]), class(y[[nm]]))
  }, logical(1))
  
  lvl_ok <- vapply(common, function(nm) {
    vx <- x[[nm]]; vy <- y[[nm]]
    if (!(is.factor(vx) && is.factor(vy))) return(TRUE)
    identical(levels(vx), levels(vy))
  }, logical(1))
  
  haven_ok <- vapply(common, function(nm) {
    vx <- x[[nm]]; vy <- y[[nm]]
    is_lbl_x <- inherits(vx, "haven_labelled") || inherits(vx, "labelled") || inherits(vx, "labelled_spss")
    is_lbl_y <- inherits(vy, "haven_labelled") || inherits(vy, "labelled") || inherits(vy, "labelled_spss")
    if (!(is_lbl_x || is_lbl_y)) return(TRUE)
    if (!(is_lbl_x && is_lbl_y)) return(FALSE)
    lx <- attr(vx, "labels", TRUE); ly <- attr(vy, "labels", TRUE)
    # normalize to comparable string
    norm <- function(z) if (is.null(z)) NA_character_ else paste(sprintf("%s=%s", names(z), as.character(unname(z))), collapse=";")
    identical(norm(lx), norm(ly))
  }, logical(1))
  
  ok_vec <- same_class & (!check_levels | lvl_ok) & (!check_haven_labels | haven_ok)
  list(
    ok = all(ok_vec),
    bad_cols = common[!ok_vec],
    details = data.frame(
      column = common,
      same_class = same_class,
      levels_match = lvl_ok,
      haven_labels_match = haven_ok,
      stringsAsFactors = FALSE
    )
  )
}



df1 <- read_sav("IT_277273.sav")
df2 <- read_sav("IT_824323.sav")
df3 <- read_sav("IT_999625.sav")

res <- compare_dfs_compact(df1, df2, ignore_order = TRUE)
print_compare_summary(res)
summarize_overlap(df1, df2)


#compare_dfs(df1, df2)
#compare_dfs(df2, df3)




