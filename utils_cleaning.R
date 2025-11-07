suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(stringi)
  library(haven)
  library(rlang)
  library(rstatix)
})

mk_step <- function(name,
                    fn,
                    exclude = NULL,
                    datasets = NULL) {
  list(
    type = "step",
    name = name,
    fn = fn,
    exclude = exclude,
    datasets = datasets
  )
}
mk_group <- function(name, steps) {
  stopifnot(is.list(steps))
  list(
    type = "group",
    name = name,
    steps = steps
  )
}

get_age_numeric <- function(df, var_age = "Age") {
  x <- df[[var_age]]
  # if (inherits(x, "haven_labelled")) return(haven::as_numeric(x))
  # suppressWarnings(as.numeric(x))
  as.numeric(x)
}

# get_gender_labels <- function(df, var_gender = "gender") {
#  haven::as_factor(df[[var_gender]], levels = "labels")
# }

# ---- minimal stats used in the wide summary ----
# .build_simple_stats <- function(df, var_age, var_gender, male_label, female_label) {
#  age_num <- get_age_numeric(df, var_age)
#  g_lab   <- get_gender_labels(df, var_gender)
#  tibble(
#    n          = nrow(df),
#    male       = sum(g_lab == male_label, na.rm = TRUE),
#    female     = sum(g_lab == female_label, na.rm = TRUE),
#    median_age = median(df$age, na.rm = TRUE)
#  )
# }

# reusable step factories
step_filter_age <- function(min_age = 18,
                            max_age = 65,
                            var_age = "Age",
                            keep_na = TRUE) {
  function(df) {
    age_num <- get_age_numeric(df, var_age)
    if (keep_na) {
      df[is.na(age_num) |
        (age_num >= min_age &
          age_num <= max_age), , drop = FALSE]
    } else {
      df[!is.na(age_num) &
        age_num >= min_age & age_num <= max_age, , drop = FALSE]
    }
  }
}

step_drop_na_block <- function(col_pattern = "^FTOS_v1_\\d+$") {
  force(col_pattern)
  function(df) {
    have <- grep(col_pattern, names(df), value = TRUE)
    if (length(have) < 1) {
      return(df)
    }

    message(sprintf("Block columns: %s", paste(have, collapse = ", ")))

    block <- lapply(df[have], function(x) {
      if (inherits(x, "haven_labelled")) {
        x <- haven::zap_labels(x)
      }
      as.character(x)
    })
    block <- as.data.frame(block, stringsAsFactors = FALSE)

    # keep if not *all* NA in the block
    keep <- rowSums(!is.na(block) & block != "") > 0
    df[keep, , drop = FALSE]
  }
}

normalize_chr <- function(v) {
  # keep TRUE NA as NA_character_, convert everything else to character
  out <- ifelse(is.na(v), NA_character_, as.character(v))
  # squash weird spaces: NBSP, ZWSP, BOM, etc.
  out <- trimws(out)
  # treat blanks and stringy NAs as missing
  out[out %in% c("", "NA", "<NA>")] <- NA_character_
  out
}


step_constant_answers <- function(col_pattern = "^FTOS_v1_\\d+$") {
  force(col_pattern)

  function(df) {
    have <- grep(col_pattern, names(df), value = TRUE)

    if (length(have) < 2) {
      # no columns found
      return(df)
    }

    block <- lapply(df[have], function(x) {
      if (inherits(x, "haven_labelled")) {
        x <- haven::zap_labels(x)
      }
      normalize_chr(x)
    })
    block <- as.data.frame(block, stringsAsFactors = FALSE)

    non_na <- rowSums(!is.na(block))
    all_same <- apply(block, 1L, function(r) {
      vals <- r[!is.na(r)]
      if (length(vals) < length(have)) {
        return(FALSE)
      } # too few answers → keep
      length(unique(vals)) == 1L # all answered values identical → drop
    })

    df[!all_same, , drop = FALSE]
  }
}

to_numeric <- function(x) {
  # Flatten list-columns
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  # Remove labels then coerce
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }
  suppressWarnings(as.numeric(x))
}

step_guttman <- function(scale_patterns,
                         z_thresh = 2,
                         max_fails = 2) {
  if (!is.list(scale_patterns) || is.null(names(scale_patterns))) {
    stop("`scale_patterns` must be a named list of regex patterns.")
  }
  function(df) {
    fail_flags <- data.frame(row.names = seq_len(nrow(df)))

    for (scale_name in names(scale_patterns)) {
      cols <- grep(scale_patterns[[scale_name]], names(df), value = TRUE)
      if (length(cols) < 2) {
        next
      }

      mat <- as.data.frame(lapply(df[cols], to_numeric), check.names = FALSE)

      # if everything NA after coercion, skip
      if (all(vapply(mat, function(v) {
        all(is.na(v))
      }, logical(1)))) {
        fail_flags[[scale_name]] <- 0L
        next
      }

      rng <- range(unlist(mat), na.rm = TRUE)
      if (!all(is.finite(rng))) {
        fail_flags[[scale_name]] <- 0L
        next
      }
      v_min <- rng[1]
      v_max <- rng[2]
      Ncat <- as.integer(v_max - v_min + 1L)
      if (!is.finite(Ncat) ||
        Ncat < 2L) {
        fail_flags[[scale_name]] <- 0L
        next
      }

      mat0 <- as.data.frame(lapply(mat, function(v) {
        v <- v - v_min
        as.integer(round(v))
      }), check.names = FALSE)

      gfit <- tryCatch(
        PerFit::Gpoly(as.matrix(mat0), Ncat = Ncat),
        error = function(e) {
          NULL
        }
      )
      if (is.null(gfit)) {
        fail_flags[[scale_name]] <- 0L
        next
      }

      g <- as.numeric(gfit[["PFscores"]][["PFscores"]])
      z <- as.numeric(scale(g))
      fail_flags[[scale_name]] <- ifelse(is.na(z) |
        abs(z) <= z_thresh, 0L, 1L)
    }

    if (ncol(fail_flags) == 0L) {
      return(df)
    }
    total_fails <- rowSums(fail_flags, na.rm = TRUE)
    keep_rows <- total_fails < max_fails
    # message(sprintf(
    #  "Guttman Filter: Removed %d rows. %d rows remain.",
    #  sum(!keep_rows),
    #  sum(keep_rows)
    # ))
    df[keep_rows, , drop = FALSE]
  }
}

step_mahalanobis <- function(scale_patterns, max_fails = 2) {
  if (!is.list(scale_patterns) || is.null(names(scale_patterns))) {
    stop("`scale_patterns` must be a named list of regex patterns.")
  }
  function(df) {
    fail_flags <- data.frame(row.names = seq_len(nrow(df)))

    for (scale_name in names(scale_patterns)) {
      cols <- grep(scale_patterns[[scale_name]], names(df), value = TRUE)
      if (length(cols) < 2) {
        next
      }

      block <- as.data.frame(lapply(df[cols], to_numeric), check.names = FALSE)

      # if block is all NA/constant, skip
      all_na <- vapply(block, function(v) {
        all(is.na(v))
      }, logical(1))
      if (all(all_na)) {
        fail_flags[[scale_name]] <- 0L
        next
      }

      # drop all-NA columns to avoid singular cov
      block <- block[, !all_na, drop = FALSE]
      if (ncol(block) < 2) {
        fail_flags[[scale_name]] <- 0L
        next
      }

      res <- rstatix::mahalanobis_distance(block)
      fail_flags[[scale_name]] <- as.integer(res$is.outlier)
    }

    if (ncol(fail_flags) == 0L) {
      return(df)
    }
    total_fails <- rowSums(fail_flags, na.rm = TRUE)
    keep_rows <- total_fails < max_fails
    # message(sprintf(
    #  "Mahalanobis Filter: Removed %d rows. %d rows remain.",
    #  sum(!keep_rows),
    #  sum(keep_rows)
    # ))
    df[keep_rows, , drop = FALSE]
  }
}


step_detect_zigzag <- function(col_pattern = "^FTOS_v1_\\d+$",
                               require_adjacent = TRUE) {
  function(df) {
    dataset_name <- if ("id" %in% names(df) && nrow(df) > 0) {
      as.character(df$id[1])
    } else {
      "unknown"
    }

    have <- grep(col_pattern, names(df), value = TRUE)
    if (length(have) < 2) {
      message(sprintf("[%s] Zigzag Detection [%s]: Scale not detected (fewer than 2 columns found)", dataset_name, col_pattern))
      return(df)
    }
    message(sprintf("[%s] Zigzag Detection [%s]: Scale detected with %d columns", dataset_name, col_pattern, length(have)))

    ord <- order(suppressWarnings(as.integer(sub(
      ".*_(\\d+)$", "\\1", have
    ))))
    cols <- have[ord]

    block <- lapply(df[cols], function(x) {
      if (inherits(x, "haven_labelled")) {
        as.numeric(haven::zap_labels(x))
      } else {
        suppressWarnings(as.numeric(x))
      }
    })
    mat <- do.call(cbind, block)

    is_zigzag <- apply(mat, 1, function(row) {
      v <- row[!is.na(row)]
      n <- length(v)
      if (n < length(have)) {
        return(FALSE)
      }

      u <- unique(v)
      if (length(u) != 2) {
        return(FALSE)
      }

      if (require_adjacent) {
        if (abs(diff(sort(u))) != 1) {
          return(FALSE)
        }
      }

      alt1 <- rep(u, length.out = n)
      alt2 <- rep(rev(u), length.out = n)
      all(v == alt1) || all(v == alt2)
    })

    n_removed <- sum(is_zigzag)
    message(sprintf(
      "[%s] Zigzag Detection [%s]: Removed %d participants (%d remain)",
      dataset_name, col_pattern, n_removed, nrow(df) - n_removed
    ))

    df[!is_zigzag, , drop = FALSE]
  }
}

step_check_control <- function(col = "FTOS_x") {
  function(df) {
    if (!(col %in% names(df))) {
      # skip
      return(df)
    }

    x <- df[[col]]

    codes <- if (inherits(x, "haven_labelled")) {
      as.numeric(haven::zap_labels(x))
    } else {
      suppressWarnings(as.numeric(x))
    }

    if (length(codes) == 0 || all(is.na(codes))) {
      message(sprintf("Column '%s' is all NA (or empty); skipping.", col))
      return(df)
    }

    freq_table <- sort(table(codes, useNA = "no"), decreasing = TRUE)

    keep_code_chr <- names(freq_table)[1]

    keep_code_num <- as.numeric(keep_code_chr)

    message(paste0("Removed control value: "), keep_code_num)

    df[codes == keep_code_num | is.na(codes), , drop = FALSE]
  }
}

step_remove_foreigners <- function(col = "Nationality", code = 46) {
  function(df) {
    if (!(col %in% names(df))) {
      message(sprintf("Column '%s' not found; skipping.", col))
      return(df)
    }
    x <- df[[col]]
    codes <- if (inherits(x, "haven_labelled")) {
      as.numeric(haven::zap_labels(x))
    } else {
      suppressWarnings(as.numeric(x))
    }
    df[codes == as.numeric(code), , drop = FALSE]
  }
}

step_filter_min_duration <- function(start_col = "startdate",
                                     end_col = "submitdate",
                                     threshold = 600,
                                     units = "secs") {
  function(df) {
    if (!(start_col %in% names(df)) || !(end_col %in% names(df))) {
      warning(
        sprintf(
          "step_filter_min_duration: columns '%s' or '%s' not found, skipping.",
          start_col,
          end_col
        )
      )
      return(df)
    }

    start <- as.POSIXct(df[[start_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    end <- as.POSIXct(df[[end_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    dur <- as.numeric(difftime(end, start, units = units))

    df[!is.na(dur) & dur >= threshold, , drop = FALSE]
  }
}


# --- Step runner (applies a single step and logs it) ---
apply_step <- function(df, name, fn) {
  new_df <- fn(df)
  tibble(
    step = name,
    removed = nrow(df) - nrow(new_df),
    remaining = nrow(new_df)
  ) |>
    list(df = new_df, audit = _)
}

.matches <- function(name, patterns) {
  if (is.null(patterns) || length(patterns) == 0) {
    return(FALSE)
  }
  any(vapply(patterns, function(p) {
    grepl(p, name, perl = TRUE)
  }, logical(1)))
}

.should_run_step <- function(s, dataset_name) {
  if (.matches(dataset_name, s$exclude)) {
    return(FALSE)
  }
  if (!is.null(s$datasets)) {
    return(.matches(dataset_name, s$datasets))
  }
  TRUE
}


run_cleaning_pipeline <- function(df, dataset_name, steps) {
  audit <- tibble(
    step = character(),
    removed = integer(),
    remaining = integer()
  )
  cur <- df

  run_one <- function(s, prefix = NULL) {
    # normalize: bare function becomes a named step
    if (is.function(s)) {
      s <- mk_step(deparse(substitute(s)), s)
    }

    if (identical(s$type, "group")) {
      group_name <- if (is.null(prefix)) {
        s$name
      } else {
        paste0(prefix, " ▸ ", s$name)
      }
      start_n <- nrow(cur)
      ran <- 0L
      skipped <- 0L

      for (sub in s$steps) {
        sub_named <- if (is.function(sub)) {
          mk_step(deparse(substitute(sub)), sub)
        } else {
          sub
        }
        if (!.should_run_step(sub_named, dataset_name)) {
          skipped <- skipped + 1L
          next
        }
        ran <- ran + 1L
        cur <<- sub_named$fn(cur)
      }
      removed_total <- start_n - nrow(cur)
      audit <<- dplyr::bind_rows(
        audit,
        tibble::tibble(
          step = sprintf("%s [group: %d ran, %d skipped]", group_name, ran, skipped),
          removed = removed_total,
          remaining = nrow(cur)
        )
      )
      return(invisible(NULL))
    }

    # single step with its own gating
    step_name <- if (is.null(prefix)) {
      s$name
    } else {
      paste0(prefix, " ▸ ", s$name)
    }
    if (!.should_run_step(s, dataset_name)) {
      audit <<- dplyr::bind_rows(
        audit,
        tibble::tibble(
          step = paste0(step_name, " [skipped]"),
          removed = 0L,
          remaining = nrow(cur)
        )
      )
      return(invisible(NULL))
    }
    res <- apply_step(cur, step_name, s$fn)
    cur <<- res$df
    audit <<- dplyr::bind_rows(audit, res$audit)
    invisible(NULL)
  }

  for (s in steps) {
    run_one(s)
  }

  list(df_clean = cur, audit = audit)
}

build_wide_summary <- function(name,
                               df_initial,
                               df_final,
                               audit,
                               var_age = "Age") {
  # init <- .build_simple_stats(df_initial, var_age, var_gender, male_label, female_label)
  # fin  <- .build_simple_stats(df_final,   var_age, var_gender, male_label, female_label)

  fields <- list(name = name, initial_n = nrow(df_initial))
  # initial_male       = dplyr::pull(init, male),
  # initial_female     = dplyr::pull(init, female),
  # initial_median_age = dplyr::pull(init, median_age))

  if (nrow(audit) > 0) {
    for (i in seq_len(nrow(audit))) {
      fields[[paste0("step_", i, "_description")]] <- audit$step[i]
      fields[[paste0("step_", i, "_removed")]] <- audit$removed[i]
    }
  }

  fields[["final_n"]] <- nrow(df_final) # dplyr::pull(fin, n)
  total_removed <- nrow(df_initial) - nrow(df_final)
  fields[["percentage_removed"]] <- 100 * total_removed / nrow(df_initial)
  # fields[["final_male"]]       <- dplyr::pull(fin, male)
  # fields[["final_female"]]     <- dplyr::pull(fin, female)
  # fields[["final_median_age"]] <- dplyr::pull(fin, median_age)

  tibble::as_tibble(fields)
}
