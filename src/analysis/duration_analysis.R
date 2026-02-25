library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(labelled)
library(tools)
library(here)

# Load configuration
source(here::here("config", "paths.R"))

# Note: Using absolute paths from config

# Dataset grouping
first_stage_df <- DATASETS$first_stage_df


parse_time <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S", tz = tz)
}


# duration helper
analyze_duration_histograms <- function(df, dfname, unit = c("mins", "secs", "hours"),
                                        save_dir = NULL, bins = 50) {
  unit <- match.arg(unit)
  unit_factor <- switch(unit,
    secs  = 1,
    mins  = 60,
    hours = 3600
  )

  # 1) compute duration
  df2 <- df %>%
    mutate(
      start_dt = parse_time(startdate, tz = "UTC"),
      end_dt = parse_time(datestamp, tz = "UTC"),
      duration_s = as.numeric(difftime(end_dt, start_dt, units = "secs")),
      duration_u = duration_s / unit_factor
    ) %>%
    filter(!is.na(duration_u), duration_u >= 0) # drop junk

  if (!"lastpage" %in% names(df2)) {
    stop("Column `lastpage` not found in data.")
  }

  # 2) robust stats + outlier labelling per lastpage (MAD z-scores)
  stats <- df2 %>%
    group_by(lastpage) %>%
    summarize(
      n = dplyr::n(),
      median_d = median(duration_u, na.rm = TRUE),
      mad_d = mad(duration_u, constant = 1.4826, na.rm = TRUE), # consistent MAD
      .groups = "drop"
    )

  df3 <- df2 %>%
    left_join(stats, by = "lastpage") %>%
    mutate(
      robust_z = ifelse(mad_d > 0, (duration_u - median_d) / mad_d, NA_real_),
      is_outlier = !is.na(robust_z) & robust_z < -1
    )


  # 3) summary table of outliers by lastpage
  outlier_summary <- df3 %>%
    group_by(lastpage) %>%
    summarize(
      n = dplyr::n(),
      outliers = sum(is_outlier, na.rm = TRUE),
      pct_outliers = round(100 * outliers / n, 2),
      median_duration = median(duration_u, na.rm = TRUE),
      mad_duration = unique(mad_d),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_outliers), desc(outliers))

  # 4) faceted histogram with median & ±3*MAD lines
  # free_x so each section scales nicely
  vlines <- stats %>%
    mutate(
      lo = median_d - 3 * mad_d,
      hi = median_d + 3 * mad_d
    )

  p <- ggplot(df3, aes(x = duration_u)) +
    geom_histogram(bins = bins, linewidth = 0.2) +
    geom_vline(data = vlines, aes(xintercept = median_d), linetype = "dashed") +
    geom_vline(data = vlines, aes(xintercept = lo)) +
    geom_vline(data = vlines, aes(xintercept = hi)) +
    facet_wrap(~lastpage, scales = "free_x") +
    labs(
      x = paste("Duration (", unit, ")", sep = ""),
      y = "Count",
      title = "Duration Histograms by Last Page (median and ±3×MAD shown)"
    ) +
    theme_minimal(base_size = 12)

  if (!is.null(save_dir)) {
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    unique_pages <- sort(unique(df3$lastpage))
    for (pg in unique_pages) {
      df_pg <- df3 %>% filter(lastpage == pg)
      st_pg <- vlines %>% filter(lastpage == pg)
      p_pg <- ggplot(df_pg, aes(x = duration_u)) +
        geom_histogram(bins = bins, fill = "lightblue", color = "white", linewidth = 0.2) +
        geom_vline(data = st_pg, aes(xintercept = median_d), linetype = "dashed") +
        geom_vline(data = st_pg, aes(xintercept = lo)) +
        geom_vline(data = st_pg, aes(xintercept = hi)) +
        labs(
          x = paste("Duration (", unit, ")", sep = ""), y = "Count",
          title = paste0("Duration – lastpage = ", pg)
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          strip.background = element_rect(fill = "white", color = NA)
        )

      ggsave(file.path(save_dir, paste0(dfname, "_duration_lastpage_", pg, ".png")),
        plot = p_pg, width = 7, height = 4.5, dpi = 150, bg = "white"
      )
    }
  }

  list(plot = p, outlier_summary = outlier_summary, data = df3)
}

for (f in first_stage_df) {
  # f<- "BR_PT_277273_clean.sav"
  df <- read_sav(paste0(f, "_clean.sav"))
  res <- analyze_duration_histograms(df, f, unit = "mins", save_dir = "duration_plots")
  print(res$plot)
  write_csv(res$outlier_summary, file.path("duration_plots", paste0(f, "_table.csv")))
  View(res$outlier_summary)
}
