## =============================================================================
## R/07_demographics.R
## Optional demographics module.
##
## Requires:   demographics.csv  in the study data folder
##             (set via STUDY_DATA_PATH env var or config$data$path)
## Activated:  demographics.generate: true  in study_config.yml
## Columns:    controlled by demographics.columns.* in config (see below).
##             Column names that don't exist are silently skipped.
##
## Outputs (all into descriptive/):
##   Tables  — D1_sample_characteristics, D2_sequence_allocation
##   Figures — D1_age_distribution, D2_grouping_var_distribution,
##             D3_training_level_distribution
##
## Optional SF-36 module:
##   Activated: sf36.generate: true  in config
##   Requires:  sf36_scores.csv  in the study data folder
##   Outputs — D3_sf36_domain_scores (table) + D4_sf36_profile (figure)
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

.script_dir <- local({
  d <- Sys.getenv("R_SCRIPTS_DIR", unset = "")
  if (nzchar(d)) return(d)
  for (i in rev(seq_along(sys.frames()))) {
    f <- sys.frames()[[i]]$ofile
    if (!is.null(f) && nzchar(f))
      return(dirname(normalizePath(f, winslash = "/")))
  }
  normalizePath("R", winslash = "/")
})
source(file.path(.script_dir, "00_setup.R"))
log_h1("07  DEMOGRAPHICS")

# Wrap in local() so return() acts as a safe early exit when sourced by run_all.R
# (quit() would kill the entire R session; return() from local() does not)
local({

cfg <- read_config()
dat <- load_rds("analysis_data")

# --- Check if demographics module is enabled ---
if (!isTRUE(as.logical(cfg$demographics$generate %||% FALSE))) {
  log_line("Demographics module disabled (demographics.generate = false). Skipping.")
  return(invisible(NULL))
}

# --- Resolve demographics.csv path ---
data_dir <- Sys.getenv("STUDY_DATA_PATH",
              unset = cfg$data$path %||% file.path(getwd(), "study_data"))
demo_file <- file.path(data_dir, cfg$demographics$file %||% "demographics.csv")

if (!file.exists(demo_file)) {
  log_warn(paste0("Demographics file not found: ", demo_file, "  — skipping module."))
  return(invisible(NULL))
}

demo <- readr::read_csv(demo_file, show_col_types = FALSE) |>
  janitor::clean_names()

log_line(paste0("Loaded demographics: n = ", nrow(demo), " rows, ",
                ncol(demo), " columns"))

# --- Column name config (override in demographics.columns.*) ---
col_age      <- cfg$demographics$columns$age          %||% "age"
col_gender   <- cfg$demographics$columns$gender       %||% "gender"
col_level    <- cfg$demographics$columns$training_level %||% "training_level"
col_id       <- cfg$demographics$columns$participant  %||% "participant"

int_label <- cfg$study$intervention_label %||% "Intervention"
ctl_label <- cfg$study$control_label      %||% "Control"

pt_alpha <- as.numeric(cfg_get("figures", "point_alpha", default = 0.75))
pt_size  <- as.numeric(cfg_get("figures", "point_size",  default = 2.2))
col_int  <- cfg_get("figures", "color_intervention", default = "#2E8B57")
col_ctl  <- cfg_get("figures", "color_control",      default = "#CD853F")

# =============================================================================
# TABLE D1: Sample Characteristics
# =============================================================================
log_h2("Table D1: Sample Characteristics")

.sum_rows <- list()

# Age
if (col_age %in% names(demo)) {
  ages <- demo[[col_age]]
  .sum_rows[["age"]] <- tibble::tibble(
    Characteristic = "Age (years)",
    n              = sum(!is.na(ages)),
    Value          = sprintf("%.1f (%.1f)",
                             mean(ages, na.rm = TRUE),
                             sd(ages,   na.rm = TRUE)),
    Note = sprintf("range %g\u2013%g", min(ages, na.rm=TRUE), max(ages, na.rm=TRUE))
  )
}

# Gender
if (col_gender %in% names(demo)) {
  gender_tbl <- demo |>
    dplyr::count(.data[[col_gender]], name = "n") |>
    dplyr::mutate(
      Characteristic = paste0("Gender: ", .data[[col_gender]]),
      Value          = sprintf("%d (%.1f%%)", n, 100 * n / sum(n)),
      Note           = NA_character_
    ) |>
    dplyr::select(Characteristic, n, Value, Note)
  .sum_rows[["gender"]] <- gender_tbl
}

# Training level
if (col_level %in% names(demo)) {
  level_tbl <- demo |>
    dplyr::count(.data[[col_level]], name = "n") |>
    dplyr::mutate(
      Characteristic = paste0("Level: ", .data[[col_level]]),
      Value          = sprintf("%d (%.1f%%)", n, 100 * n / sum(n)),
      Note           = NA_character_
    ) |>
    dplyr::select(Characteristic, n, Value, Note)
  .sum_rows[["level"]] <- level_tbl
}

# Sequence allocation (from main analysis data)
seq_alloc <- tibble::tibble(
  Characteristic = c(
    paste0(int_label, "-first"),
    paste0(ctl_label, "-first")
  ),
  n = c(
    sum(dat$intervention_period == 1, na.rm = TRUE),
    sum(dat$intervention_period == 2, na.rm = TRUE)
  )
) |>
  dplyr::mutate(
    Value = sprintf("%d (%.1f%%)", n, 100 * n / sum(n)),
    Note  = NA_character_
  )

.sum_rows[["sequence"]] <- seq_alloc

tblD1 <- dplyr::bind_rows(.sum_rows)

save_table(tblD1, "D1_sample_characteristics", subfolder = "descriptive",
           caption = "Sample characteristics: continuous variables M(SD); categorical variables n(%)")

# =============================================================================
# FIGURE D1: Age distribution (only if age column present)
# =============================================================================
if (col_age %in% names(demo)) {
  log_h2("Figure D1: Age distribution")

  ages <- demo[[col_age]]
  bw   <- max(1, diff(range(ages, na.rm = TRUE)) / 15)

  figD1 <- ggplot2::ggplot(demo, ggplot2::aes(x = .data[[col_age]])) +
    ggplot2::geom_histogram(binwidth = bw, fill = col_int,
                             colour = "white", alpha = 0.85, linewidth = 0.3) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("Count") +
    theme_clean()

  save_figure(figD1, "D1_age_distribution", subfolder = "descriptive",
              width = 6, height = 4)
}

# =============================================================================
# FIGURE D2: Gender distribution bar chart
# =============================================================================
if (col_gender %in% names(demo)) {
  log_h2("Figure D2: Gender distribution")

  gender_counts <- demo |>
    dplyr::count(.data[[col_gender]], name = "n") |>
    dplyr::mutate(
      pct   = 100 * n / sum(n),
      label = paste0(n, "\n(", round(pct, 1), "%)")
    )

  figD2 <- ggplot2::ggplot(gender_counts,
      ggplot2::aes(x = .data[[col_gender]], y = n,
                   fill = .data[[col_gender]])) +
    ggplot2::geom_col(width = 0.55, alpha = 0.85, colour = "grey30") +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       vjust = -0.4, size = 3.5) +
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Count") +
    ggplot2::expand_limits(y = max(gender_counts$n) * 1.18) +
    theme_clean()

  save_figure(figD2, "D2_gender_distribution", subfolder = "descriptive",
              width = 5.5, height = 4.5)
}

# =============================================================================
# FIGURE D3: Training level distribution
# =============================================================================
if (col_level %in% names(demo)) {
  log_h2("Figure D3: Training level distribution")

  level_counts <- demo |>
    dplyr::count(.data[[col_level]], name = "n") |>
    dplyr::mutate(
      pct   = 100 * n / sum(n),
      label = paste0(n, "\n(", round(pct, 1), "%)")
    )

  figD3 <- ggplot2::ggplot(level_counts,
      ggplot2::aes(x = factor(.data[[col_level]]), y = n)) +
    ggplot2::geom_col(width = 0.55, fill = col_ctl,
                      alpha = 0.85, colour = "grey30") +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       vjust = -0.4, size = 3.5) +
    ggplot2::xlab("Training Level") +
    ggplot2::ylab("Count") +
    ggplot2::expand_limits(y = max(level_counts$n) * 1.18) +
    theme_clean()

  save_figure(figD3, "D3_training_level_distribution", subfolder = "descriptive",
              width = 6, height = 4.5)
}

# =============================================================================
# SF-36 MODULE (optional — sf36.generate: true in config)
# =============================================================================
if (isTRUE(as.logical(cfg$sf36$generate %||% FALSE))) {
  log_h2("SF-36 analysis")

  sf36_file <- file.path(data_dir, cfg$sf36$file %||% "sf36_scores.csv")

  if (!file.exists(sf36_file)) {
    log_warn(paste0("SF-36 file not found: ", sf36_file, " — skipping SF-36 section."))
  } else {
    sf36 <- readr::read_csv(sf36_file, show_col_types = FALSE) |>
      janitor::clean_names()

    # Default SF-36 domain column names; override via cfg$sf36$domains list
    sf36_default_domains <- c(
      "physical_functioning", "role_physical", "bodily_pain", "general_health",
      "vitality",             "social_functioning", "role_emotional", "mental_health"
    )
    sf36_domain_labels <- c(
      "Physical Functioning", "Role - Physical", "Bodily Pain", "General Health",
      "Vitality", "Social Functioning", "Role - Emotional", "Mental Health"
    )

    # Use config overrides if provided
    domains_use <- if (!is.null(cfg$sf36$domains)) {
      names(cfg$sf36$domains)
    } else {
      sf36_default_domains
    }
    domain_labels_use <- if (!is.null(cfg$sf36$domains)) {
      unlist(cfg$sf36$domains)
    } else {
      sf36_domain_labels
    }

    # Keep only domains that actually exist in the data
    present <- domains_use %in% names(sf36)
    domains_use       <- domains_use[present]
    domain_labels_use <- domain_labels_use[present]

    if (length(domains_use) == 0) {
      log_warn("No SF-36 domain columns found in sf36_scores.csv — skipping.")
    } else {
      # TABLE: SF-36 domain scores
      sf36_summary <- tibble::tibble(
        Domain = domain_labels_use,
        M      = round(purrr::map_dbl(domains_use, ~mean(sf36[[.x]], na.rm=TRUE)), 1),
        SD     = round(purrr::map_dbl(domains_use, ~sd(sf36[[.x]],   na.rm=TRUE)), 1),
        Median = round(purrr::map_dbl(domains_use, ~stats::median(sf36[[.x]], na.rm=TRUE)), 1),
        Min    = round(purrr::map_dbl(domains_use, ~min(sf36[[.x]], na.rm=TRUE)), 1),
        Max    = round(purrr::map_dbl(domains_use, ~max(sf36[[.x]], na.rm=TRUE)), 1)
      )

      save_table(sf36_summary, "D4_sf36_domain_scores", subfolder = "descriptive",
                 caption = "SF-36 health survey domain scores (0-100; higher = better)")

      # FIGURE: SF-36 profile
      sf36_means <- tibble::tibble(
        domain_label = factor(domain_labels_use, levels = domain_labels_use),
        mean_score   = sf36_summary$M,
        se           = round(purrr::map_dbl(domains_use,
                               ~sd(sf36[[.x]], na.rm=TRUE) /
                                 sqrt(sum(!is.na(sf36[[.x]])))), 2)
      )

      figSF <- ggplot2::ggplot(sf36_means,
          ggplot2::aes(x = .data$domain_label, y = .data$mean_score)) +
        ggplot2::geom_col(width = 0.65, fill = col_int,
                          alpha = 0.85, colour = "grey30") +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$mean_score - .data$se,
                       ymax = .data$mean_score + .data$se),
          width = 0.25, linewidth = 0.8
        ) +
        ggplot2::coord_cartesian(ylim = c(0, 100)) +
        ggplot2::scale_y_continuous(breaks = seq(0, 100, 20)) +
        ggplot2::xlab("SF-36 Domain") +
        ggplot2::ylab("Mean Score (0\u2013100)") +
        theme_clean() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 9)
        )

      save_figure(figSF, "D5_sf36_profile", subfolder = "descriptive",
                  width = 10, height = 5)
    }
  }
}

log_h2("DEMOGRAPHICS COMPLETE")

}) # end local()
