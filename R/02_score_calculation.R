## =============================================================================
## R/02_score_calculation.R
## Build wide (per-participant) and long analysis datasets from item responses.
## Scores are scaled 0–N (config: scores.scale_to).
## Both full-item and restricted-item scores are computed when exclusions exist.
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
log_h1("02  SCORE CALCULATION")

cfg      <- read_config()
raw_data <- load_rds("raw_data")

assignment        <- raw_data$assignment
x_items           <- raw_data$x_items
y_items           <- raw_data$y_items
x_cols_full       <- raw_data$x_cols_full
x_cols_restricted <- raw_data$x_cols_restricted
y_cols_full       <- raw_data$y_cols_full
y_cols_restricted <- raw_data$y_cols_restricted

scale_to      <- as.numeric(cfg$scores$scale_to %||% 10)
has_x_excl    <- length(raw_data$x_excluded) > 0
has_y_excl    <- length(raw_data$y_excluded) > 0
has_exclusions <- has_x_excl || has_y_excl

# Labels
int_label <- cfg$study$intervention_label %||% "Intervention"
ctl_label <- cfg$study$control_label      %||% "Control"

# =============================================================================
# SCORE FUNCTIONS
# =============================================================================

compute_score <- function(items_df, cols, scale_to) {
  raw <- rowSums(dplyr::select(items_df, dplyr::all_of(cols)), na.rm = TRUE)
  (raw / length(cols)) * scale_to
}

# =============================================================================
# COMPUTE X SCORES
# =============================================================================

x_scores <- x_items |>
  dplyr::select(participant, dplyr::all_of(x_cols_full)) |>
  dplyr::mutate(
    x_raw_full       = rowSums(dplyr::across(dplyr::all_of(x_cols_full)), na.rm = TRUE),
    x_score_full     = (.data$x_raw_full / length(x_cols_full)) * scale_to
  )

if (has_x_excl) {
  x_scores <- x_scores |>
    dplyr::mutate(
      x_raw_restricted  = rowSums(dplyr::across(dplyr::all_of(x_cols_restricted)), na.rm = TRUE),
      x_score_restricted = (.data$x_raw_restricted / length(x_cols_restricted)) * scale_to
    )
} else {
  x_scores <- x_scores |>
    dplyr::mutate(
      x_raw_restricted   = .data$x_raw_full,
      x_score_restricted = .data$x_score_full
    )
}

# Add time taken if available
if ("time_taken_x_sec" %in% names(x_items)) {
  x_scores <- dplyr::left_join(x_scores,
    dplyr::select(x_items, participant, time_taken_x_sec),
    by = "participant")
}

x_scores <- dplyr::select(x_scores, participant, dplyr::starts_with("x_"))
log_check("X scores: mean_full=", round(mean(x_scores$x_score_full), 2),
          " | mean_restricted=", round(mean(x_scores$x_score_restricted), 2))
log_calc("X score (full)",
  formula = "(row_sum_of_items / n_items) * scale_to",
  inputs  = list(
    n_items        = length(x_cols_full),
    scale_to       = scale_to,
    n_participants = nrow(x_scores),
    items_used     = paste(x_cols_full, collapse = ", ")
  ),
  result  = paste0(
    "mean=", round(mean(x_scores$x_score_full), 4),
    "  SD=",  round(sd(x_scores$x_score_full), 4),
    "  min=", round(min(x_scores$x_score_full), 2),
    "  max=", round(max(x_scores$x_score_full), 2)
  )
)
if (has_x_excl) {
  log_calc("X score (restricted)",
    formula = "(row_sum_of_items / n_restricted_items) * scale_to",
    inputs  = list(
      n_restricted = length(x_cols_restricted),
      excluded     = paste(raw_data$x_excluded, collapse = ", ")
    ),
    result  = paste0(
      "mean=", round(mean(x_scores$x_score_restricted), 4),
      "  SD=",  round(sd(x_scores$x_score_restricted), 4)
    )
  )
}

# =============================================================================
# COMPUTE Y SCORES
# =============================================================================

y_scores <- y_items |>
  dplyr::select(participant, dplyr::all_of(y_cols_full)) |>
  dplyr::mutate(
    y_raw_full       = rowSums(dplyr::across(dplyr::all_of(y_cols_full)), na.rm = TRUE),
    y_score_full     = (.data$y_raw_full / length(y_cols_full)) * scale_to
  )

if (has_y_excl) {
  y_scores <- y_scores |>
    dplyr::mutate(
      y_raw_restricted  = rowSums(dplyr::across(dplyr::all_of(y_cols_restricted)), na.rm = TRUE),
      y_score_restricted = (.data$y_raw_restricted / length(y_cols_restricted)) * scale_to
    )
} else {
  y_scores <- y_scores |>
    dplyr::mutate(
      y_raw_restricted   = .data$y_raw_full,
      y_score_restricted = .data$y_score_full
    )
}

if ("time_taken_y_sec" %in% names(y_items)) {
  y_scores <- dplyr::left_join(y_scores,
    dplyr::select(y_items, participant, time_taken_y_sec),
    by = "participant")
}

y_scores <- dplyr::select(y_scores, participant, dplyr::starts_with("y_"))
log_check("Y scores: mean_full=", round(mean(y_scores$y_score_full), 2),
          " | mean_restricted=", round(mean(y_scores$y_score_restricted), 2))
log_calc("Y score (full)",
  formula = "(row_sum_of_items / n_items) * scale_to",
  inputs  = list(
    n_items        = length(y_cols_full),
    scale_to       = scale_to,
    n_participants = nrow(y_scores),
    items_used     = paste(y_cols_full, collapse = ", ")
  ),
  result  = paste0(
    "mean=", round(mean(y_scores$y_score_full), 4),
    "  SD=",  round(sd(y_scores$y_score_full), 4),
    "  min=", round(min(y_scores$y_score_full), 2),
    "  max=", round(max(y_scores$y_score_full), 2)
  )
)
if (has_y_excl) {
  log_calc("Y score (restricted)",
    formula = "(row_sum_of_items / n_restricted_items) * scale_to",
    inputs  = list(
      n_restricted = length(y_cols_restricted),
      excluded     = paste(raw_data$y_excluded, collapse = ", ")
    ),
    result  = paste0(
      "mean=", round(mean(y_scores$y_score_restricted), 4),
      "  SD=",  round(sd(y_scores$y_score_restricted), 4)
    )
  )
}

# =============================================================================
# BUILD WIDE DATASET
# =============================================================================

dat <- assignment |>
  dplyr::left_join(x_scores, by = "participant") |>
  dplyr::left_join(y_scores, by = "participant") |>
  dplyr::arrange(.data$participant)

# ---- Condition scores (intervention vs control) ----
# Each participant's score under intervention condition and control condition
# (which form they used in each condition is determined by the period/form mapping)
for (.scoring in c("full", "restricted")) {
  
  # Which score did they get under intervention vs control?
  # intervention_period tells us when they received the intervention.
  # form_x_period / form_y_period tell us when they took each form.
  # If form_x_period == intervention_period, their X score = intervention score.
  
  dat <- dat |>
    dplyr::mutate(
      !!paste0("intervention_score_", .scoring) := dplyr::case_when(
        .data$form_x_period == .data$intervention_period ~
          .data[[paste0("x_score_", .scoring)]],
        .data$form_y_period == .data$intervention_period ~
          .data[[paste0("y_score_", .scoring)]],
        TRUE ~ NA_real_
      ),
      !!paste0("control_score_", .scoring) := dplyr::case_when(
        .data$form_x_period == .data$control_period ~
          .data[[paste0("x_score_", .scoring)]],
        .data$form_y_period == .data$control_period ~
          .data[[paste0("y_score_", .scoring)]],
        TRUE ~ NA_real_
      ),
      # Period scores (regardless of condition)
      !!paste0("period1_score_", .scoring) := dplyr::case_when(
        .data$form_x_period == 1L ~ .data[[paste0("x_score_", .scoring)]],
        .data$form_y_period == 1L ~ .data[[paste0("y_score_", .scoring)]],
        TRUE ~ NA_real_
      ),
      !!paste0("period2_score_", .scoring) := dplyr::case_when(
        .data$form_x_period == 2L ~ .data[[paste0("x_score_", .scoring)]],
        .data$form_y_period == 2L ~ .data[[paste0("y_score_", .scoring)]],
        TRUE ~ NA_real_
      )
    )
}

# Sequence group label
dat <- dat |>
  dplyr::mutate(
    sequence_group = dplyr::case_when(
      .data$intervention_period == 1L ~
        paste0(int_label, "-first"),
      .data$intervention_period == 2L ~
        paste0(ctl_label, "-first"),
      TRUE ~ "Unknown"
    )
  )

# =============================================================================
# 4-SUBGROUP VARIABLE
# In a counterbalanced 2x2 crossover, participants fall into one of four
# cells based on:
#   (1) WHEN they received the intervention (period 1 or 2)
#   (2) WHICH form served as their intervention form (Form X or Form Y)
# This distinguishes e.g. "intervention-first via Form X" from
# "intervention-first via Form Y" — important when form equivalence is
# uncertain or form order effects are suspected.
# =============================================================================

dat <- dat |>
  dplyr::mutate(
    # Which form served as the intervention form for this participant?
    intervention_form = dplyr::case_when(
      .data$form_x_period == .data$intervention_period ~ cfg$study$form_x_label %||% "Form X",
      .data$form_y_period == .data$intervention_period ~ cfg$study$form_y_label %||% "Form Y",
      TRUE ~ "Unknown"
    ),
    # 4-level subgroup: sequence × intervention form
    subgroup4 = paste0(
      dplyr::if_else(.data$intervention_period == 1L,
                     paste0(int_label, "-first"),
                     paste0(ctl_label, "-first")),
      " / ",
      .data$intervention_form
    )
  )

# Log 4-subgroup allocation
subgroup4_tbl <- dplyr::count(dat, .data$subgroup4, name = "n") |>
  dplyr::arrange(.data$subgroup4)

log_h2("4-subgroup allocation")
log_line(sprintf("  %-55s  n", "Subgroup"))
log_line("  ", strrep("-", 60))
for (i in seq_len(nrow(subgroup4_tbl))) {
  log_line(sprintf("  %-55s  %d",
    subgroup4_tbl$subgroup4[i], subgroup4_tbl$n[i]))
}

log_check("wide dataset rows=", nrow(dat))
log_check("intervention_score_full range: ",
          round(min(dat$intervention_score_full, na.rm=TRUE),2), "–",
          round(max(dat$intervention_score_full, na.rm=TRUE),2))
log_check("control_score_full range: ",
          round(min(dat$control_score_full, na.rm=TRUE),2), "–",
          round(max(dat$control_score_full, na.rm=TRUE),2))

save_rds(dat, "analysis_data")

# =============================================================================
# BUILD LONG DATASET
# =============================================================================

score_cols <- grep("_score_(full|restricted)$", names(dat), value = TRUE)

dat_long <- dat |>
  dplyr::select(participant, sequence_group, intervention_period,
                dplyr::all_of(score_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(score_cols),
    names_to  = c("context", "scoring"),
    names_pattern = "^(.+)_score_(full|restricted)$",
    values_to = "score"
  ) |>
  dplyr::mutate(
    condition = dplyr::case_when(
      .data$context == "intervention" ~ int_label,
      .data$context == "control"      ~ ctl_label,
      TRUE ~ NA_character_
    ),
    period = dplyr::case_when(
      .data$context == "period1" ~ "Period 1",
      .data$context == "period2" ~ "Period 2",
      TRUE ~ NA_character_
    ),
    scoring = factor(.data$scoring, levels = c("full", "restricted"))
  ) |>
  dplyr::filter(!is.na(.data$score))

save_rds(dat_long, "analysis_data_long")

# =============================================================================
# SUMMARY
# =============================================================================

log_h2("SCORE CALCULATION COMPLETE")
for (.s in c("full", "restricted")) {
  int_mean <- mean(dat[[paste0("intervention_score_", .s)]], na.rm = TRUE)
  ctl_mean <- mean(dat[[paste0("control_score_", .s)]], na.rm = TRUE)
  p1_mean  <- mean(dat[[paste0("period1_score_", .s)]], na.rm = TRUE)
  p2_mean  <- mean(dat[[paste0("period2_score_", .s)]], na.rm = TRUE)
  log_line("  [", .s, "] ",
           int_label, " mean=", round(int_mean,2),
           " | ", ctl_label, " mean=", round(ctl_mean,2),
           " | Period1 mean=", round(p1_mean,2),
           " | Period2 mean=", round(p2_mean,2))
}
if (!has_exclusions) {
  log_line("  (full = restricted — no items excluded in config)")
}

# Per-participant score table in the log
log_h2("Per-participant score table")
log_line(sprintf("  %-8s  %-22s  %-8s  %-8s  %-8s  %-8s",
  "Part.", "Sequence", "Int.", "Ctrl.", "Per.1", "Per.2"))
for (i in seq_len(nrow(dat))) {
  r <- dat[i, ]
  log_line(sprintf("  %-8s  %-22s  %-8.2f  %-8.2f  %-8.2f  %-8.2f",
    r$participant,
    r$sequence_group,
    r$intervention_score_full,
    r$control_score_full,
    r$period1_score_full,
    r$period2_score_full))
}
