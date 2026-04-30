## =============================================================================
## R/05_figures.R
## All publication-quality figures.
## Rules:
##   - No titles, no subtitles, no captions on the PNG
##   - Axis labels come from config (intervention_label, form labels, etc.)
##   - Filenames are descriptive
##   - All output as PNG at config DPI
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
log_h1("05  FIGURES")

cfg      <- read_config()
dat      <- load_rds("analysis_data")
results  <- load_rds("analysis_results")
psych    <- load_rds("psychometrics")

int_label  <- cfg$study$intervention_label %||% "Intervention"
ctl_label  <- cfg$study$control_label      %||% "Control"
form_x_lbl <- cfg$study$form_x_label       %||% "Form X"
form_y_lbl <- cfg$study$form_y_label       %||% "Form Y"
score_lab  <- cfg_get("figures","y_axis_score_label", default = "Score (0–10)")
scale_to   <- as.numeric(cfg$scores$scale_to %||% 10)
# Axis: 1-point integer breaks, zoomed to the floor of the lowest observed score
score_y_lo     <- floor(min(dat[, grep("_score_(full|restricted)$", names(dat), value = TRUE)], na.rm = TRUE))
# Allow comparison renderer to force a common lower bound across all runs
.y_lo_env <- suppressWarnings(as.numeric(Sys.getenv("SCORE_Y_LO_OVERRIDE", unset = "")))
if (!is.na(.y_lo_env)) {
  score_y_lo <- .y_lo_env
  log_line("Score axis lower bound overridden to ", score_y_lo, " (common scale)")
}
score_y_breaks <- seq(score_y_lo, scale_to, by = 1)
.score_y_scale <- function() ggplot2::scale_y_continuous(
  name = score_lab, limits = c(score_y_lo, scale_to), breaks = score_y_breaks)

col_int   <- cfg_get("figures","color_intervention", default="#2E8B57")
col_ctl   <- cfg_get("figures","color_control",      default="#CD853F")
col_p1    <- cfg_get("figures","color_period1",      default="#4682B4")
col_p2    <- cfg_get("figures","color_period2",      default="#B22222")

# Helper: produce factor levels in numeric order (1, 2, …, 10, 11, …).
# Falls back to alphabetical if participant IDs aren't all numeric.
.part_levels <- function(x) {
  u <- as.character(unique(x))
  n <- suppressWarnings(as.numeric(u))
  if (!anyNA(n)) u[order(n)] else sort(u)
}

# =============================================================================
# HELPER: spaghetti + group mean layer
# =============================================================================

spaghetti_layer <- function(df, x_col, y_col, group_col, id_col,
                             colors, labels = NULL) {
  labs <- labels %||% levels(factor(df[[group_col]]))
  
  list(
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                   group = .data[[id_col]]),
      colour = "grey70", linewidth = 0.4, alpha = 0.6
    ),
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                   colour = .data[[group_col]]),
      size = 2.2, alpha = 0.75
    ),
    ggplot2::scale_colour_manual(values = colors, labels = labs,
                                 name = NULL),
    ggplot2::scale_x_discrete(labels = labs)
  )
}

# =============================================================================
# FIGURE 1: Intervention vs Control — paired spaghetti
# =============================================================================
log_h2("Figure 1: Intervention vs Control (paired spaghetti)")

df_int <- dat |>
  dplyr::select(participant, sequence_group,
                int   = intervention_score_full,
                ctl   = control_score_full) |>
  tidyr::pivot_longer(c(int, ctl),
                      names_to  = "condition_code",
                      values_to = "score") |>
  dplyr::mutate(
    condition = factor(
      dplyr::if_else(.data$condition_code == "int", int_label, ctl_label),
      levels = c(ctl_label, int_label)
    )
  )

group_means_int <- df_int |>
  dplyr::group_by(.data$condition) |>
  dplyr::summarise(
    mean  = mean(.data$score, na.rm = TRUE),
    se    = sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
    .groups = "drop"
  )

fig1 <- ggplot2::ggplot(df_int,
              ggplot2::aes(x = .data$condition, y = .data$score)) +
  ggplot2::geom_line(ggplot2::aes(group = .data$participant),
                     colour = "grey70", linewidth = 0.4, alpha = 0.7) +
  ggplot2::geom_jitter(ggplot2::aes(colour = .data$condition),
                       width = 0.06, size = 2.2, alpha = 0.7) +
  ggplot2::geom_crossbar(data = group_means_int,
    ggplot2::aes(x = .data$condition, y = .data$mean,
                 ymin = .data$mean - .data$se,
                 ymax = .data$mean + .data$se,
                 fill  = .data$condition),
    width = 0.35, alpha = 0.25, colour = "grey30", linewidth = 0.5
  ) +
  ggplot2::scale_colour_manual(
    values = c(stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label))),
    guide  = "none") +
  ggplot2::scale_fill_manual(
    values = c(stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label))),
    guide  = "none") +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab("Condition") +
  theme_clean()

save_figure(fig1, "intervention_vs_control_paired", subfolder = "primary")

# =============================================================================
# FIGURE 2: Intervention effect by sequence group (interaction check)
# =============================================================================
log_h2("Figure 2: Intervention effect by sequence")

df_seq <- df_int |>
  dplyr::mutate(sequence_group = dat$sequence_group[
    match(.data$participant, dat$participant)])

group_means_seq <- df_seq |>
  dplyr::group_by(.data$condition, .data$sequence_group) |>
  dplyr::summarise(
    mean = mean(.data$score, na.rm = TRUE),
    se   = sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
    .groups = "drop"
  )

fig2 <- ggplot2::ggplot(df_seq,
              ggplot2::aes(x = .data$condition, y = .data$score,
                           colour = .data$condition)) +
  ggplot2::geom_line(ggplot2::aes(group = .data$participant),
                     colour = "grey70", linewidth = 0.35, alpha = 0.65) +
  ggplot2::geom_jitter(width = 0.07, size = 2.0, alpha = 0.7) +
  ggplot2::geom_point(data = group_means_seq,
    ggplot2::aes(x = .data$condition, y = .data$mean),
    shape = 18, size = 4.5, stroke = 0.8
  ) +
  ggplot2::geom_errorbar(data = group_means_seq,
    ggplot2::aes(x  = .data$condition, y = .data$mean,
                 ymin = .data$mean - .data$se,
                 ymax = .data$mean + .data$se),
    width = 0.18, linewidth = 0.7
  ) +
  ggplot2::scale_colour_manual(
    values = c(stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label))),
    guide  = "none") +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab("Condition") +
  ggplot2::facet_wrap(~ sequence_group) +
  theme_clean()

save_figure(fig2, "intervention_effect_by_sequence", subfolder = "primary")

# df_period: data used by Figures 4 and 15
df_period <- dat |>
  dplyr::select(participant, sequence_group,
                `Period 1` = period1_score_full,
                `Period 2` = period2_score_full) |>
  tidyr::pivot_longer(c(`Period 1`, `Period 2`),
                      names_to  = "period",
                      values_to = "score") |>
  dplyr::mutate(period = factor(.data$period,
                                levels = c("Period 1", "Period 2")))

# =============================================================================
# FIGURE 4: Period effects by sequence — 2-panel
# =============================================================================
log_h2("Figure 4: Period effects by sequence")

df_period_seq <- dplyr::mutate(df_period,
  sequence_group = dat$sequence_group[match(.data$participant, dat$participant)]
)

group_means_period_seq <- df_period_seq |>
  dplyr::group_by(.data$period, .data$sequence_group) |>
  dplyr::summarise(
    mean = mean(.data$score, na.rm = TRUE),
    se   = sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
    .groups = "drop"
  )

fig4 <- ggplot2::ggplot(df_period_seq,
              ggplot2::aes(x = .data$period, y = .data$score,
                           colour = .data$period)) +
  ggplot2::geom_line(ggplot2::aes(group = .data$participant),
                     colour = "grey70", linewidth = 0.35, alpha = 0.65) +
  ggplot2::geom_jitter(width = 0.07, size = 2.0, alpha = 0.7) +
  ggplot2::geom_point(data = group_means_period_seq,
    ggplot2::aes(y = .data$mean), shape = 18, size = 4.5) +
  ggplot2::geom_errorbar(data = group_means_period_seq,
    ggplot2::aes(y = .data$mean,
                 ymin = .data$mean - .data$se,
                 ymax = .data$mean + .data$se),
    width = 0.18, linewidth = 0.7
  ) +
  ggplot2::scale_colour_manual(
    values = c("Period 1" = col_p1, "Period 2" = col_p2),
    guide  = "none") +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab("Period") +
  ggplot2::facet_wrap(~ sequence_group) +
  theme_clean()

save_figure(fig4, "period_effects_by_sequence", subfolder = "period_effects")

# =============================================================================
# FIGURE 5: Form X vs Form Y score correlation (form equivalence)
# =============================================================================
log_h2("Figure 5: Form X vs Form Y correlation")

fig5 <- dat |>
  dplyr::select(x = x_score_full, y = y_score_full, seq = sequence_group) |>
  ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_abline(slope = 1, intercept = 0,
                       linetype = "dashed", colour = "grey60", linewidth = 0.5) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ x,
                       colour = "grey40", fill = "grey85",
                       linewidth = 0.8, alpha = 0.4) +
  ggplot2::geom_point(ggplot2::aes(colour = .data$seq),
                      size = 2.5, alpha = 0.8) +
  ggplot2::scale_colour_manual(
    values = c(
      stats::setNames(c(col_int, col_ctl),
                      c(paste0(int_label, "-first"),
                        paste0(ctl_label, "-first")))
    ), name = "Sequence"
  ) +
  ggplot2::scale_x_continuous(name = paste0(form_x_lbl, " Score"),
                               limits = c(score_y_lo, scale_to), breaks = score_y_breaks) +
  ggplot2::scale_y_continuous(name = paste0(form_y_lbl, " Score"),
                               limits = c(score_y_lo, scale_to), breaks = score_y_breaks) +
  theme_clean() +
  ggplot2::theme(legend.position = "bottom")

save_figure(fig5, "form_x_vs_form_y_correlation", subfolder = "psychometrics")

# =============================================================================
# FIGURE 6: Mixed-model coefficient plot (if models available)
# =============================================================================
log_h2("Figure 6: Mixed-model coefficients")

if (!is.null(results$mixed_models$full$model1)) {
  coef_data <- purrr::map_dfr(c("full", "restricted"), function(scoring) {
    m <- results$mixed_models[[scoring]]$model1
    if (is.null(m)) return(NULL)
    coefs <- as.data.frame(summary(m)$coefficients)
    coefs$term    <- rownames(coefs)
    coefs$scoring <- scoring
    coefs
  }) |>
    dplyr::filter(!grepl("Intercept", .data$term)) |>
    dplyr::mutate(
      term_clean = .data$term |>
        gsub("condition_fac", "Condition: ", x = _) |>
        gsub("period_fac",    "Period: ", x = _) |>
        gsub("sequence_fac",  "Seq: ", x = _),
      lo95 = .data$Estimate - 1.96 * .data$`Std. Error`,
      hi95 = .data$Estimate + 1.96 * .data$`Std. Error`
    )
  
  fig6 <- coef_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$term_clean,
                                 y = .data$Estimate,
                                 colour = .data$scoring)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo95,
                                        ymax = .data$hi95),
                           width = 0.25, linewidth = 0.8,
                           position = ggplot2::position_dodge(0.4)) +
    ggplot2::geom_point(size = 2.8,
                        position = ggplot2::position_dodge(0.4)) +
    ggplot2::scale_colour_manual(
      values = c(full = col_int, restricted = col_ctl),
      name   = "Scoring"
    ) +
    ggplot2::coord_flip() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Estimate (95% CI)") +
    theme_clean()
  
  save_figure(fig6, "mixed_model_fixed_effects", subfolder = "mixed_models")
} else {
  log_warn("Mixed models unavailable — Figure 6 skipped.")
}

# =============================================================================
# FIGURE 7: Participant-level scores (all four scores per person)
# =============================================================================
log_h2("Figure 7: Per-participant profile")

df_pp <- dat |>
  dplyr::select(
    participant, sequence_group,
    !!int_label := intervention_score_full,
    !!ctl_label := control_score_full
  ) |>
  tidyr::pivot_longer(
    c(dplyr::all_of(c(int_label, ctl_label))),
    names_to = "condition", values_to = "score"
  ) |>
  dplyr::mutate(
    condition  = factor(.data$condition, levels = c(ctl_label, int_label)),
    participant = factor(.data$participant, levels = .part_levels(dat$participant))
  )

fig7 <- ggplot2::ggplot(df_pp,
              ggplot2::aes(x = .data$participant, y = .data$score,
                           colour = .data$condition,
                           shape  = .data$condition)) +
  ggplot2::geom_segment(
    data = dat |>
      dplyr::mutate(
        participant = factor(.data$participant, levels = .part_levels(dat$participant))
      ),
    ggplot2::aes(
      x    = .data$participant, xend = .data$participant,
      y    = .data$control_score_full,
      yend = .data$intervention_score_full,
      group = .data$participant
    ),
    colour = "grey70", linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.85) +
  ggplot2::scale_colour_manual(
    values = c(stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label))),
    name   = "Condition"
  ) +
  ggplot2::scale_shape_manual(
    values = c(stats::setNames(c(16, 17), c(ctl_label, int_label))),
    name   = "Condition"
  ) +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab("Participant") +
  theme_clean() +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

save_figure(fig7, "per_participant_condition_scores",
            subfolder = "supplementary", width = 9, height = 5)

# =============================================================================
# FIGURE 8: Score distribution — violin + box, all four conditions
# =============================================================================
log_h2("Figure 8: Distribution violin/box")

df_dist <- dplyr::bind_rows(
  dplyr::transmute(dat, label = int_label, score = intervention_score_full),
  dplyr::transmute(dat, label = ctl_label, score = control_score_full),
  dplyr::transmute(dat, label = "Period 1", score = period1_score_full),
  dplyr::transmute(dat, label = "Period 2", score = period2_score_full)
) |>
  dplyr::mutate(
    label = factor(.data$label,
                   levels = c(ctl_label, int_label, "Period 1", "Period 2")),
    group_type = ifelse(.data$label %in% c(int_label, ctl_label),
                        "Condition", "Period")
  )

fig8 <- ggplot2::ggplot(df_dist,
              ggplot2::aes(x = .data$label, y = .data$score,
                           fill = .data$label, colour = .data$label)) +
  ggplot2::geom_violin(alpha = 0.25, linewidth = 0.4, trim = TRUE) +
  ggplot2::geom_boxplot(width = 0.18, alpha = 0.6, outlier.size = 2.5,
                        linewidth = 0.5, colour = "grey25",
                        fill = "white") +
  ggplot2::scale_fill_manual(
    values = c(stats::setNames(
      c(col_ctl, col_int, col_p1, col_p2),
      c(ctl_label, int_label, "Period 1", "Period 2")
    )), guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = c(stats::setNames(
      c(col_ctl, col_int, col_p1, col_p2),
      c(ctl_label, int_label, "Period 1", "Period 2")
    )), guide = "none"
  ) +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab(NULL) +
  ggplot2::facet_wrap(~ group_type, scales = "free_x") +
  theme_clean()

save_figure(fig8, "score_distributions_violin_box", subfolder = "descriptive")

# =============================================================================
# FIGURE 9: Restricted vs full score comparison (only if exclusions exist
#            AND optional_analyses.run_restricted_comparison is not FALSE)
# =============================================================================
if ((length(cfg$item_exclusions$y %||% character(0)) > 0 ||
     length(cfg$item_exclusions$x %||% character(0)) > 0) &&
    isTRUE(cfg_get("optional_analyses", "run_restricted_comparison", default = TRUE))) {
  
  log_h2("Figure 9: Full vs restricted scoring")
  
  df_scoring_compare <- dplyr::bind_rows(
    dplyr::transmute(dat, scoring = "Full",
                     int_score = intervention_score_full,
                     ctl_score = control_score_full),
    dplyr::transmute(dat, scoring = "Restricted",
                     int_score = intervention_score_restricted,
                     ctl_score = control_score_restricted)
  ) |>
    tidyr::pivot_longer(c(int_score, ctl_score),
                        names_to = "condition_code", values_to = "score") |>
    dplyr::mutate(
      condition = factor(
        dplyr::if_else(.data$condition_code == "int_score", int_label, ctl_label),
        levels = c(ctl_label, int_label)
      ),
      scoring = factor(.data$scoring, levels = c("Full", "Restricted"))
    )
  
  fig9 <- ggplot2::ggplot(df_scoring_compare,
                ggplot2::aes(x = .data$condition, y = .data$score,
                             fill = .data$scoring)) +
    ggplot2::geom_boxplot(width = 0.45, alpha = 0.7, outlier.size = 2,
                          position = ggplot2::position_dodge(0.55)) +
    ggplot2::scale_fill_manual(
      values = c(Full = col_int, Restricted = col_ctl),
      name   = "Scoring"
    ) +
    ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                                 breaks = score_y_breaks) +
    ggplot2::labs(
      x        = "Condition",
      subtitle = "Within-run comparison — same data, full vs restricted item scoring"
    ) +
    theme_clean() +
    ggplot2::theme(legend.position = "bottom")
  
  save_figure(fig9, "full_vs_restricted_scoring", subfolder = "supplementary")
}

# =============================================================================
# Aesthetic config shared by new figures
# =============================================================================
pt_alpha <- as.numeric(cfg_get("figures", "point_alpha",    default = 0.75))
pt_size  <- as.numeric(cfg_get("figures", "point_size",     default = 2.2))
ln_alpha <- as.numeric(cfg_get("figures", "line_alpha",     default = 0.55))
eb_width <- as.numeric(cfg_get("figures", "errorbar_width", default = 0.18))
ci_level <- as.numeric(cfg$figures$ci_level %||% 0.95)
ci_mult  <- stats::qnorm(0.5 + ci_level / 2)

col_seq_ab <- cfg_get("figures", "color_seq_ab", default = col_int)
col_seq_ba <- cfg_get("figures", "color_seq_ba", default = col_ctl)

# Sequence-group colour map (used by multiple figures below)
.seq_ab_lbl  <- paste0(int_label, "-first")
.seq_ba_lbl  <- paste0(ctl_label, "-first")
seq_colors   <- stats::setNames(c(col_seq_ab, col_seq_ba),
                                 c(.seq_ab_lbl, .seq_ba_lbl))

# =============================================================================
# FIGURE 10: Score delta dotplot (int − ctl, sorted lollipop per participant)
# =============================================================================
log_h2("Figure 10: Score delta dotplot")

dat_delta <- dat |>
  dplyr::mutate(delta = .data$intervention_score_full - .data$control_score_full) |>
  dplyr::arrange(.data$delta) |>
  dplyr::mutate(participant = factor(
    .data$participant,
    levels = unique(as.character(.data$participant))  # intentionally delta-sorted
  ))

fig10 <- ggplot2::ggplot(dat_delta,
    ggplot2::aes(x = .data$delta, y = .data$participant)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      colour = "grey50", linewidth = 0.5) +
  ggplot2::geom_segment(
    ggplot2::aes(x    = 0,           xend = .data$delta,
                 y    = .data$participant, yend = .data$participant),
    colour = "grey70", linewidth = 0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = .data$delta > 0),
    size = pt_size, alpha = pt_alpha
  ) +
  ggplot2::scale_colour_manual(
    values = c(`TRUE` = col_int, `FALSE` = col_ctl),
    labels = c(`TRUE` = paste0(int_label, " higher"),
               `FALSE` = paste0(ctl_label, " higher")),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(
    name = paste0(int_label, " \u2212 ", ctl_label, " (score difference)")
  ) +
  ggplot2::ylab("Participant (sorted by delta)") +
  theme_clean() +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.y    = ggplot2::element_blank(),
                 axis.ticks.y   = ggplot2::element_blank())

save_figure(fig10, "score_delta_dotplot", subfolder = "primary",
            width = 6.5, height = max(4.5, nrow(dat_delta) * 0.32))

# =============================================================================
# FIGURE 11: Effect size forest plot (all contrasts with CI)
# =============================================================================
log_h2("Figure 11: Effect size forest plot")

.es_row <- function(res, label, group) {
  if (is.null(res) || is.null(res$dz) || is.na(res$dz)) return(NULL)
  se_dz <- 1 / sqrt(max(res$n - 1, 1))
  tibble::tibble(
    label = label, group = group,
    dz    = res$dz,
    lo    = res$dz - ci_mult * se_dz,
    hi    = res$dz + ci_mult * se_dz,
    p     = res$p
  )
}

forest_data <- dplyr::bind_rows(
  .es_row(results$contrast_intervention_full,  paste0(int_label," vs ",ctl_label," (full)"),        "Intervention"),
  .es_row(results$contrast_intervention_restr, paste0(int_label," vs ",ctl_label," (restricted)"), "Intervention"),
  .es_row(results$contrast_period_full,        "Period 2 vs Period 1 (full)",                       "Period"),
  .es_row(results$contrast_period_restr,       "Period 2 vs Period 1 (restricted)",                 "Period")
)

if (!is.null(forest_data) && nrow(forest_data) > 0) {
  forest_data$label <- factor(forest_data$label, levels = rev(unique(forest_data$label)))
  fig11 <- ggplot2::ggplot(forest_data,
      ggplot2::aes(x = .data$dz, y = .data$label, colour = .data$group)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
      orientation = "y", width = 0.22, linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_colour_manual(
      values = c("Intervention" = col_int, "Period" = col_p1),
      name   = NULL
    ) +
    ggplot2::xlab(sprintf("Cohen's dz  (%d%% CI)", round(ci_level * 100))) +
    ggplot2::ylab(NULL) +
    ggplot2::facet_grid(.data$group ~ ., scales = "free_y", space = "free") +
    theme_clean() +
    ggplot2::theme(legend.position = "none",
                   strip.text.y    = ggplot2::element_text(angle = 0))
  save_figure(fig11, "effect_size_forest", subfolder = "primary",
              width = 7, height = max(3.5, nrow(forest_data) * 0.8))
} else {
  log_warn("No effect size data available — Figure 11 skipped.")
}

# =============================================================================
# FIGURE 13: Empirical CDF by condition
# =============================================================================
log_h2("Figure 13: Empirical CDF by condition")

df_ecdf <- dplyr::bind_rows(
  dplyr::transmute(dat, condition = int_label, score = .data$intervention_score_full),
  dplyr::transmute(dat, condition = ctl_label, score = .data$control_score_full)
) |>
  dplyr::mutate(
    condition = factor(.data$condition, levels = c(ctl_label, int_label))
  )

fig13 <- ggplot2::ggplot(df_ecdf,
    ggplot2::aes(x = .data$score, colour = .data$condition)) +
  ggplot2::stat_ecdf(linewidth = 0.95, pad = FALSE) +
  ggplot2::scale_colour_manual(
    values = stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label)),
    name   = "Condition"
  ) +
  ggplot2::scale_x_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::scale_y_continuous(
    name   = "Cumulative Proportion",
    labels = scales::percent_format()
  ) +
  theme_clean() +
  ggplot2::theme(legend.position = "bottom")

save_figure(fig13, "score_ecdf_by_condition", subfolder = "descriptive")

# =============================================================================
# FIGURE 14: Histogram of within-person score differences (int − ctl)
# =============================================================================
log_h2("Figure 14: Score difference histogram")

dat_diff14  <- dat |>
  dplyr::mutate(delta = .data$intervention_score_full - .data$control_score_full) |>
  dplyr::filter(!is.na(.data$delta))
n_diff14    <- nrow(dat_diff14)
mean_d14    <- mean(dat_diff14$delta, na.rm = TRUE)

# Adaptive binwidth: 1 for integer/narrow ranges, wider for large ranges
range_d14  <- diff(range(dat_diff14$delta, na.rm = TRUE))
bw_d14     <- if (range_d14 <= 20) 1 else ceiling(range_d14 / 20)

fig14 <- ggplot2::ggplot(dat_diff14, ggplot2::aes(x = .data$delta)) +
  ggplot2::geom_histogram(binwidth = bw_d14, fill = col_int, colour = "white",
                           linewidth = 0.3, boundary = -0.5) +
  ggplot2::geom_vline(xintercept = 0,       linetype = "dashed",
                      colour = "grey40",    linewidth = 0.6) +
  ggplot2::geom_vline(xintercept = mean_d14, linetype = "solid",
                      colour = col_ctl,     linewidth = 0.9) +
  ggplot2::labs(caption = paste0("N\u202f=\u202f", n_diff14)) +
  ggplot2::scale_x_continuous(
    name = paste0(int_label, " \u2212 ", ctl_label, " (score difference)")
  ) +
  ggplot2::scale_y_continuous(
    name   = "Count (participants)",
    breaks = function(x) {
      nice <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 500)
      # Pick the smallest nice step that keeps ticks <= 10
      step <- nice[which(ceiling(max(x) / nice) <= 9)[1]]
      if (is.na(step)) step <- nice[length(nice)]
      seq(0, ceiling(max(x) / step) * step, by = step)
    }
  ) +
  theme_clean()

save_figure(fig14, "score_difference_histogram", subfolder = "descriptive",
            width = 5.5, height = 4.0)

# =============================================================================
# FIGURE 15: Sequence trajectories (Period 1 → 2, group means + individual lines)
# =============================================================================
log_h2("Figure 15: Sequence group trajectories")

df_traj <- dat |>
  dplyr::select("participant", "sequence_group",
                "Period 1" = "period1_score_full",
                "Period 2" = "period2_score_full") |>
  tidyr::pivot_longer(c("Period 1", "Period 2"),
                      names_to  = "period",
                      values_to = "score") |>
  dplyr::mutate(period = factor(.data$period, levels = c("Period 1", "Period 2")))

means_traj <- df_traj |>
  dplyr::group_by(.data$sequence_group, .data$period) |>
  dplyr::summarise(
    tmean = mean(.data$score, na.rm = TRUE),
    tse   = sd(.data$score, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  )

fig15 <- ggplot2::ggplot(df_traj,
    ggplot2::aes(x = .data$period, y = .data$score,
                 colour = .data$sequence_group)) +
  ggplot2::geom_line(
    ggplot2::aes(group = .data$participant),
    alpha = ln_alpha * 0.6, linewidth = 0.35
  ) +
  ggplot2::geom_line(
    data    = means_traj,
    ggplot2::aes(x = .data$period, y = .data$tmean,
                 colour = .data$sequence_group, group = .data$sequence_group),
    linewidth = 1.3
  ) +
  ggplot2::geom_errorbar(
    data    = means_traj,
    ggplot2::aes(x    = .data$period, y = .data$tmean,
                 ymin = .data$tmean - .data$tse,
                 ymax = .data$tmean + .data$tse),
    width = eb_width, linewidth = 0.8
  ) +
  ggplot2::geom_point(
    data    = means_traj,
    ggplot2::aes(x = .data$period, y = .data$tmean),
    size = 3.5
  ) +
  ggplot2::scale_colour_manual(values = seq_colors, name = "Sequence") +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab(NULL) +
  theme_clean() +
  ggplot2::theme(legend.position = "bottom")

save_figure(fig15, "sequence_period_trajectories", subfolder = "period_effects")

# =============================================================================
# FIGURE 16: Q-Q normality plots for key score differences
# =============================================================================
log_h2("Figure 16: Q-Q normality plots")

df_qq <- dplyr::bind_rows(
  dplyr::transmute(dat,
    variable = paste0(int_label, " \u2212 ", ctl_label),
    value    = .data$intervention_score_full - .data$control_score_full
  ),
  dplyr::transmute(dat,
    variable = "Period 2 \u2212 Period 1",
    value    = .data$period2_score_full - .data$period1_score_full
  )
)

fig16 <- ggplot2::ggplot(df_qq, ggplot2::aes(sample = .data$value)) +
  ggplot2::stat_qq(size = pt_size, alpha = pt_alpha, colour = col_int) +
  ggplot2::stat_qq_line(colour = "grey40", linewidth = 0.6, linetype = "dashed") +
  ggplot2::xlab("Theoretical Quantiles") +
  ggplot2::ylab("Sample Quantiles") +
  ggplot2::facet_wrap(~ .data$variable, scales = "free_y") +
  theme_clean()

save_figure(fig16, "normality_qq_differences", subfolder = "supplementary")

# =============================================================================
# FIGURE 17: LME residual diagnostics (conditional — model must be present)
# =============================================================================
.lme_model <- results$mixed_models$full$model1 %||% results$mixed_models$model1

if (!is.null(.lme_model)) {
  log_h2("Figure 17: LME residual diagnostics")

  df_diag <- tibble::tibble(
    fitted   = stats::fitted(.lme_model),
    residual = stats::residuals(.lme_model)
  )

  fig17a <- ggplot2::ggplot(df_diag,
      ggplot2::aes(x = .data$fitted, y = .data$residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_point(colour = col_int, size = pt_size, alpha = pt_alpha) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = col_ctl,
                         linewidth = 0.7, formula = y ~ x) +
    ggplot2::xlab("Fitted Values") +
    ggplot2::ylab("Residuals") +
    theme_clean()

  fig17b <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = .data$residual)) +
    ggplot2::stat_qq(size = pt_size, alpha = pt_alpha, colour = col_int) +
    ggplot2::stat_qq_line(colour = "grey40", linewidth = 0.6, linetype = "dashed") +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Sample Quantiles") +
    theme_clean()

  if (length(unique(round(df_diag$residual, 8))) <= 1) {
    log_warn("LME residuals are constant -- Figure 17 skipped (degenerate data).")
  } else {
    fig17 <- if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(fig17a, fig17b, ncol = 2) +
        patchwork::plot_annotation(tag_levels = "A")
    } else {
      fig17a
    }
    save_figure(fig17, "lme_residual_diagnostic", subfolder = "mixed_models",
                width = 9, height = 4.5)
  }
} else {
  log_warn("LME model unavailable \u2014 Figure 17 skipped.")
}

# =============================================================================
# FIGURES 18-19: Time-taken figures (conditional on data + config flag)
# =============================================================================
.has_time_x  <- "time_taken_x_sec" %in% names(dat)
.has_time_y  <- "time_taken_y_sec" %in% names(dat)
.run_time_fig <- isTRUE(cfg_get("optional_analyses", "run_time_analysis", default = TRUE))

if (.run_time_fig && (.has_time_x || .has_time_y)) {

  df_time <- dplyr::bind_rows(
    if (.has_time_x) dplyr::transmute(dat,
      form     = form_x_lbl,
      time_min = .data$time_taken_x_sec / 60,
      score    = .data$x_score_full) else NULL,
    if (.has_time_y) dplyr::transmute(dat,
      form     = form_y_lbl,
      time_min = .data$time_taken_y_sec / 60,
      score    = .data$y_score_full) else NULL
  ) |>
    dplyr::filter(!is.na(.data$time_min))

  form_time_cols <- stats::setNames(c(col_p1, col_p2), c(form_x_lbl, form_y_lbl))

  # FIGURE 18: Violin + boxplot of time taken by form
  log_h2("Figure 18: Time taken by form")

  fig18 <- ggplot2::ggplot(df_time,
      ggplot2::aes(x = .data$form, y = .data$time_min, fill = .data$form)) +
    ggplot2::geom_violin(alpha = 0.25, trim = TRUE, linewidth = 0.4) +
    ggplot2::geom_boxplot(width = 0.16, alpha = 0.6, colour = "grey25",
                          fill  = "white", linewidth = 0.5,
                          outlier.size = pt_size) +
    ggplot2::scale_fill_manual(values = form_time_cols, guide = "none") +
    ggplot2::xlab("Form") +
    ggplot2::ylab("Time Taken (minutes)") +
    theme_clean()

  save_figure(fig18, "time_taken_by_form", subfolder = "descriptive",
              width = 5.5, height = 4.5)

  # FIGURE 19: Time vs Score scatter with regression line
  log_h2("Figure 19: Time vs score scatter")

  fig19 <- ggplot2::ggplot(df_time,
      ggplot2::aes(x = .data$time_min, y = .data$score, colour = .data$form)) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.15,
                         linewidth = 0.8, formula = y ~ x) +
    ggplot2::geom_point(size = pt_size, alpha = pt_alpha) +
    ggplot2::scale_colour_manual(values = form_time_cols, name = "Form") +
    ggplot2::xlab("Time Taken (minutes)") +
    ggplot2::ylab(score_lab) +
    theme_clean() +
    ggplot2::theme(legend.position = "bottom")

  save_figure(fig19, "time_vs_score_correlation", subfolder = "descriptive")

} else if (!.run_time_fig) {
  log_line("Time figures (18-19) skipped: optional_analyses.run_time_analysis = false")
} else {
  log_line("Time figures (18-19) skipped: no time_taken_*_sec columns found in data")
}
# =============================================================================
# FIGURE 20: Ceiling / floor effects visualization
# =============================================================================
.run_ceil <- isTRUE(cfg_get("optional_analyses", "run_ceiling_effects_figure",
                             default = TRUE))

if (.run_ceil) {
  log_h2("Figure 20: Ceiling/floor effects")

  ceil_thresh  <- as.numeric(cfg$analysis$ceiling_threshold %||% (scale_to * 0.95))
  floor_thresh <- as.numeric(cfg$analysis$floor_threshold   %||% (scale_to * 0.05))

  df_ceil <- dplyr::bind_rows(
    dplyr::transmute(dat, group = int_label,  score = .data$intervention_score_full),
    dplyr::transmute(dat, group = ctl_label,  score = .data$control_score_full),
    dplyr::transmute(dat, group = "Period 1", score = .data$period1_score_full),
    dplyr::transmute(dat, group = "Period 2", score = .data$period2_score_full)
  ) |>
    dplyr::mutate(
      group = factor(.data$group,
                     levels = c(ctl_label, int_label, "Period 1", "Period 2")),
      cat   = dplyr::case_when(
        .data$score >= ceil_thresh  ~ "At ceiling",
        .data$score <= floor_thresh ~ "At floor",
        TRUE                        ~ "Middle"
      )
    )

  pct_ceil <- df_ceil |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(
      n_total   = dplyr::n(),
      n_ceil    = sum(.data$cat == "At ceiling"),
      pct_ceil  = 100 * sum(.data$cat == "At ceiling") / dplyr::n(),
      pct_floor = 100 * sum(.data$cat == "At floor")   / dplyr::n(),
      .groups = "drop"
    )

  log_stat("Ceiling effects (% at ceiling per group)",
    as.list(stats::setNames(
      sprintf("%d/%d (%.1f%%)",
              pct_ceil$n_ceil, pct_ceil$n_total, pct_ceil$pct_ceil),
      as.character(pct_ceil$group)
    ))
  )

  fig20 <- ggplot2::ggplot(df_ceil,
      ggplot2::aes(x = .data$group, y = .data$score,
                   colour = .data$cat)) +
    ggplot2::annotate("rect",
                      xmin = -Inf, xmax = Inf,
                      ymin = ceil_thresh, ymax = scale_to,
                      fill = "#D62728", alpha = 0.07) +
    ggplot2::annotate("rect",
                      xmin = -Inf, xmax = Inf,
                      ymin = 0, ymax = floor_thresh,
                      fill = "#1F77B4", alpha = 0.07) +
    ggplot2::geom_hline(yintercept = ceil_thresh,  linetype = "dashed",
                        colour = "#D62728", linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = floor_thresh, linetype = "dashed",
                        colour = "#1F77B4", linewidth = 0.6) +
    ggplot2::geom_jitter(width = 0.18, size = pt_size, alpha = pt_alpha) +
    ggplot2::stat_summary(fun = mean, geom = "crossbar",
                          width = 0.38, colour = "grey20",
                          linewidth = 0.6) +
    ggplot2::scale_colour_manual(
      values = c("At ceiling" = "#D62728",
                 "At floor"   = "#1F77B4",
                 "Middle"     = "grey55"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      name   = score_lab,
      limits = c(score_y_lo, scale_to),
      breaks = score_y_breaks
    ) +
    ggplot2::xlab(NULL) +
    theme_clean() +
    ggplot2::theme(legend.position = "bottom")

  save_figure(fig20, "ceiling_floor_effects", subfolder = "supplementary",
              width = 7, height = 5)
} else {
  log_line("Ceiling/floor figure skipped: optional_analyses.run_ceiling_effects_figure = false")
}

# =============================================================================
# FIGURE 21: Period-specific intervention effect
# Shows the within-person Int−Ctl difference split by WHEN the intervention
# occurred (Period 1 vs Period 2). A significant difference between the two
# boxplots means the effect size is moderated by period order.
# =============================================================================
log_h2("Figure 21: Period-specific intervention effect")

if ("subgroup4" %in% names(dat)) {
  df_period_int <- dat |>
    dplyr::mutate(
      int_minus_ctl = .data$intervention_score_full - .data$control_score_full,
      int_period_lbl = factor(
        dplyr::if_else(.data$intervention_period == 1L,
                       paste0(int_label, " in Period 1\n(", int_label, "-first)"),
                       paste0(int_label, " in Period 2\n(", ctl_label, "-first)")),
        levels = c(paste0(int_label, " in Period 1\n(", int_label, "-first)"),
                   paste0(int_label, " in Period 2\n(", ctl_label, "-first)"))
      )
    )

  mean_by_period_int <- df_period_int |>
    dplyr::group_by(.data$int_period_lbl) |>
    dplyr::summarise(
      mu = mean(.data$int_minus_ctl, na.rm = TRUE),
      se = sd(.data$int_minus_ctl,   na.rm = TRUE) /
             sqrt(sum(!is.na(.data$int_minus_ctl))),
      .groups = "drop"
    )

  fig21 <- ggplot2::ggplot(df_period_int,
      ggplot2::aes(x = .data$int_period_lbl, y = .data$int_minus_ctl,
                   fill = .data$int_period_lbl)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_violin(alpha = 0.2, trim = TRUE, linewidth = 0.3) +
    ggplot2::geom_jitter(ggplot2::aes(colour = .data$int_period_lbl),
                         width = 0.07, size = pt_size, alpha = pt_alpha) +
    ggplot2::geom_crossbar(
      data    = mean_by_period_int,
      ggplot2::aes(x = .data$int_period_lbl, y = .data$mu,
                   ymin = .data$mu - .data$se, ymax = .data$mu + .data$se),
      width = 0.32, alpha = 0.3, colour = "grey25", linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(col_p1, col_p2), guide = "none") +
    ggplot2::scale_colour_manual(
      values = c(col_p1, col_p2), guide = "none") +
    ggplot2::scale_y_continuous(
      name = paste0(int_label, " \u2212 ", ctl_label, " (score difference)")
    ) +
    ggplot2::xlab("When intervention occurred") +
    theme_clean()

  save_figure(fig21, "period_specific_intervention_effect",
              subfolder = "period_effects", width = 7, height = 5)
}

# =============================================================================
# FIGURE 22: 4-subgroup spaghetti (Int vs Ctl for each of the 4 subgroups)
# Each panel = one of the four subgroups.
# This makes cross-subgroup consistency visible at a glance.
# =============================================================================
log_h2("Figure 22: 4-subgroup intervention spaghetti")

if ("subgroup4" %in% names(dat)) {
  df_sg4 <- dat |>
    dplyr::select(participant, subgroup4,
                  int = intervention_score_full,
                  ctl = control_score_full) |>
    tidyr::pivot_longer(c(int, ctl),
                        names_to  = "condition_code",
                        values_to = "score") |>
    dplyr::mutate(
      condition = factor(
        dplyr::if_else(.data$condition_code == "int", int_label, ctl_label),
        levels = c(ctl_label, int_label)
      )
    )

  gmeans_sg4 <- df_sg4 |>
    dplyr::group_by(.data$subgroup4, .data$condition) |>
    dplyr::summarise(
      mu = mean(.data$score, na.rm = TRUE),
      se = sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
      .groups = "drop"
    )

  fig22 <- ggplot2::ggplot(df_sg4,
      ggplot2::aes(x = .data$condition, y = .data$score,
                   colour = .data$condition)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$participant),
                       colour = "grey70", linewidth = 0.35, alpha = 0.65) +
    ggplot2::geom_jitter(width = 0.07, size = pt_size * 0.9, alpha = pt_alpha) +
    ggplot2::geom_point(data = gmeans_sg4,
      ggplot2::aes(x = .data$condition, y = .data$mu),
      shape = 18, size = 4.5
    ) +
    ggplot2::geom_errorbar(data = gmeans_sg4,
      ggplot2::aes(x = .data$condition, y = .data$mu,
                   ymin = .data$mu - .data$se, ymax = .data$mu + .data$se),
      width = eb_width, linewidth = 0.7
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(c(col_ctl, col_int), c(ctl_label, int_label)),
      guide  = "none") +
    ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                                 breaks = score_y_breaks) +
    ggplot2::xlab("Condition") +
    ggplot2::facet_wrap(~ subgroup4, ncol = 2) +
    theme_clean()

  n_sg <- length(unique(dat$subgroup4))
  save_figure(fig22, "subgroup4_intervention_spaghetti",
              subfolder = "supplementary",
              width = 9,
              height = max(5, ceiling(n_sg / 2) * 3.5))
}

# =============================================================================
# FIGURE 23: Ceiling / floor by subgroup4
# Same ceiling/floor plot as Figure 20, but split by 4 subgroups —
# lets you see whether the ceiling effect is driven by a specific subgroup.
# =============================================================================
log_h2("Figure 23: Ceiling/floor by 4 subgroups")

if (.run_ceil && "subgroup4" %in% names(dat)) {
  df_ceil_sg4 <- dplyr::bind_rows(
    dplyr::transmute(dat, subgroup4, group = int_label,
                     score = .data$intervention_score_full),
    dplyr::transmute(dat, subgroup4, group = ctl_label,
                     score = .data$control_score_full)
  ) |>
    dplyr::mutate(
      cat = dplyr::case_when(
        .data$score >= ceil_thresh  ~ "At ceiling",
        .data$score <= floor_thresh ~ "At floor",
        TRUE                        ~ "Middle"
      ),
      panel = paste0(.data$group, "\n(", .data$subgroup4, ")")
    )

  fig23 <- ggplot2::ggplot(df_ceil_sg4,
      ggplot2::aes(x = .data$group, y = .data$score,
                   colour = .data$cat)) +
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = ceil_thresh, ymax = scale_to,
      fill = "#D62728", alpha = 0.07) +
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf, ymin = 0, ymax = floor_thresh,
      fill = "#1F77B4", alpha = 0.07) +
    ggplot2::geom_hline(yintercept = ceil_thresh,  linetype = "dashed",
                        colour = "#D62728", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = floor_thresh, linetype = "dashed",
                        colour = "#1F77B4", linewidth = 0.5) +
    ggplot2::geom_jitter(width = 0.15, size = pt_size, alpha = pt_alpha) +
    ggplot2::stat_summary(fun = mean, geom = "crossbar",
                          width = 0.35, colour = "grey20",
                          linewidth = 0.5) +
    ggplot2::scale_colour_manual(
      values = c("At ceiling" = "#D62728", "At floor" = "#1F77B4",
                 "Middle" = "grey55"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                                 breaks = score_y_breaks) +
    ggplot2::xlab(NULL) +
    ggplot2::facet_wrap(~ subgroup4, ncol = 2) +
    theme_clean() +
    ggplot2::theme(legend.position = "bottom")

  n_sg <- length(unique(dat$subgroup4))
  save_figure(fig23, "ceiling_floor_by_subgroup4",
              subfolder = "supplementary",
              width = 9,
              height = max(5, ceiling(n_sg / 2) * 3.5))
}

# =============================================================================
# FIGURE 24: 4-subgroup delta dotplot
# One lollipop per participant coloured by subgroup — shows whether
# the distribution of Int-minus-Ctl differences clustering by subgroup.
# =============================================================================
log_h2("Figure 24: 4-subgroup delta dotplot")

if ("subgroup4" %in% names(dat)) {
  dat_delta_sg4 <- dat |>
    dplyr::mutate(delta = .data$intervention_score_full - .data$control_score_full) |>
    dplyr::arrange(.data$delta) |>
    dplyr::mutate(rank_id = factor(seq_len(dplyr::n())))

  sg4_colors <- scales::hue_pal()(length(unique(dat_delta_sg4$subgroup4)))
  names(sg4_colors) <- sort(unique(dat_delta_sg4$subgroup4))

  fig24 <- ggplot2::ggplot(dat_delta_sg4,
      ggplot2::aes(x = .data$delta, y = .data$rank_id,
                   colour = .data$subgroup4)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$delta,
                   y = .data$rank_id, yend = .data$rank_id),
      colour = "grey78", linewidth = 0.45
    ) +
    ggplot2::geom_point(size = pt_size, alpha = pt_alpha) +
    ggplot2::scale_colour_manual(values = sg4_colors, name = "Subgroup") +
    ggplot2::scale_x_continuous(
      name = paste0(int_label, " \u2212 ", ctl_label, " (score difference)")
    ) +
    ggplot2::ylab("Participant (sorted by delta)") +
    theme_clean() +
    ggplot2::theme(
      legend.position = "right",
      axis.text.y     = ggplot2::element_blank(),
      axis.ticks.y    = ggplot2::element_blank()
    )

  save_figure(fig24, "subgroup4_delta_dotplot",
              subfolder = "supplementary",
              width = 7.5,
              height = max(4.5, nrow(dat_delta_sg4) * 0.30))
}

# =============================================================================
# FIGURE 25: 2×2 crossover interaction with condition labels on x-axis
# Enhances Figure 12 with explicit condition labels showing WHAT was tested
# in each period for each sequence group.
# =============================================================================
log_h2("Figure 25: 2x2 crossover with condition-period labels")

cell_means_25 <- dat |>
  dplyr::group_by(.data$sequence_group, .data$intervention_period) |>
  dplyr::summarise(
    p1_mean = mean(.data$period1_score_full, na.rm = TRUE),
    p1_se   = sd(.data$period1_score_full,   na.rm = TRUE) / sqrt(dplyr::n()),
    p2_mean = mean(.data$period2_score_full, na.rm = TRUE),
    p2_se   = sd(.data$period2_score_full,   na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    p1_label = dplyr::if_else(
      .data$intervention_period == 1L,
      paste0("P1: ", int_label), paste0("P1: ", ctl_label)),
    p2_label = dplyr::if_else(
      .data$intervention_period == 2L,
      paste0("P2: ", int_label), paste0("P2: ", ctl_label))
  ) |>
  tidyr::pivot_longer(
    cols          = dplyr::matches("^p[12]_(mean|se|label)"),
    names_to      = c("period_code", ".value"),
    names_pattern = "(p[12])_(mean|se|label)"
  ) |>
  dplyr::mutate(
    period_label = factor(.data$label,
                          levels = unique(.data$label[order(.data$period_code)]))
  )

fig25 <- ggplot2::ggplot(cell_means_25,
    ggplot2::aes(x = .data$period_label, y = .data$mean,
                 colour = .data$sequence_group,
                 group  = .data$sequence_group)) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = .data$mean - .data$se, ymax = .data$mean + .data$se),
    width = eb_width, linewidth = 0.8
  ) +
  ggplot2::geom_point(size = 3.8) +
  ggplot2::scale_colour_manual(values = seq_colors, name = "Sequence") +
  ggplot2::scale_y_continuous(name = score_lab, limits = c(score_y_lo, scale_to),
                               breaks = score_y_breaks) +
  ggplot2::xlab("Period (condition assigned)") +
  theme_clean() +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = ggplot2::element_text(size = rel(0.88)))

save_figure(fig25, "crossover_2x2_with_condition_labels",
            subfolder = "period_effects", width = 8, height = 5)

# =============================================================================
# FIGURE 26: Alpha-if-item-deleted (both forms, side-by-side)
# Items are shown in numeric order. Excluded items are labelled with * and
# coloured distinctly. Dashed vertical reference = overall KR-20 alpha.
# =============================================================================
if (!is.null(psych) &&
    !is.null(psych$item_analysis_x) && !is.null(psych$item_analysis_y)) {
  log_h2("Figure 26: Alpha-if-item-deleted")

  # Build per-form data frame, items in numeric order
  .alpha_tbl <- function(ia, rel, form_lbl) {
    alph <- tryCatch(rel$alpha_std %||% rel$kr20, error = function(e) rel$kr20)
    ord  <- order(suppressWarnings(as.numeric(gsub("[^0-9]", "", ia$item))))
    ia_s <- ia[ord, ]
    ia_s |>
      dplyr::mutate(
        item_lbl   = paste0(toupper(.data$item),
                            dplyr::if_else(.data$excluded, "*", "")),
        item_lbl   = factor(.data$item_lbl, levels = unique(.data$item_lbl)),
        form_alpha = alph,
        facet_form = form_lbl
      )
  }

  .d26 <- dplyr::bind_rows(
    .alpha_tbl(psych$item_analysis_x, psych$reliability_x, form_x_lbl),
    .alpha_tbl(psych$item_analysis_y, psych$reliability_y, form_y_lbl)
  ) |>
    dplyr::mutate(facet_form = factor(.data$facet_form,
                                      levels = c(form_x_lbl, form_y_lbl)))

  .has_excl_26 <- any(.d26$excluded, na.rm = TRUE)

  fig26 <- ggplot2::ggplot(.d26,
      ggplot2::aes(x = .data$alpha_if_deleted, y = .data$item_lbl,
                   colour = .data$excluded)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$form_alpha),
                        linetype = "dashed", colour = "grey40", linewidth = 0.6) +
    ggplot2::geom_segment(
      ggplot2::aes(x    = .data$form_alpha, xend = .data$alpha_if_deleted,
                   y    = .data$item_lbl,   yend = .data$item_lbl),
      colour = "grey78", linewidth = 0.45
    ) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_colour_manual(
      values = c(`TRUE` = "#D62728", `FALSE` = col_int),
      labels = c(`TRUE` = "Excluded (*)", `FALSE` = "Included"),
      name   = NULL
    ) +
    ggplot2::scale_x_continuous(name = "KR-20 \u03b1 if Item Deleted") +
    ggplot2::ylab("Item") +
    ggplot2::facet_wrap(~ facet_form, scales = "free_y") +
    theme_clean() +
    ggplot2::theme(legend.position = if (.has_excl_26) "bottom" else "none")

  if (.has_excl_26) {
    fig26 <- fig26 +
      ggplot2::labs(caption = "* Item excluded from restricted scoring")
  }

  .n_items_26 <- max(nrow(psych$item_analysis_x), nrow(psych$item_analysis_y))
  save_figure(fig26, "alpha_if_deleted", subfolder = "item_analysis",
              width  = 10,
              height = max(5, .n_items_26 * 0.42 + 1.5))
} else {
  log_warn("Psychometrics unavailable \u2014 Figure 26 (alpha-if-deleted) skipped.")
}

# =============================================================================
# FIGURE 27: Inter-item correlation heatmaps (one PNG per form)
# Cells labelled with Pearson r. Excluded items marked with * on axes.
# =============================================================================
.raw_data_27 <- tryCatch(load_rds("raw_data"), error = function(e) NULL)

if (!is.null(.raw_data_27)) {
  log_h2("Figure 27: Inter-item correlation heatmap")

  .corr_heatmap <- function(items_df, item_cols, excl_cols, axis_lbl,
                             excl_always = NULL) {
    # item_cols   : ALL item columns for the form (including scoring-excluded ones).
    # excl_cols   : items excluded from restricted scoring in this run.
    # excl_always : (optional) subset of excl_cols excluded in ALL comparison
    #               variants. When provided, these items are marked "**" in axis
    #               labels and cell annotations; remaining excl_cols items get "*".
    #               When NULL (default / single-run mode), all excl_cols get "*".

    mat <- as.matrix(dplyr::select(items_df, dplyr::all_of(item_cols)))
    storage.mode(mat) <- "numeric"

    # Detect constant (zero-variance) items: Pearson r is undefined for these.
    item_var  <- apply(mat, 2, var,  na.rm = TRUE)
    item_mean <- apply(mat, 2, mean, na.rm = TRUE)
    zv_cols   <- names(item_var)[!is.na(item_var) & item_var == 0]
    na_v_cols <- names(item_var)[ is.na(item_var)]
    const_cols <- union(zv_cols, na_v_cols)

    # Resolve two-level exclusion sets.
    # excl_always_eff : items marked ** (excluded in all comparison variants).
    # excl_strict_eff : items marked *  (excluded only in the stricter variant).
    excl_always_eff <- if (!is.null(excl_always)) excl_always else character(0)
    excl_strict_eff <- setdiff(excl_cols, excl_always_eff)
    # Two-level mode is active when both sets are non-empty and excl_always was
    # explicitly provided; single-run mode uses * for everything.
    two_level <- !is.null(excl_always) &&
                 length(excl_always_eff) > 0 &&
                 length(excl_strict_eff) > 0

    # Sort items numerically for consistent axis ordering.
    ord          <- order(suppressWarnings(as.numeric(gsub("[^0-9]", "", item_cols))))
    ordered_cols <- item_cols[ord]
    n_items      <- length(ordered_cols)

    # Axis labels:
    #   two-level mode : ** for excl_always_eff, * for excl_strict_eff
    #   single-run mode: *  for all excl_cols
    make_lbl <- function(col) {
      if (!is.null(excl_always) && col %in% excl_always_eff)
        paste0(toupper(col), "**")
      else if (col %in% excl_cols)
        paste0(toupper(col), "*")
      else
        toupper(col)
    }
    lbl_cols <- vapply(ordered_cols, make_lbl, character(1))

    # Compute correlation matrix for all items.
    # Constant items produce NA correlations — handled explicitly below.
    all_mat <- as.matrix(dplyr::select(items_df, dplyr::all_of(ordered_cols)))
    storage.mode(all_mat) <- "numeric"
    cmat <- suppressWarnings(cor(all_mat, use = "pairwise.complete.obs"))

    # Build long-format data frame for ggplot.
    long <- expand.grid(
      row = ordered_cols, col = ordered_cols,
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(
        value    = as.vector(cmat),
        on_diag  = .data$row == .data$col,
        # A cell is "constant" when at least one member item is constant
        # (off-diagonal only — diagonal is always grey regardless).
        is_const = !.data$on_diag &
                     (.data$row %in% const_cols | .data$col %in% const_cols),
        # Fill: NA makes tile grey (via na.value); numeric drives the colour scale.
        r_fill   = dplyr::case_when(
          .data$on_diag  ~ NA_real_,
          .data$is_const ~ NA_real_,
          TRUE           ~ .data$value
        ),
        # Exclusion suffix appended to numeric cell values.
        # Strongest marker wins: ** if either item is always-excluded,
        # else * if either item is strict-only excluded.
        excl_sfx = dplyr::case_when(
          .data$on_diag | .data$is_const                                          ~ "",
          .data$row %in% excl_always_eff | .data$col %in% excl_always_eff        ~ "**",
          .data$row %in% excl_strict_eff | .data$col %in% excl_strict_eff        ~ "*",
          TRUE                                                                    ~ ""
        ),
        # Cell text: blank on diagonal, "C" for constant pairs,
        # numeric (with exclusion suffix) otherwise.
        cell_text = dplyr::case_when(
          .data$on_diag  ~ "",
          .data$is_const ~ "C",
          TRUE           ~ paste0(sprintf("%.2f", .data$value), .data$excl_sfx)
        ),
        row_lbl = factor(
          vapply(.data$row, make_lbl, character(1)),
          levels = rev(lbl_cols)   # top-to-bottom = first item to last item
        ),
        col_lbl = factor(
          vapply(.data$col, make_lbl, character(1)),
          levels = lbl_cols        # left-to-right = first item to last item
        )
      )

    # Colour scale limits from estimable (non-diagonal, non-constant) pairs.
    valid_vals <- long$value[!long$on_diag & !long$is_const]
    lim <- max(abs(valid_vals), na.rm = TRUE)
    lim <- if (is.finite(lim) && lim > 0) lim else 0.05
    lim <- max(lim, 0.05)

    # Degenerate: no estimable pairs at all.
    if (!any(!long$on_diag & !long$is_const)) {
      msg <- paste0(
        "No estimable correlations \u2014 all items are constant.\n",
        "C = Pearson r undefined (constant item).")
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                          hjust = 0.5, vjust = 0.5, size = 3.5) +
        ggplot2::theme_void()
      return(list(plot = p, n_items = n_items))
    }

    # ---- Build caption / legend key ------------------------------------- #
    key_lines <- character(0)
    if (two_level) {
      key_lines <- c(key_lines,
        "** excluded in both restricted variants",
        "*  excluded only in the stricter restricted variant")
    } else if (length(excl_always_eff) > 0) {
      # All excluded items are in excl_always (** markers); excl_strict is empty.
      key_lines <- c(key_lines, "** excluded from restricted scoring")
    } else if (length(excl_cols) > 0) {
      key_lines <- c(key_lines, "* excluded from restricted scoring")
    }
    if (length(const_cols) > 0) {
      key_lines <- c(key_lines,
        "C = Pearson r undefined because at least one item is constant")
    }
    cap_full    <- if (length(key_lines) > 0) paste(key_lines, collapse = "\n") else NULL
    scoring_lbl <- if (length(excl_cols) > 0) {
      paste0("Restricted scoring: ",
             paste(toupper(excl_cols), collapse = " and "), " excluded")
    } else {
      NULL
    }

    p <- ggplot2::ggplot(long,
        ggplot2::aes(x = .data$col_lbl, y = .data$row_lbl, fill = .data$r_fill)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
      ggplot2::geom_text(
        data = dplyr::filter(long, !.data$on_diag),
        ggplot2::aes(x = .data$col_lbl, y = .data$row_lbl,
                     label = .data$cell_text),
        size = 2.5, colour = "grey10", inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_gradient2(
        low      = "#B22222",
        mid      = "white",
        high     = "#2E8B57",
        midpoint = 0,
        limits   = c(-lim, lim),
        name     = "r",
        na.value = "grey93",
        breaks   = round(seq(-lim, lim, length.out = 5), 2)
      ) +
      ggplot2::scale_x_discrete(name = axis_lbl) +
      ggplot2::scale_y_discrete(name = axis_lbl) +
      theme_clean() +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y     = ggplot2::element_text(size = 8),
        legend.position = "right",
        panel.grid      = ggplot2::element_blank(),
        plot.subtitle   = ggplot2::element_text(size = 8, colour = "grey30",
                                                margin = ggplot2::margin(b = 4)),
        plot.caption    = ggplot2::element_text(size = 7, hjust = 0,
                                                lineheight = 1.2,
                                                colour = "grey40")
      )

    # In two-level mode the axis/cell markers already convey the exclusion
    # context; suppress the "Restricted scoring: ..." subtitle so the figure
    # reads as a neutral cross-run descriptive rather than a single-run output.
    .subtitle_lbl <- if (two_level) NULL else scoring_lbl
    if (!is.null(.subtitle_lbl) || !is.null(cap_full))
      p <- p + ggplot2::labs(subtitle = .subtitle_lbl, caption = cap_full)

    list(plot = p, n_items = n_items)
  }

  .sz <- function(n) max(4.5, n * 0.65)

  # Read cross-run exclusion context injected by 08_comparison_figures.R.
  # HEATMAP_EXCL_ALWAYS_X / _Y : comma-separated item names excluded in ALL
  # comparison variants; these receive "**" markers instead of "*".
  # When the env var is absent (normal single-run invocation), excl_always = NULL
  # and the function falls back to single-level "*" markers for all excl_cols.
  .parse_env_excl <- function(var) {
    ev <- Sys.getenv(var, "")
    if (nzchar(ev)) trimws(strsplit(ev, ",")[[1]]) else NULL
  }
  .excl_always_x_env <- .parse_env_excl("HEATMAP_EXCL_ALWAYS_X")
  .excl_always_y_env <- .parse_env_excl("HEATMAP_EXCL_ALWAYS_Y")

  fig27x_res <- .corr_heatmap(.raw_data_27$x_items,
                               .raw_data_27$x_cols_full,
                               .raw_data_27$x_excluded,
                               paste0(form_x_lbl, " Item"),
                               excl_always = .excl_always_x_env)
  .nx_shown  <- fig27x_res$n_items
  save_figure(fig27x_res$plot, "item_intercorrelation_heatmap_x",
              subfolder = "item_analysis",
              width = .sz(.nx_shown) + 1.5, height = .sz(.nx_shown))

  fig27y_res <- .corr_heatmap(.raw_data_27$y_items,
                               .raw_data_27$y_cols_full,
                               .raw_data_27$y_excluded,
                               paste0(form_y_lbl, " Item"),
                               excl_always = .excl_always_y_env)
  .ny_shown  <- fig27y_res$n_items
  save_figure(fig27y_res$plot, "item_intercorrelation_heatmap_y",
              subfolder = "item_analysis",
              width = .sz(.ny_shown) + 1.5, height = .sz(.ny_shown))
} else {
  log_warn("raw_data unavailable \u2014 Figure 27 (inter-item correlation heatmap) skipped.")
}

# =============================================================================
# FIGURE 28: Item response matrix heatmap (exploratory)
# Participant \u00d7 item binary response grid, sorted by total score descending.
# Excluded items marked * on x-axis. One PNG per form.
# =============================================================================
if (!is.null(.raw_data_27)) {
  log_h2("Figure 28: Item response matrix")

  .resp_matrix <- function(items_df, cols_full, excl_cols, form_lbl,
                            excl_always = NULL) {
    # excl_always : (optional) subset of excl_cols excluded in ALL comparison
    #   variants — these receive "**" axis markers.  Remaining excl_cols items
    #   receive "*".  When NULL (single-run mode) all excl_cols receive "*".
    #   Mirrors the two-level logic in .corr_heatmap().
    .num_ord <- suppressWarnings(as.numeric(sub("^[xy]", "", cols_full)))
    .col_ord <- cols_full[order(.num_ord)]

    # Two-level exclusion sets.
    .ea28     <- if (!is.null(excl_always)) excl_always else character(0)
    .es28     <- setdiff(excl_cols, .ea28)
    .two28    <- !is.null(excl_always) && length(.ea28) > 0 && length(.es28) > 0

    # Axis labels with ** / * suffixes.
    .col_lbl <- vapply(.col_ord, function(col) {
      if (!is.null(excl_always) && col %in% .ea28) paste0(toupper(col), "**")
      else if (col %in% excl_cols)                 paste0(toupper(col), "*")
      else                                          toupper(col)
    }, character(1))

    .part_ord <- .part_levels(as.character(items_df$participant))

    long <- items_df |>
      dplyr::mutate(participant = factor(as.character(.data$participant),
                                         levels = .part_ord)) |>
      dplyr::select(participant, dplyr::all_of(.col_ord)) |>
      tidyr::pivot_longer(dplyr::all_of(.col_ord),
                          names_to  = ".raw_item",
                          values_to = "response") |>
      dplyr::mutate(
        item    = factor(.col_lbl[match(.data$.raw_item, .col_ord)],
                         levels = .col_lbl),
        correct = dplyr::case_when(
          is.na(.data$response) ~ "Missing",
          .data$response == 1   ~ "Correct",
          TRUE                  ~ "Incorrect"
        )
      )

    n_p    <- length(.part_ord)
    ytxt   <- max(5L, 9L - n_p %/% 4L)

    # Caption — two-level when excl_always provided and both sets non-empty.
    .cap28 <- if (.two28) {
      paste0("** excluded in both restricted variants\n",
             "*  excluded only in the stricter restricted variant")
    } else if (length(.ea28) > 0) {
      "** excluded from restricted scoring"
    } else if (length(excl_cols) > 0) {
      "* Item excluded from restricted scoring"
    } else {
      NULL
    }

    ggplot2::ggplot(long, ggplot2::aes(x = .data$item, y = .data$participant,
                                        fill = .data$correct)) +
      ggplot2::geom_tile(colour = "grey80", linewidth = 0.2) +
      ggplot2::scale_fill_manual(
        values = c("Correct" = "#2E8B57", "Incorrect" = "white",
                   "Missing" = "#D3D3D3"),
        name = NULL
      ) +
      ggplot2::labs(
        x       = NULL,
        y       = "Participant",
        title   = paste0(form_lbl, ": Item Response Pattern"),
        caption = .cap28
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y     = ggplot2::element_text(size = ytxt),
        panel.grid      = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  }

  .ni_x28 <- length(.raw_data_27$x_cols_full)
  .ni_y28 <- length(.raw_data_27$y_cols_full)
  .np_x28 <- nrow(.raw_data_27$x_items)
  .np_y28 <- nrow(.raw_data_27$y_items)

  fig28x <- .resp_matrix(.raw_data_27$x_items, .raw_data_27$x_cols_full,
                          .raw_data_27$x_excluded, form_x_lbl, .excl_always_x_env)
  save_figure(fig28x, "item_response_matrix_x",
              subfolder = "exploratory",
              width  = max(5, .ni_x28 * 0.55) + 1.5,
              height = max(4, .np_x28 * 0.30) + 1.5)

  fig28y <- .resp_matrix(.raw_data_27$y_items, .raw_data_27$y_cols_full,
                          .raw_data_27$y_excluded, form_y_lbl, .excl_always_y_env)
  save_figure(fig28y, "item_response_matrix_y",
              subfolder = "exploratory",
              width  = max(5, .ni_y28 * 0.55) + 1.5,
              height = max(4, .np_y28 * 0.30) + 1.5)
} else {
  log_warn("raw_data unavailable \u2014 Figure 28 (item response matrix) skipped.")
}

# =============================================================================
# FIGURE 29: Item endorsement rates by sequence group (exploratory)
# % correct per item split by sequence group. Both forms faceted.
# =============================================================================
if (!is.null(.raw_data_27)) {
  log_h2("Figure 29: Item endorsement by sequence group")

  .seq_map_29 <- dplyr::distinct(dat,
    participant    = as.character(.data$participant),
    sequence_group = .data$sequence_group
  )

  .endorse_long_29 <- function(items_df, cols_full, excl_cols, form_lbl,
                               excl_always = NULL) {
    # excl_always : same two-level logic as .resp_matrix() / .corr_heatmap().
    .num_ord <- suppressWarnings(as.numeric(sub("^[xy]", "", cols_full)))
    .col_ord <- cols_full[order(.num_ord)]
    .ea29     <- if (!is.null(excl_always)) excl_always else character(0)
    .col_lbl <- vapply(.col_ord, function(col) {
      if (!is.null(excl_always) && col %in% .ea29) paste0(toupper(col), "**")
      else if (col %in% excl_cols)                 paste0(toupper(col), "*")
      else                                          toupper(col)
    }, character(1))

    items_df |>
      dplyr::mutate(participant = as.character(.data$participant)) |>
      dplyr::select(participant, dplyr::all_of(.col_ord)) |>
      tidyr::pivot_longer(dplyr::all_of(.col_ord),
                          names_to  = ".raw_item",
                          values_to = "response") |>
      dplyr::mutate(
        item = factor(.col_lbl[match(.data$.raw_item, .col_ord)],
                      levels = .col_lbl),
        form = form_lbl
      ) |>
      dplyr::left_join(.seq_map_29, by = "participant")
  }

  .long29 <- dplyr::bind_rows(
    .endorse_long_29(.raw_data_27$x_items, .raw_data_27$x_cols_full,
                     .raw_data_27$x_excluded, form_x_lbl, .excl_always_x_env),
    .endorse_long_29(.raw_data_27$y_items, .raw_data_27$y_cols_full,
                     .raw_data_27$y_excluded, form_y_lbl, .excl_always_y_env)
  )

  .summ29 <- .long29 |>
    dplyr::filter(!is.na(.data$response), !is.na(.data$sequence_group)) |>
    dplyr::group_by(.data$form, .data$item, .data$sequence_group) |>
    dplyr::summarise(
      pct = mean(.data$response, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  .seq_lvls29 <- sort(unique(.summ29$sequence_group))
  .seq_cols29 <- stats::setNames(
    c(col_int, col_ctl, "#8B6914", "#2F4F8F")[seq_along(.seq_lvls29)],
    .seq_lvls29
  )
  has_excl29     <- length(.raw_data_27$x_excluded) > 0 ||
                    length(.raw_data_27$y_excluded) > 0
  # Two-level caption for Figure 29 — combine X and Y exclusion contexts.
  .ea29_x <- if (!is.null(.excl_always_x_env)) .excl_always_x_env else character(0)
  .ea29_y <- if (!is.null(.excl_always_y_env)) .excl_always_y_env else character(0)
  .es29_x <- setdiff(.raw_data_27$x_excluded, .ea29_x)
  .es29_y <- setdiff(.raw_data_27$y_excluded, .ea29_y)
  .two29  <- (!is.null(.excl_always_x_env) || !is.null(.excl_always_y_env)) &&
             (length(.ea29_x) > 0 || length(.ea29_y) > 0) &&
             (length(.es29_x) > 0 || length(.es29_y) > 0)
  .cap29  <- if (.two29) {
    paste0("** excluded in both restricted variants\n",
           "*  excluded only in the stricter restricted variant")
  } else if (length(.ea29_x) > 0 || length(.ea29_y) > 0) {
    "** excluded from restricted scoring"
  } else if (has_excl29) {
    "* Item excluded from restricted scoring"
  } else {
    NULL
  }
  .n_items_max29 <- max(length(.raw_data_27$x_cols_full),
                        length(.raw_data_27$y_cols_full))

  fig29 <- ggplot2::ggplot(.summ29,
      ggplot2::aes(x = .data$item, y = .data$pct,
                   fill = .data$sequence_group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.65) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                        colour = "grey50", linewidth = 0.4) +
    ggplot2::facet_wrap(~ form, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                labels = function(x) paste0(x, "%")) +
    ggplot2::scale_fill_manual(values = .seq_cols29, name = "Sequence group") +
    ggplot2::labs(
      x       = NULL,
      y       = "% Correct",
      caption = .cap29
    ) +
    theme_clean() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      strip.text      = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  save_figure(fig29, "item_endorsement_by_sequence",
              subfolder = "exploratory",
              width  = max(6, .n_items_max29 * 0.65) + 2,
              height = 8)
} else {
  log_warn("raw_data unavailable \u2014 Figure 29 (item endorsement by sequence) skipped.")
}

# =============================================================================
# FIGURE S1: Participant flow diagram (CONSORT-style crossover design)
# =============================================================================
log_h2("Figure S1: Participant flow diagram")

.n_s1     <- nrow(dat)
.n_int_s1 <- sum(dat$intervention_period == 1L, na.rm = TRUE)   # intervention-first
.n_ctl_s1 <- sum(dat$intervention_period == 2L, na.rm = TRUE)   # control-first

.dot_s1 <- paste0(
  'digraph crossover_flow {
    graph [layout = dot, rankdir = TB, fontname = "Arial",
           nodesep = 0.4, ranksep = 0.5]
    node  [shape = box, style = "rounded,filled", fillcolor = white,
           fontname = "Arial", fontsize = 14, margin = "0.15,0.10"]
    edge  [arrowsize = 0.8]

    A  [label = "Participants (n=', .n_s1, ')"]
    B  [label = "Randomized 1:1 to order"]
    C  [label = "Lecture (60 min)"]
    D  [label = "', int_label, ' guidance (5-10 min)"]
    E  [label = "Break (10-15 min)"]
    F1 [label = "', ctl_label, ' first (n=', .n_ctl_s1, ')"]
    F2 [label = "', int_label, ' first (n=', .n_int_s1, ')"]
    G1 [label = "', ctl_label, ' study (20 min)"]
    G2 [label = "', int_label, ' study (20 min)"]
    H1 [label = "Post-test 1 (X or Y)"]
    H2 [label = "Post-test 1 (X or Y)"]
    I1 [label = "Break (10-15 min)"]
    I2 [label = "Break (10-15 min)"]
    J1 [label = "', int_label, ' study (20 min)"]
    J2 [label = "', ctl_label, ' study (20 min)"]
    K1 [label = "Post-test 2 (alternate form)"]
    K2 [label = "Post-test 2 (alternate form)"]

    A -> B -> C -> D -> E
    E -> F1
    E -> F2
    F1 -> G1 -> H1 -> I1 -> J1 -> K1
    F2 -> G2 -> H2 -> I2 -> J2 -> K2
  }'
)

if (requireNamespace("DiagrammeR",  quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  .s1_widget <- tryCatch(
    DiagrammeR::grViz(.dot_s1),
    error = function(e) {
      log_warn("Figure S1 grViz render failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(.s1_widget)) {
    .s1_path <- out_path(
      Sys.getenv("FIGURES_ROOT_SUFFIX", unset = "figures"),
      "supplementary", "figure_s1_participant_flow.png"
    )
    dir.create(dirname(.s1_path), recursive = TRUE, showWarnings = FALSE)

    .s1_saved <- tryCatch({
      .tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(.s1_widget, .tmp_html, selfcontained = FALSE)
      on.exit(unlink(.tmp_html), add = TRUE)
      on.exit(unlink(sub("\\.html$", "_files", .tmp_html), recursive = TRUE), add = TRUE)

      if (requireNamespace("webshot2", quietly = TRUE)) {
        webshot2::webshot(.tmp_html, file = .s1_path,
                          vwidth = 700, vheight = 1050, zoom = 2, delay = 0.5)
      } else if (requireNamespace("webshot", quietly = TRUE)) {
        webshot::webshot(.tmp_html, file = .s1_path,
                         vwidth = 700, vheight = 1050, zoom = 2, delay = 0.5)
      } else {
        stop("Neither webshot2 nor webshot is installed.")
      }
      TRUE
    }, error = function(e) {
      log_warn("Figure S1 PNG export failed: ", conditionMessage(e))
      FALSE
    })

    if (isTRUE(.s1_saved) && file.exists(.s1_path)) {
      # Trim excess whitespace left by the browser viewport
      if (requireNamespace("magick", quietly = TRUE)) {
        tryCatch({
          .img <- magick::image_read(.s1_path)
          .img <- magick::image_trim(.img)
          .img <- magick::image_border(.img, "white", "30x30")
          magick::image_write(.img, .s1_path)
        }, error = function(e) {
          log_warn("Figure S1 trim step failed: ", conditionMessage(e))
        })
      }
      sz_kb <- tryCatch(round(file.size(.s1_path) / 1024, 1), error = function(e) NA_real_)
      log_line("Figure saved : figures/supplementary/figure_s1_participant_flow.png")
      log_line("             : n=", .n_s1,
               " | ", ctl_label, "-first n=", .n_ctl_s1,
               " | ", int_label, "-first n=", .n_int_s1,
               " | ", sz_kb, " KB")
    }
  }

} else {
  log_warn("Figure S1 skipped: DiagrammeR and/or htmlwidgets not installed.")
  log_warn("         Install via: install.packages(c('DiagrammeR', 'htmlwidgets'))")
}

log_h2("FIGURES COMPLETE")