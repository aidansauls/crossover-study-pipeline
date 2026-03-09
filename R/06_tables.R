## =============================================================================
## R/06_tables.R
## All publication tables — CSV + PNG output.
## Tables have no embedded titles (add in manuscript).
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
log_h1("06  TABLES")

cfg     <- read_config()
dat     <- load_rds("analysis_data")
results <- load_rds("analysis_results")
psych   <- load_rds("psychometrics")

int_label  <- cfg$study$intervention_label %||% "Intervention"
ctl_label  <- cfg$study$control_label      %||% "Control"
form_x_lbl <- cfg$study$form_x_label       %||% "Form X"
form_y_lbl <- cfg$study$form_y_label       %||% "Form Y"
scale_to   <- as.numeric(cfg$scores$scale_to %||% 10)

# =============================================================================
# TABLE 1: Participant Flow & Sequence Allocation
# =============================================================================
log_h2("Table 1: Participant flow")

tbl1 <- tibble::tibble(
  Item  = c(
    "Total enrolled",
    paste0(int_label, "-first (sequence AB)"),
    paste0(ctl_label, "-first (sequence BA)")
  ),
  n = c(
    results$n,
    sum(dat$intervention_period == 1, na.rm = TRUE),
    sum(dat$intervention_period == 2, na.rm = TRUE)
  )
) |>
  dplyr::mutate(
    `%` = dplyr::case_when(
      .data$n == results$n ~ 100,
      TRUE ~ round(.data$n / results$n * 100, 1)
    )
  )

save_table(tbl1, "01_participant_flow", subfolder = "descriptive",
           caption = "Participant flow and sequence allocation")

# =============================================================================
# TABLE 2: Descriptive Statistics
# =============================================================================
log_h2("Table 2: Descriptive statistics")

desc_labels <- c(
  intervention_score_full       = paste0(int_label, " score (full)"),
  control_score_full            = paste0(ctl_label, " score (full)"),
  period1_score_full            = "Period 1 score (full)",
  period2_score_full            = "Period 2 score (full)",
  x_score_full                  = paste0(form_x_lbl, " score (full)"),
  y_score_full                  = paste0(form_y_lbl, " score (full)"),
  intervention_score_restricted = paste0(int_label, " score (restricted)"),
  control_score_restricted      = paste0(ctl_label, " score (restricted)"),
  period1_score_restricted      = "Period 1 score (restricted)",
  period2_score_restricted      = "Period 2 score (restricted)"
)

tbl2 <- results$descriptives |>
  dplyr::mutate(
    `Measure` = dplyr::recode(.data$Measure, !!!desc_labels),
    `Mean (SD)` = paste0(round(.data$Mean, 2), " (", round(.data$SD, 2), ")"),
    `Median [IQR]` = paste0(round(.data$Median, 2),
                             " [", round(.data$IQR_lo, 2),
                             ", ", round(.data$IQR_hi, 2), "]"),
    `Range` = paste0(round(.data$Min, 2), "–", round(.data$Max, 2)),
    `Ceiling %` = paste0(round(.data$`Ceiling (perfect)` * 100, 1), "%"),
    `Floor %`   = paste0(round(.data$`Floor (zero)` * 100, 1), "%")
  ) |>
  dplyr::select(Measure, N, `Mean (SD)`, `Median [IQR]`, Range,
                `Ceiling %`, `Floor %`)

save_table(tbl2, "02_descriptive_statistics", subfolder = "descriptive",
           caption = "Descriptive statistics by condition and period")

# =============================================================================
# TABLE 3: Primary Contrasts (intervention vs control + period effects)
# =============================================================================
log_h2("Table 3: Primary contrasts")

format_contrast <- function(res, label) {
  if (is.null(res)) return(NULL)
  tibble::tibble(
    Contrast    = label,
    N           = res$n,
    `Mean A (SD)` = paste0(round(res$mean_a, 2), " (", round(res$sd_a, 2), ")"),
    `Mean B (SD)` = paste0(round(res$mean_b, 2), " (", round(res$sd_b, 2), ")"),
    `Mean Diff`   = round(res$mean_diff, 3),
    `95% CI`      = fmt_ci(res$ci_lo, res$ci_hi),
    `Cohen dz`    = round(res$dz, 3),
    `p`           = ifelse(!is.na(res$p), fmt_p(res$p), "NA")
  )
}

tbl3_rows <- list(
  format_contrast(results$contrast_intervention_full,
    paste0(int_label, " vs ", ctl_label, " (full scoring; A=", int_label, ")")),
  format_contrast(results$contrast_period_full,
    "Period 2 vs Period 1 (full scoring; A=Period 2)"),
  format_contrast(results$contrast_intervention_restr,
    paste0(int_label, " vs ", ctl_label, " (restricted scoring)")),
  format_contrast(results$contrast_period_restr,
    "Period 2 vs Period 1 (restricted scoring)")
)

tbl3 <- dplyr::bind_rows(tbl3_rows[!sapply(tbl3_rows, is.null)])

save_table(tbl3, "03_primary_contrasts", subfolder = "primary",
           caption = "Paired contrasts: intervention effect and period effect")

# =============================================================================
# TABLE 4: Carryover & Sequence x Period
# =============================================================================
log_h2("Table 4: Carryover and sequence x period")

format_carryover <- function(res, label) {
  if (is.null(res)) return(NULL)
  tibble::tibble(
    Test    = label,
    `Int-first Period 1 M (SD)` =
      paste0(round(res$mean_a, 2), " (", round(res$sd_a, 2), ")"),
    `Int-first n_a` = res$n_a,
    `Ctl-first Period 1 M (SD)` =
      paste0(round(res$mean_b, 2), " (", round(res$sd_b, 2), ")"),
    `Ctl-first n_b` = res$n_b,
    `t`  = ifelse(!is.na(res$t), round(res$t, 3), "NA"),
    `p`  = ifelse(!is.na(res$p), fmt_p(res$p), "NA"),
    Interpretation = res$interpretation
  )
}

format_seq_period <- function(res, label) {
  if (is.null(res)) return(NULL)
  tibble::tibble(
    Test    = label,
    `AB Mean Diff (SD)` =
      paste0(round(res$mean_diff_ab, 2), " (", round(res$sd_diff_ab, 2), ")"),
    `AB n` = res$n_ab,
    `BA Mean Diff (SD)` =
      paste0(round(res$mean_diff_ba, 2), " (", round(res$sd_diff_ba, 2), ")"),
    `BA n` = res$n_ba,
    `t`    = ifelse(!is.na(res$t), round(res$t, 3), "NA"),
    `p`    = ifelse(!is.na(res$p), fmt_p(res$p), "NA"),
    Interpretation = res$interpretation
  )
}

tbl4_carry <- dplyr::bind_rows(
  format_carryover(results$carryover_full,  "Grizzle carryover (full)"),
  format_carryover(results$carryover_restr, "Grizzle carryover (restricted)")
)

save_table(tbl4_carry, "04a_carryover_test", subfolder = "period_effects",
           caption = "Grizzle carryover test: Period-1 scores by sequence group")

tbl4_seq <- dplyr::bind_rows(
  format_seq_period(results$seq_period_full,  "Sequence x Period interaction (full)"),
  format_seq_period(results$seq_period_restr, "Sequence x Period interaction (restricted)")
)

save_table(tbl4_seq, "04b_sequence_period_interaction", subfolder = "period_effects",
           caption = "Sequence x period interaction: Period 2 - Period 1 by sequence")

# =============================================================================
# TABLE 5: Item Analysis
# (See also 03_psychometrics.R which saves its own item analysis table)
# This version is a simplified 2-page summary with flags highlighted.
# =============================================================================
log_h2("Table 5: Item analysis summary")

tbl5 <- psych$item_analysis |>
  dplyr::transmute(
    Form          = .data$form,
    Item          = toupper(.data$item),
    N             = .data$n,
    `P Correct`   = round(.data$p_correct, 3),
    `Difficulty`  = .data$difficulty_cat,
    `Item-Rest r` = round(.data$item_rest_corr, 3),
    `Alpha-if-Del`= round(.data$alpha_if_deleted, 3),
    Excluded      = .data$excluded,
    Note          = .data$flag
  )

save_table(tbl5, "05_item_analysis_summary", subfolder = "psychometrics",
           caption = "Item-level difficulty and discrimination")

# =============================================================================
# TABLE 6: Reliability
# =============================================================================
log_h2("Table 6: Reliability")

fmt_omega <- function(x) if (!is.na(x) && !is.null(x)) round(x, 3) else "—"

make_rel_row <- function(rel) {
  tibble::tibble(
    Form              = rel$label,
    N                 = rel$n,
    Items             = rel$k,
    `KR-20`           = round(rel$kr20, 3),
    `alpha (std)`     = round(rel$alpha_std, 3),
    `omega_t`         = fmt_omega(rel$omega_t),
    `omega_h`         = fmt_omega(rel$omega_h),
    `Split-half (SB)` = fmt_omega(rel$split_half_corrected)
  )
}

tbl6 <- dplyr::bind_rows(
  make_rel_row(psych$reliability_x),
  make_rel_row(psych$reliability_y),
  if (!identical(psych$reliability_x, psych$reliability_x_restricted))
    make_rel_row(psych$reliability_x_restricted) else NULL,
  if (!identical(psych$reliability_y, psych$reliability_y_restricted))
    make_rel_row(psych$reliability_y_restricted) else NULL
)

save_table(tbl6, "06_reliability_summary", subfolder = "psychometrics",
           caption = "Internal consistency: KR-20, alpha, omega, split-half")

# =============================================================================
# TABLE 7: Mixed-effects model results
# =============================================================================
log_h2("Table 7: Mixed-effects model")

if (!is.null(results$mixed_models$full$model1)) {
  
  fmt_model_coefs <- function(model, scoring_label) {
    coefs <- as.data.frame(summary(model)$coefficients)
    coefs$term    <- rownames(coefs)
    coefs$scoring <- scoring_label
    tibble::tibble(
      Scoring   = scoring_label,
      Term      = coefs$term,
      Estimate  = round(coefs$Estimate, 3),
      SE        = round(coefs$`Std. Error`, 3),
      df        = round(coefs$df, 1),
      `t value` = round(coefs[["t value"]] %||% coefs$t.ratio, 3),
      `p`       = sapply(coefs$`Pr(>|t|)` %||% coefs$p.value, fmt_p)
    )
  }
  
  tbl7 <- dplyr::bind_rows(
    fmt_model_coefs(results$mixed_models$full$model1, "Full"),
    if (!is.null(results$mixed_models$restricted$model1))
      fmt_model_coefs(results$mixed_models$restricted$model1, "Restricted")
    else NULL
  )
  
  save_table(tbl7, "07_mixed_model_results", subfolder = "mixed_models",
             caption = "Linear mixed-effects model: condition + period + sequence (1|participant)")
  
} else {
  log_warn("Mixed model not available \u2014 Table 7 skipped.")
}

# =============================================================================
# TABLE 8: Effect size summary (all contrasts, dz + approx CI + magnitude)
# =============================================================================
log_h2("Table 8: Effect size summary")

ci_level_tbl <- as.numeric(cfg$figures$ci_level %||% 0.95)
ci_mult_tbl  <- stats::qnorm(0.5 + ci_level_tbl / 2)

.mag <- function(d) {
  dplyr::case_when(
    is.na(d)       ~ NA_character_,
    abs(d) >= 0.8  ~ "Large",
    abs(d) >= 0.5  ~ "Medium",
    abs(d) >= 0.2  ~ "Small",
    TRUE           ~ "Negligible"
  )
}

.es_tbl_row <- function(res, contrast_label) {
  if (is.null(res) || is.null(res$dz) || is.na(res$dz)) return(NULL)
  se_dz <- 1 / sqrt(max(res$n - 1, 1))
  tibble::tibble(
    Contrast  = contrast_label,
    n         = res$n,
    dz        = round(res$dz, 3),
    `95% CI`  = sprintf("[%.3f, %.3f]",
                        res$dz - ci_mult_tbl * se_dz,
                        res$dz + ci_mult_tbl * se_dz),
    Magnitude = .mag(res$dz),
    p         = dplyr::if_else(is.na(res$p), NA_character_, fmt_p(res$p))
  )
}

tbl8 <- dplyr::bind_rows(
  .es_tbl_row(results$contrast_intervention_full,  paste0(int_label, " vs ", ctl_label, " (full)")),
  .es_tbl_row(results$contrast_intervention_restr, paste0(int_label, " vs ", ctl_label, " (restricted)")),
  .es_tbl_row(results$contrast_period_full,        "Period 2 vs Period 1 (full)"),
  .es_tbl_row(results$contrast_period_restr,       "Period 2 vs Period 1 (restricted)")
)

if (!is.null(tbl8) && nrow(tbl8) > 0) {
  save_table(tbl8, "08_effect_size_summary", subfolder = "primary",
             caption = paste0("Cohen's dz effect sizes with approximate ",
                              round(ci_level_tbl * 100), "% confidence intervals"))
} else {
  log_warn("No effect size data available \u2014 Table 8 skipped.")
}

# =============================================================================
# TABLE 9: 2×2 cell means (Period × Sequence group)
# =============================================================================
log_h2("Table 9: 2x2 period x sequence cell means")

tbl9 <- dat |>
  dplyr::group_by(.data$sequence_group) |>
  dplyr::summarise(
    n_p1          = dplyr::n(),
    mean_p1       = round(mean(.data$period1_score_full, na.rm = TRUE), 2),
    sd_p1         = round(sd(.data$period1_score_full,   na.rm = TRUE), 2),
    median_p1     = round(stats::median(.data$period1_score_full, na.rm = TRUE), 2),
    mean_p2       = round(mean(.data$period2_score_full, na.rm = TRUE), 2),
    sd_p2         = round(sd(.data$period2_score_full,   na.rm = TRUE), 2),
    median_p2     = round(stats::median(.data$period2_score_full, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  dplyr::transmute(
    Sequence    = .data$sequence_group,
    n           = .data$n_p1,
    `Period 1 M (SD)`  = sprintf("%.2f (%.2f)", .data$mean_p1, .data$sd_p1),
    `Period 1 Median`  = .data$median_p1,
    `Period 2 M (SD)`  = sprintf("%.2f (%.2f)", .data$mean_p2, .data$sd_p2),
    `Period 2 Median`  = .data$median_p2
  )

# Add marginal row (all sequences combined)
tbl9_margins <- dat |>
  dplyr::summarise(
    Sequence         = "Overall",
    n                = dplyr::n(),
    `Period 1 M (SD)`   = sprintf("%.2f (%.2f)",
                                  mean(.data$period1_score_full, na.rm = TRUE),
                                  sd(.data$period1_score_full,   na.rm = TRUE)),
    `Period 1 Median`   = round(stats::median(.data$period1_score_full, na.rm = TRUE), 2),
    `Period 2 M (SD)`   = sprintf("%.2f (%.2f)",
                                  mean(.data$period2_score_full, na.rm = TRUE),
                                  sd(.data$period2_score_full,   na.rm = TRUE)),
    `Period 2 Median`   = round(stats::median(.data$period2_score_full, na.rm = TRUE), 2)
  )

tbl9 <- dplyr::bind_rows(tbl9, tbl9_margins)

save_table(tbl9, "09_period_condition_cell_means", subfolder = "descriptive",
           caption = "Cell means (M, SD, Median) by period and sequence group")

# =============================================================================
# TABLE 10: Normality tests (Shapiro-Wilk) — conditional on config flag
# =============================================================================
.run_norm <- isTRUE(cfg_get("optional_analyses", "run_normality_tests", default = TRUE))

if (.run_norm) {
  log_h2("Table 10: Normality tests (Shapiro-Wilk)")

  .sw_row <- function(x, label) {
    x_clean <- x[!is.na(x)]
    if (length(x_clean) < 3 || length(x_clean) > 5000) return(NULL)
    tst <- stats::shapiro.test(x_clean)
    tibble::tibble(
      Variable = label,
      n        = length(x_clean),
      W        = round(tst$statistic, 4),
      p        = round(tst$p.value, 4),
      Interpretation = dplyr::if_else(tst$p.value >= 0.05, "Normal", "Non-normal")
    )
  }

  tbl10 <- dplyr::bind_rows(
    .sw_row(dat$intervention_score_full, paste0(int_label, " score (full)")),
    .sw_row(dat$control_score_full,      paste0(ctl_label, " score (full)")),
    .sw_row(dat$period1_score_full,      "Period 1 score (full)"),
    .sw_row(dat$period2_score_full,      "Period 2 score (full)"),
    .sw_row(dat$intervention_score_full - dat$control_score_full,
            paste0(int_label, " \u2212 ", ctl_label, " difference")),
    .sw_row(dat$period2_score_full - dat$period1_score_full,
            "Period 2 \u2212 Period 1 difference")
  )

  if (!is.null(tbl10) && nrow(tbl10) > 0) {
    save_table(tbl10, "10_normality_tests", subfolder = "supplementary",
               caption = "Shapiro-Wilk normality tests for key study variables and difference scores")
  } else {
    log_warn("Could not compute Shapiro-Wilk tests \u2014 Table 10 skipped.")
  }
} else {
  log_line("Normality tests skipped: optional_analyses.run_normality_tests = false")
}

# =============================================================================
# TABLE 11: Time analysis — conditional on data + config flag
# =============================================================================
.run_time  <- isTRUE(cfg_get("optional_analyses", "run_time_analysis", default = TRUE))
.has_tx    <- "time_taken_x_sec" %in% names(dat)
.has_ty    <- "time_taken_y_sec" %in% names(dat)

if (.run_time && (.has_tx || .has_ty)) {
  log_h2("Table 11: Time analysis")

  .time_row <- function(col_sec, label) {
    if (!col_sec %in% names(dat)) return(NULL)
    x <- dat[[col_sec]] / 60
    tibble::tibble(
      Form   = label,
      n      = sum(!is.na(x)),
      `M (min)`  = round(mean(x, na.rm = TRUE), 2),
      `SD`       = round(sd(x,   na.rm = TRUE), 2),
      `Median`   = round(stats::median(x, na.rm = TRUE), 2),
      `Min`      = round(min(x, na.rm = TRUE), 2),
      `Max`      = round(max(x, na.rm = TRUE), 2)
    )
  }

  tbl11 <- dplyr::bind_rows(
    .time_row("time_taken_x_sec", form_x_lbl),
    .time_row("time_taken_y_sec", form_y_lbl)
  )

  if (!is.null(tbl11) && nrow(tbl11) > 0) {
    save_table(tbl11, "11_time_analysis", subfolder = "descriptive",
               caption = "Time taken to complete each form (minutes)")

    # TABLE 11b: Comparison between forms (t-test or Wilcoxon)
    if (.has_tx && .has_ty) {
      t_min_x  <- dat$time_taken_x_sec / 60
      t_min_y  <- dat$time_taken_y_sec / 60
      tt       <- stats::t.test(t_min_x, t_min_y, paired = TRUE)
      tbl11b   <- tibble::tibble(
        Comparison = paste0(form_x_lbl, " vs ", form_y_lbl),
        `Mean diff (min)` = round(tt$estimate, 3),
        t                 = round(tt$statistic, 3),
        df                = round(tt$parameter, 1),
        p                 = round(tt$p.value, 4),
        `95% CI`          = sprintf("[%.3f, %.3f]",
                                    tt$conf.int[1], tt$conf.int[2])
      )
      save_table(tbl11b, "11b_time_form_comparison", subfolder = "descriptive",
                 caption = "Paired t-test: time taken between forms")
    }
  }
} else if (!.run_time) {
  log_line("Time analysis tables skipped: optional_analyses.run_time_analysis = false")
} else {
  log_line("Time analysis tables skipped: no time_taken_*_sec columns found in data")
}

# =============================================================================
# TABLE 12: LME model comparison (AIC/BIC/LRT) — conditional
# =============================================================================
.run_mc   <- isTRUE(cfg_get("optional_analyses", "run_model_comparison", default = TRUE))
.m1_avail <- !is.null(results$mixed_models$full$model1 %||% results$mixed_models$model1)
.m2_avail <- !is.null(results$mixed_models$full$model2 %||% results$mixed_models$model2)

if (.run_mc && .m1_avail && .m2_avail) {
  log_h2("Table 12: LME model comparison")

  mm1 <- results$mixed_models$full$model1 %||% results$mixed_models$model1
  mm2 <- results$mixed_models$full$model2 %||% results$mixed_models$model2

  mc  <- stats::anova(mm1, mm2)

  tbl12 <- tibble::tibble(
    Model    = c("Model 1 (no interaction)", "Model 2 (with sequence\u00d7period)"),
    df       = mc$`Df`,
    AIC      = round(mc$AIC,     2),
    BIC      = round(mc$BIC,     2),
    logLik   = round(mc$logLik,  2),
    Chi2    = c(NA, round(mc$Chisq[2],  3)),
    Chi2_df = c(NA, mc$`Chi Df`[2]),
    p        = c(NA, round(mc$`Pr(>Chisq)`[2], 4))
  )

  save_table(tbl12, "12_model_comparison", subfolder = "mixed_models",
             caption = "LME model comparison: AIC, BIC, and likelihood ratio test")
} else if (!.run_mc) {
  log_line("Model comparison table skipped: optional_analyses.run_model_comparison = false")
} else {
  log_line("Model comparison table skipped: one or both LME models unavailable")
}

# =============================================================================
# TABLE 13: Full vs restricted scoring comparison — conditional
# =============================================================================
.run_restr <- isTRUE(cfg_get("optional_analyses", "run_restricted_comparison", default = TRUE))
.has_restr <- !is.null(results$contrast_intervention_restr) &&
              !is.null(results$contrast_intervention_full)

if (.run_restr && .has_restr) {
  log_h2("Table 13: Full vs restricted scoring comparison")

  .score_row <- function(label, full_col, restr_col) {
    f  <- dat[[full_col]]
    r  <- dat[[restr_col]]
    if (is.null(f) || is.null(r)) return(NULL)
    tibble::tibble(
      Condition       = label,
      `Full n`        = sum(!is.na(f)),
      `Full M (SD)`   = sprintf("%.2f (%.2f)", mean(f, na.rm=TRUE), sd(f, na.rm=TRUE)),
      `Restr n`       = sum(!is.na(r)),
      `Restr M (SD)`  = sprintf("%.2f (%.2f)", mean(r, na.rm=TRUE), sd(r, na.rm=TRUE)),
      `Full-Restr r`  = round(stats::cor(f, r, use = "pairwise.complete.obs"), 3)
    )
  }

  tbl13 <- dplyr::bind_rows(
    .score_row(int_label, "intervention_score_full", "intervention_score_restricted"),
    .score_row(ctl_label, "control_score_full",      "control_score_restricted"),
    .score_row("Period 1", "period1_score_full",     "period1_score_restricted"),
    .score_row("Period 2", "period2_score_full",     "period2_score_restricted")
  )

  if (!is.null(tbl13) && nrow(tbl13) > 0) {
    # Add effect sizes for both scoring schemes
    .es_block <- dplyr::bind_rows(
      tibble::tibble(
        Scoring    = "Full",
        Contrast   = paste0(int_label, " vs ", ctl_label),
        dz         = round(results$contrast_intervention_full$dz,  3),
        p          = round(results$contrast_intervention_full$p,   4)
      ),
      tibble::tibble(
        Scoring    = "Restricted",
        Contrast   = paste0(int_label, " vs ", ctl_label),
        dz         = round(results$contrast_intervention_restr$dz, 3),
        p          = round(results$contrast_intervention_restr$p,  4)
      )
    )
    save_table(tbl13, "13_full_vs_restricted_comparison", subfolder = "supplementary",
               caption = "Comparison of full and restricted scoring: means, SDs, and correlation")
    save_table(.es_block, "13b_full_vs_restricted_effect_sizes", subfolder = "supplementary",
               caption = "Cohen's dz for intervention effect under full vs restricted scoring")
  } else {
    log_warn("Restricted score columns not found \u2014 Table 13 skipped.")
  }
} else if (!.run_restr) {
  log_line("Full vs restricted comparison skipped: optional_analyses.run_restricted_comparison = false")
} else {
  log_line("Full vs restricted comparison skipped: restricted contrast results unavailable")
}

# =============================================================================
# TABLE 14: 4-subgroup descriptive statistics
# Breaks down Int / Ctl mean, SD, median, ceiling%, floor% for each of the
# four sequence × form subgroups. Useful for spotting ceiling effects and
# differential performance driven by which form was the intervention form.
# =============================================================================
log_h2("Table 14: 4-subgroup descriptive statistics")

if ("subgroup4" %in% names(dat)) {
  .ceil14  <- cfg$analysis$ceiling_threshold %||% (scale_to * 0.95)
  .floor14 <- cfg$analysis$floor_threshold   %||% (scale_to * 0.05)

  tbl14 <- dplyr::bind_rows(
    dat |> dplyr::mutate(.score = .data$intervention_score_full,
                         cond_  = int_label),
    dat |> dplyr::mutate(.score = .data$control_score_full,
                         cond_  = ctl_label)
  ) |>
    dplyr::group_by(subgroup4, cond_) |>
    dplyr::summarise(
      n              = dplyr::n(),
      mean_score     = round(mean(.data$.score, na.rm = TRUE), 2),
      sd_score       = round(sd(.data$.score,   na.rm = TRUE), 2),
      median_score   = round(median(.data$.score, na.rm = TRUE), 2),
      pct_at_ceiling = round(
        mean(.data$.score >= .ceil14, na.rm = TRUE) * 100, 1),
      pct_at_floor   = round(
        mean(.data$.score <= .floor14, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    dplyr::rename(
      Subgroup      = subgroup4,
      Condition     = cond_,
      N             = n,
      Mean          = mean_score,
      SD            = sd_score,
      Median        = median_score,
      `Ceiling (%)` = pct_at_ceiling,
      `Floor (%)`   = pct_at_floor
    )

  if (nrow(tbl14) > 0) {
    save_table(tbl14, "14_subgroup4_descriptives", subfolder = "descriptive",
               caption = "Descriptive statistics by 4-cell crossover subgroup (sequence \u00d7 intervention form)")
  }
} else {
  log_line("Table 14 skipped: subgroup4 column not present in data")
}

# =============================================================================
# TABLE 15: Period-specific intervention effect
# Was the intervention more effective when it occurred in Period 1 vs
# Period 2?  Shows mean(Int−Ctl) for each half plus the between-group test.
# =============================================================================
log_h2("Table 15: Period-specific intervention effect")

.period_int_full  <- tryCatch(results$period_int_full,  error = function(e) NULL)
.period_int_restr <- tryCatch(results$period_int_restr, error = function(e) NULL)

if (!is.null(.period_int_full) && !all(is.na(.period_int_full))) {
  .build_period_tbl <- function(pobj, scoring) {
    tibble::tibble(
      Scoring         = scoring,
      Group           = c(paste0(int_label, " in Period 1"),
                          paste0(int_label, " in Period 2"),
                          "Between-group test"),
      N               = c(pobj$n_int_p1, pobj$n_int_p2, NA_integer_),
      `Mean (Int-Ctl)` = c(round(pobj$mean_diff_int_p1, 3),
                            round(pobj$mean_diff_int_p2, 3),
                            NA_real_),
      SD              = c(round(pobj$sd_diff_int_p1, 3),
                          round(pobj$sd_diff_int_p2, 3),
                          NA_real_),
      `t (Welch)`     = c(NA_real_, NA_real_, round(pobj$t, 3)),
      `p-value`       = c(NA_real_, NA_real_, round(pobj$p, 4))
    )
  }

  tbl15 <- dplyr::bind_rows(
    .build_period_tbl(.period_int_full, "Full scoring")
  )
  if (!is.null(.period_int_restr) && !all(is.na(.period_int_restr))) {
    tbl15 <- dplyr::bind_rows(
      tbl15,
      .build_period_tbl(.period_int_restr, "Restricted scoring")
    )
  }

  save_table(tbl15, "15_period_specific_intervention_effect",
             subfolder = "period_effects",
             caption = paste0("Period-specific intervention effect: mean within-person difference (",
                              int_label, " \u2212 ", ctl_label,
                              ") stratified by when intervention occurred"))
} else {
  log_line("Table 15 skipped: period-specific intervention effect results not available")
}

# =============================================================================
# TABLE 16: 4-subgroup contrast table
# Paired t/dz for each of the four crossover subgroups, full and restricted
# scoring, so readers can judge whether the overall effect holds across all cells.
# =============================================================================
log_h2("Table 16: 4-subgroup contrast summary")

.sg4f <- tryCatch(results$sg4_contrasts_full,  error = function(e) NULL)
.sg4r <- tryCatch(results$sg4_contrasts_restr, error = function(e) NULL)

if (!is.null(.sg4f) && nrow(.sg4f) > 0) {
  .fmt_sg4 <- function(df, scoring) {
    df |>
      dplyr::mutate(Scoring = scoring) |>
      dplyr::select(
        Scoring,
        Subgroup   = subgroup4,
        N          = n,
        `Mean Int` = mean_int,
        `Mean Ctl` = mean_ctl,
        `Mean diff`= mean_diff,
        `CI low`   = ci_lo,
        `CI high`  = ci_hi,
        `dz`       = dz,
        `p-value`  = p
      ) |>
      dplyr::mutate(dplyr::across(c(`Mean Int`,`Mean Ctl`,`Mean diff`,
                                    `CI low`,`CI high`,dz), \(x) round(x, 3)),
                    `p-value` = round(.data$`p-value`, 4))
  }

  tbl16 <- .fmt_sg4(.sg4f, "Full scoring")
  if (!is.null(.sg4r) && nrow(.sg4r) > 0) {
    tbl16 <- dplyr::bind_rows(tbl16, .fmt_sg4(.sg4r, "Restricted scoring"))
  }

  save_table(tbl16, "16_subgroup4_contrasts", subfolder = "primary",
             caption = paste0("Within-person intervention effect (Cohen\u2019s dz) for each 4-cell crossover subgroup"))
} else {
  log_line("Table 16 skipped: subgroup4 contrasts not available")
}

# =============================================================================
# TABLE 17: Suspicious items summary
# Cross-reference with Table 06 (per-item psychometrics): lists any items
# flagged by detect_suspicious_items() with their flag categories and the
# bot/top hit rates that triggered the flag. Sorted by suspicion level.
# =============================================================================
log_h2("Table 17: Suspicious items summary")

.psych_rds <- tryCatch(
  load_rds("psychometrics"),
  error = function(e) NULL
)

if (!is.null(.psych_rds) &&
    "suspicious_all" %in% names(.psych_rds) &&
    nrow(.psych_rds$suspicious_all) > 0) {

  tbl17 <- .psych_rds$suspicious_all |>
    dplyr::select(
      form,
      item,
      n_flags,
      dplyr::any_of(c("p_bottom_Q", "p_top_Q", "top_miss_rate",
                       "item_rest_r", "flags"))
    ) |>
    dplyr::mutate(.item_num = as.integer(gsub("^[a-z]+", "",
                                              tolower(.data$item)))) |>
    dplyr::arrange(.data$form, .data$.item_num) |>
    dplyr::select(-.item_num) |>
    dplyr::rename_with(
      ~ dplyr::recode(.,
        form          = "Form",
        item          = "Item",
        n_flags       = "# Flags",
        p_bottom_Q    = "P(correct | bot Q)",
        p_top_Q       = "P(correct | top Q)",
        top_miss_rate = "Top-Q miss rate",
        item_rest_r   = "Item-rest r",
        flags         = "Flag criteria"
      )
    )

  .thr_top  <- as.numeric(cfg_get("psychometrics", "top_miss_threshold",  default = 0.25))
  .thr_bot  <- as.numeric(cfg_get("psychometrics", "bottom_hit_threshold", default = 0.60))
  .thr_ir   <- as.numeric(cfg_get("psychometrics", "min_item_rest_r",      default = 0.10))
  .flag_notes17 <- c(
    paste0('"# Flags": count of psychometric suspicion criteria triggered for this item.'),
    paste0("Items are ordered by form (X then Y) then item number."),
    paste0("Flagging criteria (thresholds from config/study_config.yml):"),
    paste0("  (1) Top-Q miss rate > ",  .thr_top,
           " -- item missed by more than ", round(.thr_top * 100), "% of highest-scoring participants"),
    paste0("  (2) Bot-Q hit rate > ",   .thr_bot,
           " -- item correct for more than ", round(.thr_bot * 100), "% of lowest-scoring participants (possible floor bypass)"),
    paste0("  (3) Reversed discrimination -- P(correct) lower in top ability quartile than in bottom quartile"),
    paste0("  (4) Non-monotone across Q -- P(correct) drops >10 percentage points between adjacent ability quartiles"),
    paste0("  (5) Item-rest r < ",      .thr_ir,
           " -- item-total correlation below threshold (item may not measure the same construct)")
  )

  save_table(tbl17, "17_suspicious_items",
             subfolder = "item_analysis",
             caption = "Items flagged by psychometric suspicion criteria (ability-stratified detection)",
             notes = .flag_notes17)
} else {
  log_line("Table 17 skipped: no suspicious items detected or psychometrics RDS unavailable")
}

# =============================================================================
# TABLE 18: Ability-stratified item difficulty
# P(correct) for each item broken out by ability quartile.
# Documents ceiling effects at the item level — "Did high-scorers find
# every item trivially easy?" and flags for non-monotone patterns.
# =============================================================================
log_h2("Table 18: Ability-stratified item difficulty")

if (!is.null(.psych_rds) &&
    "strat_all" %in% names(.psych_rds) &&
    nrow(.psych_rds$strat_all) > 0) {

  tbl18 <- .psych_rds$strat_all |>
    dplyr::select(form, item, ability_quartile,
                  dplyr::any_of(c("n_in_stratum", "p_correct", "n_correct"))) |>
    tidyr::pivot_wider(
      id_cols       = c(form, item),
      names_from    = ability_quartile,
      values_from   = p_correct,
      names_prefix  = "Q"
    ) |>
    dplyr::rename(Form = form, Item = item) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Q"),
                                ~ round(.x, 3)))

  if (!is.null(.psych_rds$suspicious_all) &&
      nrow(.psych_rds$suspicious_all) > 0) {
    tbl18 <- tbl18 |>
      dplyr::left_join(
        .psych_rds$suspicious_all |>
          dplyr::select(form, item, n_flags) |>
          dplyr::rename(Form = form, Item = item, `# Flags` = n_flags),
        by = c("Form", "Item")
      )
  }

  # Sort by form (X before Y) then numerically by item number
  tbl18 <- tbl18 |>
    dplyr::mutate(.item_num = as.integer(gsub("^[a-z]+", "",
                                             tolower(.data$Item)))) |>
    dplyr::arrange(.data$Form, .data$.item_num) |>
    dplyr::select(-.item_num)

  .flag_notes18 <- c(
    paste0('"# Flags": count of psychometric suspicion criteria triggered for this item (see Table 17 for detail).'),
    paste0("Q1 = lowest ability quartile, Q4 = highest ability quartile."),
    paste0("A well-functioning item shows P(correct) increasing monotonically from Q1 to Q4."),
    paste0("Items are ordered by form (X then Y) then item number.")
  )

  save_table(tbl18, "18_ability_stratified_item_difficulty",
             subfolder = "item_analysis",
             caption = "P(correct) by ability quartile for each item; # Flags = suspicion flag count",
             notes = .flag_notes18)
} else {
  log_line("Table 18 skipped: ability-stratified data not available in psychometrics RDS")
}

# =============================================================================
# TABLE 19: Item endorsement rates by sequence group (exploratory)
# Per item: n, n correct, % correct overall, and % correct per sequence group.
# =============================================================================
log_h2("Table 19: Item endorsement rates by sequence group")

.raw_tbl19 <- tryCatch(load_rds("raw_data"), error = function(e) NULL)

if (!is.null(.raw_tbl19)) {
  .seq_map_t19 <- dplyr::distinct(dat,
    participant    = as.character(.data$participant),
    sequence_group = .data$sequence_group
  )

  .endorse_tbl19 <- function(items_df, cols_full, excl_cols, form_lbl) {
    .num_ord <- suppressWarnings(as.numeric(sub("^[xy]", "", cols_full)))
    .col_ord <- cols_full[order(.num_ord)]
    items_df |>
      dplyr::mutate(participant = as.character(.data$participant)) |>
      dplyr::select(participant, dplyr::all_of(.col_ord)) |>
      tidyr::pivot_longer(dplyr::all_of(.col_ord),
                          names_to  = "item_raw",
                          values_to = "response") |>
      dplyr::mutate(
        Form     = form_lbl,
        Item     = toupper(.data$item_raw),
        Excluded = .data$item_raw %in% excl_cols
      ) |>
      dplyr::left_join(.seq_map_t19, by = "participant") |>
      dplyr::filter(!is.na(.data$response))
  }

  .long_t19 <- dplyr::bind_rows(
    .endorse_tbl19(.raw_tbl19$x_items, .raw_tbl19$x_cols_full,
                   .raw_tbl19$x_excluded, form_x_lbl),
    .endorse_tbl19(.raw_tbl19$y_items, .raw_tbl19$y_cols_full,
                   .raw_tbl19$y_excluded, form_y_lbl)
  )

  .overall_t19 <- .long_t19 |>
    dplyr::group_by(.data$Form, .data$Item, .data$Excluded) |>
    dplyr::summarise(
      `N Total`   = dplyr::n(),
      `N Correct` = sum(.data$response, na.rm = TRUE),
      `% Correct` = round(`N Correct` / `N Total` * 100, 1),
      .groups     = "drop"
    )

  .by_seq_t19 <- .long_t19 |>
    dplyr::filter(!is.na(.data$sequence_group)) |>
    dplyr::group_by(.data$Form, .data$Item, .data$sequence_group) |>
    dplyr::summarise(pct = round(mean(.data$response) * 100, 1), .groups = "drop") |>
    tidyr::pivot_wider(names_from = sequence_group, values_from = pct)

  tbl19 <- .overall_t19 |>
    dplyr::left_join(.by_seq_t19, by = c("Form", "Item")) |>
    dplyr::mutate(
      `Excluded?` = dplyr::if_else(.data$Excluded, "Yes", "No"),
      .num_order  = suppressWarnings(as.numeric(sub("^[A-Za-z]+", "", .data$Item)))
    ) |>
    dplyr::arrange(.data$Form, .data$.num_order) |>
    dplyr::select(-Excluded, -.num_order)

  .excl_note <- if (any(tbl19[["Excluded?"]] == "Yes"))
    " Excluded = item omitted from restricted scoring." else ""

  save_table(tbl19, "19_item_endorsement_rates",
             subfolder = "exploratory",
             caption   = paste0("Per-item endorsement rates overall and by sequence group.",
                                .excl_note))
} else {
  log_line("Table 19 skipped: raw_data not available")
}

log_h2("TABLES COMPLETE")
