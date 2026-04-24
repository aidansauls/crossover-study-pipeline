## =============================================================================
## R/04_analyses.R
## Primary statistical analyses:
##   1. Descriptive statistics
##   2. Intervention effect (paired contrast)
##   3. Period effects (practice, carryover via Grizzle test)
##   4. Sequence x period interaction
##   5. Linear mixed-effects models
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
log_h1("04  STATISTICAL ANALYSES")

cfg         <- read_config()
dat         <- load_rds("analysis_data")
dat_long    <- load_rds("analysis_data_long")

alpha       <- as.numeric(cfg$analysis$alpha        %||% 0.05)
ci_level    <- as.numeric(cfg$analysis$ci_level     %||% 0.95)
min_n_carry <- as.integer(cfg$analysis$min_n_carryover %||% 4)
scale_to    <- as.numeric(cfg$scores$scale_to       %||% 10)
int_label   <- cfg$study$intervention_label %||% "Intervention"
ctl_label   <- cfg$study$control_label      %||% "Control"
form_x_lbl  <- cfg$study$form_x_label       %||% "Form X"
form_y_lbl  <- cfg$study$form_y_label       %||% "Form Y"

# =============================================================================
# 1. DESCRIPTIVE STATISTICS
# =============================================================================
log_h2("Descriptive statistics")

desc_cols <- c(
  "intervention_score_full", "control_score_full",
  "period1_score_full",      "period2_score_full",
  "x_score_full",            "y_score_full",
  if ("intervention_score_restricted" %in% names(dat))
    c("intervention_score_restricted", "control_score_restricted",
      "period1_score_restricted",      "period2_score_restricted") else character(0)
)
desc_cols <- intersect(desc_cols, names(dat))

descriptives <- purrr::map_dfr(desc_cols, function(col) {
  x <- dat[[col]]
  x <- x[!is.na(x)]
  tibble::tibble(
    Measure  = col,
    N        = length(x),
    Mean     = mean(x),
    SD       = sd(x),
    Median   = median(x),
    IQR_lo   = quantile(x, 0.25),
    IQR_hi   = quantile(x, 0.75),
    Min      = min(x),
    Max      = max(x),
    `Ceiling (perfect)` = sum(x == scale_to) / length(x),
    `Floor (zero)`      = sum(x == 0) / length(x)
  )
})

# Sequence group sizes
sequence_tbl <- dat |>
  dplyr::count(.data$sequence_group, name = "n") |>
  dplyr::mutate(pct = .data$n / sum(.data$n))

log_check("Sequence groups: ",
          paste0(sequence_tbl$sequence_group, " n=", sequence_tbl$n, collapse = ", "))

# =============================================================================
# 2. INTERVENTION EFFECT
# =============================================================================
log_h2("Intervention effect")

run_paired_contrast <- function(scoring) {
  a_col <- paste0("intervention_score_", scoring)
  b_col <- paste0("control_score_",      scoring)
  if (!a_col %in% names(dat) || !b_col %in% names(dat)) return(NULL)
  paired_summary(dat[[a_col]], dat[[b_col]],
                 label   = paste0(int_label, " vs ", ctl_label, " (", scoring, ")"),
                 ci      = ci_level)
}

contrast_intervention_full <- run_paired_contrast("full")
contrast_intervention_restr <- run_paired_contrast("restricted")

log_paired_result(contrast_intervention_full)
if (!is.null(contrast_intervention_restr)) {
  log_paired_result(contrast_intervention_restr)
}

# =============================================================================
# 3. PERIOD EFFECTS (practice / learning effects)
# =============================================================================
log_h2("Period effects")

run_period_contrast <- function(scoring) {
  p1_col <- paste0("period1_score_", scoring)
  p2_col <- paste0("period2_score_", scoring)
  if (!p1_col %in% names(dat) || !p2_col %in% names(dat)) return(NULL)
  paired_summary(dat[[p2_col]], dat[[p1_col]],  # Period 2 - Period 1
                 label = paste0("Period 2 - Period 1 (", scoring, ")"),
                 ci    = ci_level)
}

contrast_period_full  <- run_period_contrast("full")
contrast_period_restr <- run_period_contrast("restricted")

log_paired_result(contrast_period_full)
if (!is.null(contrast_period_restr)) log_paired_result(contrast_period_restr)

# =============================================================================
# 4. CARRYOVER TEST (Grizzle 1965)
# The standard crossover carryover test compares Period-1 scores between
# sequence groups. Significant difference suggests carryover.
# Requires ≥ min_n_carryover per group (set in config).
# =============================================================================
log_h2("Carryover test (Grizzle)")

run_carryover <- function(scoring) {
  p1_col <- paste0("period1_score_", scoring)
  if (!p1_col %in% names(dat)) return(NULL)
  grp_a <- dat[[p1_col]][dat$intervention_period == 1]
  grp_b <- dat[[p1_col]][dat$intervention_period == 2]
  grp_a <- grp_a[!is.na(grp_a)]
  grp_b <- grp_b[!is.na(grp_b)]
  
  if (length(grp_a) < min_n_carry || length(grp_b) < min_n_carry) {
    log_warn("Insufficient n for carryover test (scoring=", scoring,
             "). n_A=", length(grp_a), " n_B=", length(grp_b),
             " (min=", min_n_carry, ")")
    return(list(
      scoring = scoring, test = "t-test",
      mean_a = mean(grp_a), sd_a = sd(grp_a),
      mean_b = mean(grp_b), sd_b = sd(grp_b),
      n_a = length(grp_a), n_b = length(grp_b),
      p = NA_real_, t = NA_real_, interpretation = "insufficient n"
    ))
  }
  # Guard: t.test requires within-group variance; catch degenerate case
  if (sd(grp_a) == 0 && sd(grp_b) == 0) {
    log_warn("Carryover test skipped (scoring=", scoring,
             "): all Period-1 scores are identical within both sequence groups ",
             "(SD = 0). This occurs when test data lack within-group variation.")
    return(list(
      scoring = scoring, test = "Welch t-test",
      mean_a = mean(grp_a), sd_a = 0,
      mean_b = mean(grp_b), sd_b = 0,
      n_a = length(grp_a), n_b = length(grp_b),
      p = NA_real_, t = NA_real_,
      interpretation = "not estimable: zero variance within sequence groups"
    ))
  }
  tt <- tryCatch(
    t.test(grp_a, grp_b, var.equal = FALSE),
    error = function(e) {
      log_warn("Carryover t.test failed (scoring=", scoring, "): ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(tt)) {
    return(list(
      scoring = scoring, test = "Welch t-test",
      mean_a = mean(grp_a), sd_a = sd(grp_a),
      mean_b = mean(grp_b), sd_b = sd(grp_b),
      n_a = length(grp_a), n_b = length(grp_b),
      p = NA_real_, t = NA_real_,
      interpretation = "not estimable: t-test error"
    ))
  }
  list(
    scoring = scoring, test = "Welch t-test",
    mean_a = mean(grp_a), sd_a = sd(grp_a),
    mean_b = mean(grp_b), sd_b = sd(grp_b),
    n_a = length(grp_a), n_b = length(grp_b),
    p = tt$p.value, t = tt$statistic,
    interpretation = if (!is.na(tt$p.value) && tt$p.value < alpha)
      "Possible carryover: period-1 scores differ between sequences (p < alpha)"
    else
      "No evidence of carryover: period-1 scores similar between sequences"
  )
}

carryover_full  <- run_carryover("full")
carryover_restr <- run_carryover("restricted")

for (.carry in list(carryover_full, carryover_restr)) {
  if (is.null(.carry)) next
  log_stat(
    paste0("Grizzle carryover test (", .carry$scoring, " scoring)"),
    test             = .carry$test,
    group_A_label    = paste0(int_label, "-first (Period 1)"),
    n_A              = .carry$n_a,
    mean_A           = round(.carry$mean_a, 4),
    sd_A             = round(.carry$sd_a,   4),
    group_B_label    = paste0(ctl_label,  "-first (Period 1)"),
    n_B              = .carry$n_b,
    mean_B           = round(.carry$mean_b, 4),
    sd_B             = round(.carry$sd_b,   4),
    t_statistic      = round(.carry$t, 4),
    p_value          = sub("^= ", "", fmt_p(.carry$p)),
    interpretation   = .carry$interpretation
  )
}

# =============================================================================
# 5. SEQUENCE x PERIOD INTERACTION
# Tests whether the treatment effect differs by sequence (i.e., treatment x period)
# Uses a 2x2 repeated-measures ANOVA approach.
# =============================================================================
log_h2("Sequence x Period interaction")

run_seq_period_interaction <- function(scoring) {
  p1_col <- paste0("period1_score_", scoring)
  p2_col <- paste0("period2_score_", scoring)
  if (!p1_col %in% names(dat) || !p2_col %in% names(dat)) return(NULL)
  
  dat_sub <- dat |>
    dplyr::select(participant, intervention_period, p1 = dplyr::all_of(p1_col),
                  p2 = dplyr::all_of(p2_col)) |>
    dplyr::mutate(
      sequence = factor(ifelse(.data$intervention_period == 1, "AB", "BA")),
      diff = .data$p2 - .data$p1
    )
  
  # Interaction test: is the period difference the same in both sequences?
  grp_ab <- dat_sub$diff[dat_sub$sequence == "AB" & !is.na(dat_sub$diff)]
  grp_ba <- dat_sub$diff[dat_sub$sequence == "BA" & !is.na(dat_sub$diff)]
  
  if (length(grp_ab) < 2 || length(grp_ba) < 2) {
    return(list(scoring = scoring, p = NA_real_,
                interpretation = "insufficient n for interaction test"))
  }
  if (sd(grp_ab) == 0 && sd(grp_ba) == 0) {
    log_warn("Sequence x Period interaction test skipped (scoring=", scoring,
             "): zero within-group variance in period differences.")
    return(list(
      scoring = scoring,
      mean_diff_ab = mean(grp_ab), sd_diff_ab = 0, n_ab = length(grp_ab),
      mean_diff_ba = mean(grp_ba), sd_diff_ba = 0, n_ba = length(grp_ba),
      t = NA_real_, p = NA_real_,
      interpretation = "not estimable: zero variance in period differences"
    ))
  }
  tt <- tryCatch(
    t.test(grp_ab, grp_ba, var.equal = FALSE),
    error = function(e) {
      log_warn("Seq x Period t.test failed (scoring=", scoring, "): ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(tt)) {
    return(list(
      scoring = scoring,
      mean_diff_ab = mean(grp_ab), sd_diff_ab = sd(grp_ab), n_ab = length(grp_ab),
      mean_diff_ba = mean(grp_ba), sd_diff_ba = sd(grp_ba), n_ba = length(grp_ba),
      t = NA_real_, p = NA_real_,
      interpretation = "not estimable: t-test error"
    ))
  }
  list(
    scoring = scoring,
    mean_diff_ab = mean(grp_ab),
    sd_diff_ab   = sd(grp_ab),
    n_ab         = length(grp_ab),
    mean_diff_ba = mean(grp_ba),
    sd_diff_ba   = sd(grp_ba),
    n_ba         = length(grp_ba),
    t  = tt$statistic,
    p  = tt$p.value,
    interpretation = if (!is.na(tt$p.value) && tt$p.value < alpha)
      paste0("Significant sequence x period interaction (p ", fmt_p(tt$p.value),
             "). Treatment effect may differ by sequence.")
    else
      paste0("No significant sequence x period interaction (p ", fmt_p(tt$p.value), ").")
  )
}

seq_period_full  <- run_seq_period_interaction("full")
seq_period_restr <- run_seq_period_interaction("restricted")

for (.spi in list(seq_period_full, seq_period_restr)) {
  if (is.null(.spi)) next
  log_stat(
    paste0("Sequence x Period interaction (", .spi$scoring, " scoring)"),
    group_AB_n              = .spi$n_ab,
    mean_period_diff_AB     = round(.spi$mean_diff_ab, 4),
    sd_period_diff_AB       = round(.spi$sd_diff_ab,   4),
    group_BA_n              = .spi$n_ba,
    mean_period_diff_BA     = round(.spi$mean_diff_ba, 4),
    sd_period_diff_BA       = round(.spi$sd_diff_ba,   4),
    t_statistic             = round(.spi$t, 4),
    p_value                 = sub("^= ", "", fmt_p(.spi$p)),
    interpretation          = .spi$interpretation
  )
}

# =============================================================================
# 5b. PERIOD-SPECIFIC INTERVENTION EFFECT
# The key 2x2 crossover question: Is the intervention effect LARGER when
# experienced in period 1 vs. period 2?
#
# Method: For the intervention-first group (AB), the intervention score is
# their Period-1 score and control is Period-2.  For the control-first (BA)
# group the intervention score is their Period-2 score and control is Period-1.
# We compare the within-person intervention-vs-control DIFFERENCE between the
# two sequence groups — a significant difference means the period in which
# the intervention occurred moderates(amplifies/dampens) the effect.
# =============================================================================
log_h2("Period-specific intervention effect (period when int. occurred)")

period_specific_int <- function(scoring) {
  int_col <- paste0("intervention_score_", scoring)
  ctl_col <- paste0("control_score_",      scoring)
  if (!int_col %in% names(dat)) return(NULL)

  dat_ps <- dat |>
    dplyr::mutate(
      int_minus_ctl = .data[[int_col]] - .data[[ctl_col]],
      int_period    = factor(.data$intervention_period,
                             levels = c(1L, 2L),
                             labels = c("Period 1 (Int 1st)", "Period 2 (Int 2nd)"))
    )

  grp_p1 <- dat_ps$int_minus_ctl[dat_ps$intervention_period == 1L]
  grp_p2 <- dat_ps$int_minus_ctl[dat_ps$intervention_period == 2L]
  grp_p1 <- grp_p1[!is.na(grp_p1)]
  grp_p2 <- grp_p2[!is.na(grp_p2)]

  if (length(grp_p1) < 2 || length(grp_p2) < 2) {
    log_warn("Insufficient n for period-specific intervention test (", scoring, ")")
    return(list(scoring = scoring, p = NA_real_,
                interpretation = "insufficient n"))
  }
  if (sd(grp_p1) == 0 && sd(grp_p2) == 0) {
    log_warn("Period-specific intervention test skipped (scoring=", scoring,
             "): zero within-group variance in int-minus-ctl differences.")
    return(list(
      scoring = scoring,
      n_int_p1 = length(grp_p1), mean_diff_int_p1 = mean(grp_p1), sd_diff_int_p1 = 0,
      n_int_p2 = length(grp_p2), mean_diff_int_p2 = mean(grp_p2), sd_diff_int_p2 = 0,
      t = NA_real_, p = NA_real_,
      interpretation = "not estimable: zero variance in int-minus-ctl differences"
    ))
  }
  tt <- tryCatch(
    t.test(grp_p1, grp_p2, var.equal = FALSE),
    error = function(e) {
      log_warn("Period-specific int t.test failed (scoring=", scoring, "): ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(tt)) {
    return(list(
      scoring = scoring,
      n_int_p1 = length(grp_p1), mean_diff_int_p1 = mean(grp_p1), sd_diff_int_p1 = sd(grp_p1),
      n_int_p2 = length(grp_p2), mean_diff_int_p2 = mean(grp_p2), sd_diff_int_p2 = sd(grp_p2),
      t = NA_real_, p = NA_real_,
      interpretation = "not estimable: t-test error"
    ))
  }

  list(
    scoring          = scoring,
    n_int_p1         = length(grp_p1),
    mean_diff_int_p1 = mean(grp_p1),
    sd_diff_int_p1   = sd(grp_p1),
    n_int_p2         = length(grp_p2),
    mean_diff_int_p2 = mean(grp_p2),
    sd_diff_int_p2   = sd(grp_p2),
    t                = tt$statistic,
    p                = tt$p.value,
    interpretation   = if (!is.na(tt$p.value) && tt$p.value < alpha)
      paste0("Intervention effect DIFFERS by when it occurred (p ", fmt_p(tt$p.value), ")")
    else
      paste0("No significant moderation by intervention period (p ", fmt_p(tt$p.value), ")")
  )
}

period_int_full  <- period_specific_int("full")
period_int_restr <- period_specific_int("restricted")

for (.pi in list(period_int_full, period_int_restr)) {
  if (is.null(.pi)) next
  log_stat(
    paste0("Period-specific intervention effect (", .pi$scoring, " scoring)"),
    group_int_in_P1_n    = .pi$n_int_p1,
    mean_int_minus_ctl_P1 = round(.pi$mean_diff_int_p1, 4),
    sd_int_minus_ctl_P1  = round(.pi$sd_diff_int_p1,   4),
    group_int_in_P2_n    = .pi$n_int_p2,
    mean_int_minus_ctl_P2 = round(.pi$mean_diff_int_p2, 4),
    sd_int_minus_ctl_P2  = round(.pi$sd_diff_int_p2,   4),
    t_statistic          = round(.pi$t, 4),
    p_value              = sub("^= ", "", fmt_p(.pi$p)),
    interpretation       = .pi$interpretation
  )
}

# =============================================================================
# 5c. 4-SUBGROUP CONTRASTS
# For each of the four subgroups (seq × int-form), compute
# the intervention-vs-control contrast. Allows checking whether the
# intervention effect is consistent across all four subgroup configurations.
# =============================================================================
log_h2("4-subgroup intervention contrasts")

subgroup4_contrasts <- function(scoring) {
  int_col  <- paste0("intervention_score_", scoring)
  ctl_col  <- paste0("control_score_",      scoring)
  if (!int_col %in% names(dat)) return(NULL)
  if (!"subgroup4" %in% names(dat)) return(NULL)

  purrr::map_dfr(unique(dat$subgroup4), function(sg) {
    rows <- dat[dat$subgroup4 == sg, ]
    a    <- rows[[int_col]]
    b    <- rows[[ctl_col]]
    comp <- !is.na(a) & !is.na(b)
    a <- a[comp]; b <- b[comp]
    if (length(a) < 2) {
      return(tibble::tibble(subgroup4 = sg, scoring = scoring,
                             n = length(a), mean_int = NA_real_,
                             mean_ctl = NA_real_, mean_diff = NA_real_,
                             dz = NA_real_, p = NA_real_))
    }
    ps <- paired_summary(a, b,
                          label  = paste0(sg, " (", scoring, ")"),
                          ci     = ci_level)
    log_paired_result(ps)
    tibble::tibble(subgroup4 = sg, scoring = scoring,
                   n         = ps$n,
                   mean_int  = round(ps$mean_a,    3),
                   mean_ctl  = round(ps$mean_b,    3),
                   mean_diff = round(ps$mean_diff, 3),
                   ci_lo     = round(ps$ci_lo,     3),
                   ci_hi     = round(ps$ci_hi,     3),
                   dz        = round(ps$dz,        3),
                   p         = round(ps$p,         4))
  })
}

sg4_contrasts_full  <- subgroup4_contrasts("full")
sg4_contrasts_restr <- subgroup4_contrasts("restricted")

# =============================================================================
# 6. MIXED-EFFECTS MODELS
# Model: score ~ condition + period + sequence_group + (1|participant)
# Also runs: condition x period interaction model
# =============================================================================
log_h2("Mixed-effects models")

mixed_models <- list()

if (requireNamespace("lme4", quietly = TRUE) &&
    requireNamespace("lmerTest", quietly = TRUE)) {
  
  library(lme4)
  library(lmerTest)
  
  run_lmer <- function(scoring) {
    long_sub <- dat_long |>
      dplyr::filter(.data$scoring == .env$scoring,
                    .data$context %in% c("intervention", "control")) |>
      dplyr::mutate(
        condition_fac = factor(.data$condition,
                               levels = c(ctl_label, int_label)),
        participant_fac = factor(.data$participant),
        period_fac = factor(
          dplyr::case_when(
            .data$participant %in%
              (dat |> dplyr::filter(.data$intervention_period == 1) |>
                 dplyr::pull(.data$participant)) &
              .data$condition == int_label ~ "Period 1",
            .data$participant %in%
              (dat |> dplyr::filter(.data$intervention_period == 1) |>
                 dplyr::pull(.data$participant)) &
              .data$condition == ctl_label ~ "Period 2",
            TRUE ~ dplyr::case_when(
              .data$condition == ctl_label ~ "Period 1",
              TRUE ~ "Period 2"
            )
          ),
          levels = c("Period 1", "Period 2")
        ),
        sequence_fac = factor(.data$sequence_group)
      )
    
    # Primary model: condition + period + sequence + random intercept
    m1 <- tryCatch(
      lmerTest::lmer(score ~ condition_fac + period_fac + sequence_fac +
                       (1 | participant_fac),
                     data = long_sub, REML = TRUE),
      error = function(e) {
        log_warn("lmer model 1 failed (scoring=", scoring, "): ", conditionMessage(e))
        NULL
      }
    )
    
    # Interaction model: condition x period
    m2 <- tryCatch(
      lmerTest::lmer(score ~ condition_fac * period_fac + sequence_fac +
                       (1 | participant_fac),
                     data = long_sub, REML = TRUE),
      error = function(e) {
        log_warn("lmer model 2 failed (scoring=", scoring, "): ", conditionMessage(e))
        NULL
      }
    )
    
    list(
      scoring   = scoring,
      model1    = m1,
      model2    = m2,
      long_data = long_sub,
      anova_compare = if (!is.null(m1) && !is.null(m2)) {
        tryCatch(anova(m1, m2), error = function(e) NULL)
      } else NULL
    )
  }
  
  mixed_models$full       <- run_lmer("full")
  mixed_models$restricted <- run_lmer("restricted")
  
  # Log all fixed effects from both models
  for (scoring in c("full", "restricted")) {
    m <- mixed_models[[scoring]]$model1
    if (!is.null(m)) {
      log_h2(paste0("LME fixed effects (", scoring, " scoring)"))
      coef_tbl <- as.data.frame(summary(m)$coefficients)
      coef_tbl$term <- rownames(coef_tbl)
      log_line(sprintf("  %-35s  %8s  %8s  %8s  %8s",
        "Term", "Estimate", "Std.Err", "t", "p"))
      for (i in seq_len(nrow(coef_tbl))) {
        p_col <- if ("Pr(>|t|)" %in% names(coef_tbl)) coef_tbl[["Pr(>|t|)"]][i] else NA_real_
        log_line(sprintf("  %-35s  %+8.4f  %8.4f  %+8.3f  %s",
          coef_tbl$term[i],
          coef_tbl$Estimate[i],
          coef_tbl[["Std. Error"]][i],
          coef_tbl[["t value"]][i],
          fmt_p(p_col)))
      }
      # Random effects variance
      vc <- as.data.frame(lme4::VarCorr(m))
      log_line("  Random effects:")
      for (i in seq_len(nrow(vc))) {
        log_line(sprintf("    %-20s  var=%s",
          paste(vc$grp[i], vc$var1[i], sep=":"),
          round(vc$vcov[i], 4)))
      }
    }
    m2 <- mixed_models[[scoring]]$model2
    if (!is.null(m2)) {
      log_h2(paste0("LME interaction model (", scoring, " scoring)"))
      coef2 <- as.data.frame(summary(m2)$coefficients)
      coef2$term <- rownames(coef2)
      for (i in seq_len(nrow(coef2))) {
        p_col2 <- if ("Pr(>|t|)" %in% names(coef2)) coef2[["Pr(>|t|)"]][i] else NA_real_
        log_line(sprintf("  %-35s  %+8.4f  %s",
          coef2$term[i], coef2$Estimate[i], fmt_p(p_col2)))
      }
      # Log model comparison if available
      comp <- mixed_models[[scoring]]$anova_compare
      if (!is.null(comp)) {
        comp_df <- as.data.frame(comp)
        log_line("  Model comparison (AIC / LRT):")
        log_line(sprintf("    Model 1 AIC=%.2f  BIC=%.2f",
          comp_df$AIC[1], comp_df$BIC[1]))
        log_line(sprintf("    Model 2 AIC=%.2f  BIC=%.2f  Chi2=%.3f  p %s",
          comp_df$AIC[2], comp_df$BIC[2],
          comp_df$Chisq[2] %||% NA_real_,
          fmt_p(comp_df[["Pr(>Chisq)"]][2])))
      }
    }
  }
  
} else {
  log_warn("lme4/lmerTest not available — mixed models skipped.")
  log_warn("Install with: install.packages(c('lme4', 'lmerTest'))")
}

# =============================================================================
# SAVE ALL RESULTS
# =============================================================================

results <- list(
  n                   = nrow(dat),
  alpha               = alpha,
  ci_level            = ci_level,
  scale_to            = scale_to,
  int_label           = int_label,
  ctl_label           = ctl_label,
  descriptives        = descriptives,
  sequence_tbl        = sequence_tbl,
  contrast_intervention_full  = contrast_intervention_full,
  contrast_intervention_restr = contrast_intervention_restr,
  contrast_period_full        = contrast_period_full,
  contrast_period_restr       = contrast_period_restr,
  carryover_full              = carryover_full,
  carryover_restr             = carryover_restr,
  seq_period_full             = seq_period_full,
  seq_period_restr            = seq_period_restr,
  period_int_full             = period_int_full,
  period_int_restr            = period_int_restr,
  sg4_contrasts_full          = sg4_contrasts_full,
  sg4_contrasts_restr         = sg4_contrasts_restr,
  mixed_models                = mixed_models
)

save_rds(results, "analysis_results")

# Record key computed values for SUMMARY.txt audit trail
# Each entry includes enough intermediate values (N, group means/SDs, SD of
# differences, SE, df) that any statistic can be independently verified.
if (exists("session_record_result")) {
  .cif <- contrast_intervention_full

  # --- Sample metadata ---
  session_record_result(
    "n_participants",
    as.character(nrow(dat))
  )
  session_record_result(
    "sequence_groups",
    paste0(sequence_tbl$sequence_group, " (n=", sequence_tbl$n, ")", collapse = ", ")
  )

  # --- Intervention effect (full scoring) ---
  # dz = mean_diff / SD(diff);  SE = SD(diff)/sqrt(N);  t = mean_diff/SE
  session_record_result(
    "intervention_effect_full__sample",
    paste0("N=", .cif$n,
           "  ", int_label, " M=", round(.cif$mean_a, 3), " SD=", round(.cif$sd_a, 3),
           "  ", ctl_label, " M=", round(.cif$mean_b, 3), " SD=", round(.cif$sd_b, 3))
  )
  session_record_result(
    "intervention_effect_full__test",
    paste0("diff=", round(.cif$mean_diff, 3),
           "  SD(diff)=", round(.cif$sd_diff, 3),
           "  SE=", round(.cif$sd_diff / sqrt(.cif$n), 3),
           "  95% CI ", fmt_ci(.cif$ci_lo, .cif$ci_hi),
           "  dz=", round(.cif$dz, 3),
           "  t(", .cif$n - 1L, ")=", round(.cif$t, 3),
           "  p ", fmt_p(.cif$p))
  )

  # --- Intervention effect (restricted scoring, if applicable) ---
  if (!is.null(contrast_intervention_restr) &&
      !is.na(contrast_intervention_restr$p)) {
    .cir <- contrast_intervention_restr
    session_record_result(
      "intervention_effect_restr__sample",
      paste0("N=", .cir$n,
             "  ", int_label, " M=", round(.cir$mean_a, 3), " SD=", round(.cir$sd_a, 3),
             "  ", ctl_label, " M=", round(.cir$mean_b, 3), " SD=", round(.cir$sd_b, 3))
    )
    session_record_result(
      "intervention_effect_restr__test",
      paste0("diff=", round(.cir$mean_diff, 3),
             "  SD(diff)=", round(.cir$sd_diff, 3),
             "  SE=", round(.cir$sd_diff / sqrt(.cir$n), 3),
             "  95% CI ", fmt_ci(.cir$ci_lo, .cir$ci_hi),
             "  dz=", round(.cir$dz, 3),
             "  t(", .cir$n - 1L, ")=", round(.cir$t, 3),
             "  p ", fmt_p(.cir$p))
    )
  }

  # --- Period effect (full scoring) ---
  .cpf <- contrast_period_full
  session_record_result(
    "period_effect_full__sample",
    paste0("N=", .cpf$n,
           "  P2 M=", round(.cpf$mean_a, 3), " SD=", round(.cpf$sd_a, 3),
           "  P1 M=", round(.cpf$mean_b, 3), " SD=", round(.cpf$sd_b, 3))
  )
  session_record_result(
    "period_effect_full__test",
    paste0("diff(P2-P1)=", round(.cpf$mean_diff, 3),
           "  SD(diff)=", round(.cpf$sd_diff, 3),
           "  SE=", round(.cpf$sd_diff / sqrt(.cpf$n), 3),
           "  95% CI ", fmt_ci(.cpf$ci_lo, .cpf$ci_hi),
           "  dz=", round(.cpf$dz, 3),
           "  t(", .cpf$n - 1L, ")=", round(.cpf$t, 3),
           "  p ", fmt_p(.cpf$p))
  )

  # --- Carryover test (Grizzle, full scoring) ---
  # Independent-samples Welch t-test comparing Period-1 scores between sequences
  if (!is.null(carryover_full)) {
    session_record_result(
      "carryover_test__full",
      if (!is.na(carryover_full$p))
        paste0(int_label, "-first n=", carryover_full$n_a,
               " M=", round(carryover_full$mean_a, 3), " SD=", round(carryover_full$sd_a, 3),
               "  ", ctl_label, "-first n=", carryover_full$n_b,
               " M=", round(carryover_full$mean_b, 3), " SD=", round(carryover_full$sd_b, 3),
               "  t=", round(carryover_full$t, 3),
               "  p ", fmt_p(carryover_full$p),
               "  -> ", carryover_full$interpretation)
      else
        paste0("-> ", carryover_full$interpretation)
    )
  }

  # --- Sequence x Period interaction (full scoring) ---
  # Tests whether the period-difference (P2-P1) differs between AB and BA sequences
  if (!is.null(seq_period_full)) {
    session_record_result(
      "seq_period_interaction__full",
      if (!is.na(seq_period_full$p))
        paste0("AB n=", seq_period_full$n_ab,
               " mean-delta-period=", round(seq_period_full$mean_diff_ab, 3),
               " SD=", round(seq_period_full$sd_diff_ab, 3),
               "  BA n=", seq_period_full$n_ba,
               " mean-delta-period=", round(seq_period_full$mean_diff_ba, 3),
               " SD=", round(seq_period_full$sd_diff_ba, 3),
               "  t=", round(seq_period_full$t, 3),
               "  p ", fmt_p(seq_period_full$p),
               "  -> ", seq_period_full$interpretation)
      else
        paste0("-> ", seq_period_full$interpretation)
    )
  }

  # --- Period-specific intervention effect (full scoring) ---
  # Tests whether the within-person Int-minus-Ctl difference varies by which
  # period the intervention was administered (i.e., moderator test)
  if (!is.null(period_int_full)) {
    session_record_result(
      "period_specific_int__full",
      if (!is.na(period_int_full$p))
        paste0(int_label, "-in-P1 n=", period_int_full$n_int_p1,
               " mean-diff=", round(period_int_full$mean_diff_int_p1, 3),
               " SD=", round(period_int_full$sd_diff_int_p1, 3),
               "  ", int_label, "-in-P2 n=", period_int_full$n_int_p2,
               " mean-diff=", round(period_int_full$mean_diff_int_p2, 3),
               " SD=", round(period_int_full$sd_diff_int_p2, 3),
               "  t=", round(period_int_full$t, 3),
               "  p ", fmt_p(period_int_full$p))
      else
        paste0(int_label, "-in-P1 n=", period_int_full$n_int_p1,
               " mean-diff=", round(period_int_full$mean_diff_int_p1, 3),
               "  ", int_label, "-in-P2 n=", period_int_full$n_int_p2,
               " mean-diff=", round(period_int_full$mean_diff_int_p2, 3),
               "  -> ", period_int_full$interpretation)
    )
  }
}
log_line(strrep("-", 60))
log_line("  N participants  : ", nrow(dat))
log_line("  Sequence groups : ",
  paste0(sequence_tbl$sequence_group, " (n=", sequence_tbl$n, ")", collapse = ", "))
log_line(strrep("-", 60))
log_line("  INTERVENTION EFFECT (full scoring)")
log_line("    ", int_label, " mean=", round(contrast_intervention_full$mean_a, 3),
         "  ", ctl_label, " mean=", round(contrast_intervention_full$mean_b, 3))
log_line("    diff=", round(contrast_intervention_full$mean_diff, 3),
         "  95% CI ", fmt_ci(contrast_intervention_full$ci_lo, contrast_intervention_full$ci_hi),
         "  dz=", round(contrast_intervention_full$dz, 3),
         "  t(", contrast_intervention_full$n - 1, ")=",
         round(contrast_intervention_full$t, 3),
         "  p ", fmt_p(contrast_intervention_full$p))
log_line("  PERIOD EFFECT (full scoring)")
log_line("    Period2 mean=", round(contrast_period_full$mean_a, 3),
         "  Period1 mean=", round(contrast_period_full$mean_b, 3))
log_line("    diff=", round(contrast_period_full$mean_diff, 3),
         "  dz=", round(contrast_period_full$dz, 3),
         "  p ", fmt_p(contrast_period_full$p))
if (!is.null(carryover_full) && !is.na(carryover_full$p)) {
  log_line("  CARRYOVER TEST : t=", round(carryover_full$t, 3),
           "  p ", fmt_p(carryover_full$p),
           " -> ", carryover_full$interpretation)
}
if (!is.null(period_int_full) && !is.na(period_int_full$p)) {
  log_line("  PERIOD-SPECIFIC INT EFFECT:")
  log_line("    Int in P1: M(diff)=", round(period_int_full$mean_diff_int_p1, 3),
           "  Int in P2: M(diff)=", round(period_int_full$mean_diff_int_p2, 3))
  log_line("    t=", round(period_int_full$t, 3),
           "  p ", fmt_p(period_int_full$p),
           " -> ", period_int_full$interpretation)
}
if (!is.null(sg4_contrasts_full) && nrow(sg4_contrasts_full) > 0) {
  log_line("  4-SUBGROUP EFFECTS (full scoring):")
  for (i in seq_len(nrow(sg4_contrasts_full))) {
    r <- sg4_contrasts_full[i, ]
    log_line(sprintf("    %-55s  n=%d  diff=%.3f  dz=%.3f  p %s",
      r$subgroup4, r$n, r$mean_diff %||% NA_real_,
      r$dz %||% NA_real_, fmt_p(r$p %||% NA_real_)))
  }
}
log_line(strrep("-", 60))
