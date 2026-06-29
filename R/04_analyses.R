## =============================================================================
## R/04_analyses.R
## Primary statistical analyses:
##   1. Descriptive statistics
##   2. Intervention effect (paired contrast)
##   3. Period effects (practice, carryover via Grizzle test)
##   4. Sequence x period interaction
##   5. Linear mixed-effects models
## Copyright (c) 2026 Aidan Sauls â€” see LICENSE for terms.
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
raw_data    <- tryCatch(load_rds("raw_data"), error = function(e) NULL)
score_meta  <- tryCatch(load_rds("score_metadata"), error = function(e) NULL)

alpha       <- as.numeric(cfg$analysis$alpha        %||% 0.05)
ci_level    <- as.numeric(cfg$analysis$ci_level     %||% 0.95)
min_n_carry <- as.integer(cfg$analysis$min_n_carryover %||% 4)
scale_to    <- as.numeric(cfg$scores$scale_to       %||% 10)
target_power <- as.numeric(cfg$power_analysis$target_power %||% 0.80)
target_power_label <- scales::percent(target_power, accuracy = 1)
target_effects_pct <- as.numeric(unlist(
  cfg$power_analysis$target_effects_pct %||% c(5, 8, 10, 12, 15, 20)
))
score_label <- score_metric_label(score_meta, scale_to)
int_label   <- cfg$study$intervention_label %||% "Intervention"
ctl_label   <- cfg$study$control_label      %||% "Control"
int_display <- condition_display_label(int_label, cfg)
ctl_display <- condition_display_label(ctl_label, cfg)
form_x_lbl  <- cfg$study$form_x_label       %||% "Form X"
form_y_lbl  <- cfg$study$form_y_label       %||% "Form Y"
.primary_scoring <- if ("intervention_score_restricted" %in% names(dat)) "restricted" else "full"
.primary_ai_col <- paste0("intervention_score_", .primary_scoring)
.primary_noai_col <- paste0("control_score_", .primary_scoring)
.primary_score_label <- paste0(score_label, " (", .primary_scoring, " scoring)")

log_check("Participant-level score metric: ", score_label)
if (!is.null(score_metric_note(score_meta))) log_warn(score_metric_note(score_meta))

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
validate_score_columns(desc_cols, score_meta, "descriptive statistics")

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
  validate_score_columns(c(a_col, b_col), score_meta,
                         paste0("paired intervention contrast (", scoring, ")"))
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
# 2b. POST-HOC POWER / SAMPLE-SIZE ANALYSIS
# Uses the participant-level paired difference under final restricted scoring.
# Target effects are entered as percentage points and converted to the configured
# common score scale before computing paired-test power.
# =============================================================================
log_h2("Post-hoc power/sample-size analysis")

post_hoc_power <- NULL
.run_power <- isTRUE(cfg$optional_analyses$run_post_hoc_power %||% TRUE)

if (!.run_power) {
  log_line("Post-hoc power analysis skipped: optional_analyses.run_post_hoc_power = false")
} else {
  .ai_col <- "intervention_score_restricted"
  .noai_col <- "control_score_restricted"
  if (!all(c(.ai_col, .noai_col) %in% names(dat))) {
    log_warn("Post-hoc power analysis skipped: restricted score columns not found.")
  } else {
    .power_solver <- if (requireNamespace("pwr", quietly = TRUE)) {
      "pwr::pwr.t.test"
    } else {
      "stats::power.t.test compatibility wrapper"
    }
    pwr.t.test <- if (requireNamespace("pwr", quietly = TRUE)) {
      pwr::pwr.t.test
    } else {
      function(n = NULL, d = NULL, sig.level = 0.05, power = NULL,
               type = c("two.sample", "one.sample", "paired"),
               alternative = c("two.sided", "less", "greater")) {
        type <- match.arg(type)
        alternative <- match.arg(alternative)
        stats::power.t.test(
          n = n,
          delta = d,
          sd = 1,
          sig.level = sig.level,
          power = power,
          type = type,
          alternative = alternative
        )
      }
    }

    validate_score_columns(c(.ai_col, .noai_col), score_meta,
                           "post-hoc power analysis")

    .power_df <- dat |>
      dplyr::transmute(
        participant = .data$participant,
        ai_score = .data[[.ai_col]],
        no_ai_score = .data[[.noai_col]],
        paired_diff = .data$ai_score - .data$no_ai_score
      ) |>
      dplyr::filter(!is.na(.data$paired_diff))

    .sd_diff <- stats::sd(.power_df$paired_diff)
    .n_pairs <- nrow(.power_df)

    if (.n_pairs < 2L || is.na(.sd_diff) || .sd_diff <= 0) {
      log_warn("Post-hoc power analysis skipped: paired differences have insufficient variance.")
    } else {
      .target_effect_score <- target_effects_pct / 100 * scale_to
      .target_dz <- .target_effect_score / .sd_diff

      .n_required <- vapply(.target_dz, function(.d) {
        ceiling(pwr.t.test(
          d = .d,
          sig.level = alpha,
          power = target_power,
          type = "paired",
          alternative = "two.sided"
        )$n)
      }, numeric(1))

      .power_observed_n <- vapply(.target_dz, function(.d) {
        pwr.t.test(
          n = .n_pairs,
          d = .d,
          sig.level = alpha,
          type = "paired",
          alternative = "two.sided"
        )$power
      }, numeric(1))

      .max_n_cfg <- suppressWarnings(as.integer(cfg$power_analysis$max_n %||% NA_integer_))
      .max_n <- if (!is.na(.max_n_cfg)) {
        max(.max_n_cfg, .n_pairs, 2L)
      } else {
        max(30L, .n_pairs, ceiling(max(.n_required, na.rm = TRUE) * 1.15))
      }

      .curve_grid <- expand.grid(
        n_pairs = seq.int(2L, .max_n),
        target_effect_pct = target_effects_pct,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      ) |>
        dplyr::mutate(
          target_effect_score = .data$target_effect_pct / 100 * scale_to,
          cohen_dz = .data$target_effect_score / .sd_diff,
          power = purrr::map2_dbl(.data$n_pairs, .data$cohen_dz, function(.n, .d) {
            pwr.t.test(
              n = .n,
              d = .d,
              sig.level = alpha,
              type = "paired",
              alternative = "two.sided"
            )$power
          })
        ) |>
        dplyr::left_join(
          tibble::tibble(
            target_effect_pct = target_effects_pct,
            n_for_target_power = .n_required
          ),
          by = "target_effect_pct"
        ) |>
        dplyr::mutate(
          target_effect_label = paste0(
            .data$target_effect_pct, " pp (N=", .data$n_for_target_power, ")"
          )
        )

      .power_table <- tibble::tibble(
        `Target effect (percentage points)` = target_effects_pct,
        `Target effect (rescaled score units)` = .target_effect_score,
        `Cohen dz` = .target_dz,
        `Power at observed N` = .power_observed_n,
        n_for_target_power = .n_required
      )

      post_hoc_power <- list(
        scoring = "restricted",
        ai_score_col = .ai_col,
        no_ai_score_col = .noai_col,
        target_power = target_power,
        target_power_label = target_power_label,
        alpha = alpha,
        alternative = "two.sided",
        test = "paired t-test",
        power_solver = .power_solver,
        n_pairs = .n_pairs,
        sd_diff = .sd_diff,
        target_effects_pct = target_effects_pct,
        target_effects_score = .target_effect_score,
        table = .power_table,
        curve = .curve_grid,
        table_path = out_path("tables", "supplementary", "20_post_hoc_power_analysis.csv"),
        figure_path = out_path("figures", "supplementary", "post_hoc_power_curve.png")
      )

      log_check("Post-hoc power target_power = ", sprintf("%.2f", target_power),
                " (", target_power_label, ")")
      log_check("Post-hoc power alpha = ", sprintf("%.2f", alpha))
      log_check("Post-hoc power SD paired differences = ", round(.sd_diff, 4))
      log_check("Post-hoc power target effects = ",
                paste0(target_effects_pct, " pp", collapse = ", "))
      log_check("Post-hoc power table path = ", post_hoc_power$table_path)
      log_check("Post-hoc power figure path = ", post_hoc_power$figure_path)
    }
  }
}

# =============================================================================
# 2c. REFERENCE SUPPORTING ANALYSES (RESTRICTED PRIMARY SCORING)
# =============================================================================
log_h2("Reference supporting analyses")

reference_analysis <- list(
  scoring = .primary_scoring,
  score_label = .primary_score_label,
  condition_descriptives = NULL,
  sequence_descriptives = NULL,
  paired_effect = NULL,
  sign_permutation = NULL,
  logistic_models = NULL
)

if (!all(c(.primary_ai_col, .primary_noai_col) %in% names(dat))) {
  log_warn("Reference supporting analyses skipped: primary score columns not found.")
} else {
  validate_score_columns(c(.primary_ai_col, .primary_noai_col), score_meta,
                         "Reference supporting analyses")

  .score_summary <- function(x) {
    x <- x[!is.na(x)]
    tibble::tibble(
      n = length(x),
      mean = mean(x),
      sd = stats::sd(x),
      median = stats::median(x),
      iqr_low = stats::quantile(x, 0.25, names = FALSE),
      iqr_high = stats::quantile(x, 0.75, names = FALSE),
      min = min(x),
      max = max(x)
    )
  }

  .condition_long_primary <- dplyr::bind_rows(
    dat |>
      dplyr::transmute(
        participant = .data$participant,
        sequence_group = .data$sequence_group,
        sequence_display = sequence_display_label(.data$sequence_group, cfg),
        condition = ctl_display,
        score = .data[[.primary_noai_col]]
      ),
    dat |>
      dplyr::transmute(
        participant = .data$participant,
        sequence_group = .data$sequence_group,
        sequence_display = sequence_display_label(.data$sequence_group, cfg),
        condition = int_display,
        score = .data[[.primary_ai_col]]
      )
  ) |>
    dplyr::mutate(
      condition = factor(.data$condition, levels = c(ctl_display, int_display))
    )

  reference_analysis$condition_descriptives <- .condition_long_primary |>
    dplyr::group_by(.data$condition) |>
    dplyr::summarise(
      n = sum(!is.na(.data$score)),
      mean = mean(.data$score, na.rm = TRUE),
      sd = stats::sd(.data$score, na.rm = TRUE),
      median = stats::median(.data$score, na.rm = TRUE),
      iqr_low = stats::quantile(.data$score, 0.25, na.rm = TRUE, names = FALSE),
      iqr_high = stats::quantile(.data$score, 0.75, na.rm = TRUE, names = FALSE),
      min = min(.data$score, na.rm = TRUE),
      max = max(.data$score, na.rm = TRUE),
      .groups = "drop"
    )

  .paired_df <- dat |>
    dplyr::transmute(
      participant = .data$participant,
      sequence_group = .data$sequence_group,
      sequence_display = sequence_display_label(.data$sequence_group, cfg),
      ai_score = .data[[.primary_ai_col]],
      no_ai_score = .data[[.primary_noai_col]],
      paired_diff = .data$ai_score - .data$no_ai_score
    ) |>
    dplyr::filter(!is.na(.data$paired_diff))

  reference_analysis$sequence_descriptives <- .paired_df |>
    dplyr::group_by(.data$sequence_display) |>
    dplyr::summarise(
      n = dplyr::n(),
      `AI-assisted mean` = mean(.data$ai_score, na.rm = TRUE),
      `AI-assisted SD` = stats::sd(.data$ai_score, na.rm = TRUE),
      `No-AI mean` = mean(.data$no_ai_score, na.rm = TRUE),
      `No-AI SD` = stats::sd(.data$no_ai_score, na.rm = TRUE),
      `Mean paired difference` = mean(.data$paired_diff, na.rm = TRUE),
      `SD paired difference` = stats::sd(.data$paired_diff, na.rm = TRUE),
      .groups = "drop"
    )

  .paired_res <- if (.primary_scoring == "restricted") {
    contrast_intervention_restr %||% contrast_intervention_full
  } else {
    contrast_intervention_full
  }
  .hedges_j <- if (!is.null(.paired_res) && .paired_res$n > 1) {
    1 - 3 / (4 * (.paired_res$n - 1) - 1)
  } else {
    NA_real_
  }
  reference_analysis$paired_effect <- tibble::tibble(
    scoring = .primary_scoring,
    score_metric = score_label,
    comparison = paste0(int_display, " minus ", ctl_display),
    n = .paired_res$n %||% nrow(.paired_df),
    `AI-assisted mean` = .paired_res$mean_a %||% mean(.paired_df$ai_score, na.rm = TRUE),
    `AI-assisted SD` = .paired_res$sd_a %||% stats::sd(.paired_df$ai_score, na.rm = TRUE),
    `No-AI mean` = .paired_res$mean_b %||% mean(.paired_df$no_ai_score, na.rm = TRUE),
    `No-AI SD` = .paired_res$sd_b %||% stats::sd(.paired_df$no_ai_score, na.rm = TRUE),
    `Mean paired difference` = .paired_res$mean_diff %||% mean(.paired_df$paired_diff, na.rm = TRUE),
    `SD paired difference` = .paired_res$sd_diff %||% stats::sd(.paired_df$paired_diff, na.rm = TRUE),
    `95% CI low` = .paired_res$ci_lo %||% NA_real_,
    `95% CI high` = .paired_res$ci_hi %||% NA_real_,
    `Cohen dz` = .paired_res$dz %||% NA_real_,
    `Hedges gz` = (.paired_res$dz %||% NA_real_) * .hedges_j,
    t = .paired_res$t %||% NA_real_,
    df = (.paired_res$n %||% NA_real_) - 1,
    p = .paired_res$p %||% NA_real_
  )

  .diff <- .paired_df$paired_diff
  .n_improved <- sum(.diff > 0, na.rm = TRUE)
  .n_worsened <- sum(.diff < 0, na.rm = TRUE)
  .n_tied <- sum(.diff == 0, na.rm = TRUE)
  .n_nonzero <- .n_improved + .n_worsened
  .sign_p <- if (.n_nonzero > 0) {
    stats::binom.test(.n_improved, .n_nonzero, p = 0.5,
                      alternative = "two.sided")$p.value
  } else {
    NA_real_
  }

  .perm_dist <- tibble::tibble()
  .perm_p <- NA_real_
  .obs_mean <- mean(.diff, na.rm = TRUE)
  .diff_clean <- .diff[!is.na(.diff)]
  .n_perm <- length(.diff_clean)
  if (.n_perm > 0 && .n_perm <= 20) {
    .abs_diff <- abs(.diff_clean)
    .sign_mat <- expand.grid(rep(list(c(-1, 1)), .n_perm))
    .perm_means <- as.numeric(as.matrix(.sign_mat) %*% .abs_diff / .n_perm)
    .perm_p <- mean(abs(.perm_means) >= abs(.obs_mean) - 1e-12)
    .perm_dist <- tibble::tibble(
      permuted_mean_difference = .perm_means
    )
  } else if (.n_perm > 20) {
    set.seed(20260629)
    .n_mc <- 100000L
    .abs_diff <- abs(.diff_clean)
    .perm_means <- replicate(
      .n_mc,
      mean(sample(c(-1, 1), .n_perm, replace = TRUE) * .abs_diff)
    )
    .perm_p <- mean(abs(.perm_means) >= abs(.obs_mean) - 1e-12)
    .perm_dist <- tibble::tibble(permuted_mean_difference = .perm_means)
  }

  reference_analysis$sign_permutation <- list(
    paired_differences = .paired_df,
    counts = tibble::tibble(
      Direction = c(paste0(int_display, " higher"), paste0(ctl_display, " higher"), "Tie"),
      n = c(.n_improved, .n_worsened, .n_tied)
    ),
    table = tibble::tibble(
      Test = c("Exact sign test", "Paired sign-flip permutation test"),
      `Difference definition` = paste0(int_display, " minus ", ctl_display),
      N = c(.n_nonzero, .n_perm),
      Statistic = c(
        paste0(.n_improved, " improved, ", .n_worsened, " worsened, ", .n_tied, " tied"),
        paste0("Observed mean difference = ", round(.obs_mean, 4))
      ),
      p = c(.sign_p, .perm_p),
      Notes = c(
        "Two-sided exact binomial sign test excludes ties.",
        if (.n_perm <= 20) "Exact sign-flip test over all 2^N sign assignments."
        else "Monte Carlo sign-flip test with 100000 sign assignments."
      )
    ),
    permutation_distribution = .perm_dist,
    observed_mean_difference = .obs_mean
  )

  log_check("Reference paired difference definition: ", int_display, " minus ", ctl_display)
  log_check("Reference sign test counts: improved=", .n_improved,
            ", worsened=", .n_worsened, ", tied=", .n_tied)
}

# ---- Item-level logistic mixed models ---------------------------------------
.fit_logistic_models <- isTRUE(cfg_get("optional_analyses", "run_logistic_mixed_models",
                                       default = TRUE))

if (.fit_logistic_models && !is.null(raw_data) && requireNamespace("lme4", quietly = TRUE)) {
  log_h2("Exploratory item-level logistic mixed models")

  .build_items_long <- function(items_df, cols, form_code) {
    if (is.null(items_df) || length(cols) == 0) return(NULL)
    .num_ord <- suppressWarnings(as.numeric(sub("^[xy]", "", cols)))
    .cols_ord <- cols[order(.num_ord)]
    items_df |>
      dplyr::mutate(participant = as.character(.data$participant)) |>
      dplyr::select(participant, dplyr::all_of(.cols_ord)) |>
      tidyr::pivot_longer(
        dplyr::all_of(.cols_ord),
        names_to = "item_raw",
        values_to = "correct_binary"
      ) |>
      dplyr::mutate(
        form_code = form_code,
        question_id = paste0(.data$form_code, "_", toupper(.data$item_raw)),
        correct_binary = as.integer(.data$correct_binary)
      )
  }

  .logistic_items <- dplyr::bind_rows(
    .build_items_long(raw_data$x_items, raw_data$x_cols_restricted, "X"),
    .build_items_long(raw_data$y_items, raw_data$y_cols_restricted, "Y")
  )

  if (!is.null(.logistic_items) && nrow(.logistic_items) > 0) {
    .assign_for_logit <- dat |>
      dplyr::transmute(
        participant = as.character(.data$participant),
        sequence_group = .data$sequence_group,
        sequence_display = sequence_display_label(.data$sequence_group, cfg),
        intervention_period = .data$intervention_period,
        form_x_period = .data$form_x_period,
        form_y_period = .data$form_y_period
      )

    .logit_df <- .logistic_items |>
      dplyr::left_join(.assign_for_logit, by = "participant") |>
      dplyr::mutate(
        period = dplyr::if_else(.data$form_code == "X",
                                .data$form_x_period,
                                .data$form_y_period),
        period_fac = factor(
          paste0("Period ", .data$period),
          levels = c("Period 1", "Period 2")
        ),
        condition_code = dplyr::if_else(
          .data$period == .data$intervention_period,
          "intervention",
          "control"
        ),
        condition_fac = factor(
          dplyr::if_else(.data$condition_code == "intervention",
                         int_display,
                         ctl_display),
          levels = c(ctl_display, int_display)
        ),
        sequence_fac = factor(
          .data$sequence_display,
          levels = sequence_display_label(
            c(paste0(ctl_label, "-first"), paste0(int_label, "-first")),
            cfg
          )
        ),
        participant_id = factor(.data$participant),
        question_id = factor(.data$question_id)
      ) |>
      dplyr::filter(!is.na(.data$correct_binary))

    .valid_binary <- all(.logit_df$correct_binary %in% c(0L, 1L))
    if (!.valid_binary) {
      log_warn("Logistic mixed models skipped: correct_binary contains values other than 0/1.")
    } else {
      .glmer_ctrl <- lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 200000)
      )

      .fit_glmer <- function(formula, label, interpretation) {
        model <- tryCatch(
          lme4::glmer(
            formula,
            data = .logit_df,
            family = stats::binomial(),
            control = .glmer_ctrl,
            nAGQ = 0
          ),
          error = function(e) {
            log_warn(label, " failed: ", conditionMessage(e))
            NULL
          }
        )
        list(label = label, interpretation = interpretation, formula = deparse(formula), model = model)
      }

      .log_models <- list(
        condition = .fit_glmer(
          correct_binary ~ condition_fac + (1 | participant_id) + (1 | question_id),
          "Model A: item-level condition model",
          paste0("Estimates ", int_display, " vs ", ctl_display, " odds of a correct response.")
        ),
        period = .fit_glmer(
          correct_binary ~ period_fac + (1 | participant_id) + (1 | question_id),
          "Model B: item-level period model",
          "Estimates Period 2 vs Period 1 odds of a correct response."
        ),
        sequence = .fit_glmer(
          correct_binary ~ sequence_fac + (1 | participant_id) + (1 | question_id),
          "Model C: item-level sequence-group model",
          paste0("Estimates ", int_display, "-first sequence vs ", ctl_display,
                 "-first sequence odds of a correct response.")
        )
      )

      .term_label <- function(term) {
        dplyr::case_when(
          term == "(Intercept)" ~ "Intercept",
          grepl("^condition_fac", term) ~ paste0(int_display, " vs ", ctl_display),
          grepl("^period_fac", term) ~ "Period 2 vs Period 1",
          grepl("^sequence_fac", term) ~ paste0(
            cfg$display_labels$sequence_ai_first %||% paste0(int_display, " first"),
            " vs ",
            cfg$display_labels$sequence_control_first %||% paste0(ctl_display, " first")
          ),
          TRUE ~ term
        )
      }

      .extract_logit <- function(obj) {
        if (is.null(obj$model)) return(NULL)
        coefs <- as.data.frame(summary(obj$model)$coefficients)
        coefs$term_raw <- rownames(coefs)
        tibble::tibble(
          Model = obj$label,
          Formula = obj$formula,
          Term = .term_label(coefs$term_raw),
          `Log-odds estimate` = coefs$Estimate,
          SE = coefs$`Std. Error`,
          OR = exp(coefs$Estimate),
          `OR CI low` = exp(coefs$Estimate - stats::qnorm(0.975) * coefs$`Std. Error`),
          `OR CI high` = exp(coefs$Estimate + stats::qnorm(0.975) * coefs$`Std. Error`),
          z = coefs$`z value`,
          p = coefs$`Pr(>|z|)`,
          Interpretation = obj$interpretation
        )
      }

      .log_table <- dplyr::bind_rows(lapply(.log_models, .extract_logit))

      reference_analysis$logistic_models <- list(
        data = .logit_df,
        models = .log_models,
        table = .log_table
      )

      log_check("Logistic model data rows = ", nrow(.logit_df),
                "; binary outcome values = ", paste(sort(unique(.logit_df$correct_binary)), collapse = ", "))
    }
  } else {
    log_warn("Logistic mixed models skipped: no restricted item-level rows available.")
  }
} else if (!.fit_logistic_models) {
  log_line("Logistic mixed models skipped: optional_analyses.run_logistic_mixed_models = false")
} else if (is.null(raw_data)) {
  log_warn("Logistic mixed models skipped: raw_data RDS not available.")
} else {
  log_warn("Logistic mixed models skipped: lme4 not available.")
}

# =============================================================================
# 3. PERIOD EFFECTS (practice / learning effects)
# =============================================================================
log_h2("Period effects")

run_period_contrast <- function(scoring) {
  p1_col <- paste0("period1_score_", scoring)
  p2_col <- paste0("period2_score_", scoring)
  if (!p1_col %in% names(dat) || !p2_col %in% names(dat)) return(NULL)
  validate_score_columns(c(p2_col, p1_col), score_meta,
                         paste0("paired period contrast (", scoring, ")"))
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
# Requires â‰¥ min_n_carryover per group (set in config).
# =============================================================================
log_h2("Carryover test (Grizzle)")

run_carryover <- function(scoring) {
  p1_col <- paste0("period1_score_", scoring)
  if (!p1_col %in% names(dat)) return(NULL)
  validate_score_columns(p1_col, score_meta,
                         paste0("carryover test (", scoring, ")"))
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
  validate_score_columns(c(p1_col, p2_col), score_meta,
                         paste0("sequence x period interaction (", scoring, ")"))
  
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
# two sequence groups â€” a significant difference means the period in which
# the intervention occurred moderates(amplifies/dampens) the effect.
# =============================================================================
log_h2("Period-specific intervention effect (period when int. occurred)")

period_specific_int <- function(scoring) {
  int_col <- paste0("intervention_score_", scoring)
  ctl_col <- paste0("control_score_",      scoring)
  if (!int_col %in% names(dat)) return(NULL)
  validate_score_columns(c(int_col, ctl_col), score_meta,
                         paste0("period-specific intervention effect (", scoring, ")"))

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
# For each of the four subgroups (seq Ã— int-form), compute
# the intervention-vs-control contrast. Allows checking whether the
# intervention effect is consistent across all four subgroup configurations.
# =============================================================================
log_h2("4-subgroup intervention contrasts")

subgroup4_contrasts <- function(scoring) {
  int_col  <- paste0("intervention_score_", scoring)
  ctl_col  <- paste0("control_score_",      scoring)
  if (!int_col %in% names(dat)) return(NULL)
  if (!"subgroup4" %in% names(dat)) return(NULL)
  validate_score_columns(c(int_col, ctl_col), score_meta,
                         paste0("4-subgroup contrasts (", scoring, ")"))

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
    validate_score_columns(paste0(c("intervention", "control"), "_score_", scoring),
                           score_meta,
                           paste0("mixed-effects model (", scoring, ")"))
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
  log_warn("lme4/lmerTest not available â€” mixed models skipped.")
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
  score_label         = score_label,
  score_metadata      = score_meta,
  int_label           = int_label,
  ctl_label           = ctl_label,
  descriptives        = descriptives,
  sequence_tbl        = sequence_tbl,
  reference_analysis     = reference_analysis,
  contrast_intervention_full  = contrast_intervention_full,
  contrast_intervention_restr = contrast_intervention_restr,
  post_hoc_power              = post_hoc_power,
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

  if (!is.null(post_hoc_power)) {
    session_record_result(
      "post_hoc_power__settings",
      paste0("target_power=", sprintf("%.2f", post_hoc_power$target_power),
             "  alpha=", sprintf("%.2f", post_hoc_power$alpha),
             "  SD(diff)=", round(post_hoc_power$sd_diff, 4),
             "  target effects=",
             paste0(post_hoc_power$target_effects_pct, " pp", collapse = ", "))
    )
    session_record_result(
      "post_hoc_power__n_for_target",
      paste0(
        post_hoc_power$table[["Target effect (percentage points)"]], " pp: N=",
        post_hoc_power$table[["n_for_target_power"]],
        collapse = "; "
      )
    )
    session_record_result(
      "post_hoc_power__outputs",
      paste0("table=", post_hoc_power$table_path,
             "  figure=", post_hoc_power$figure_path)
    )
  }

  if (!is.null(reference_analysis$paired_effect)) {
    .ape <- reference_analysis$paired_effect
    session_record_result(
      "reference_analysis__paired_effect",
      paste0(
        "scoring=", reference_analysis$scoring,
        "  diff=", round(.ape[["Mean paired difference"]][1], 3),
        "  dz=", round(.ape[["Cohen dz"]][1], 3),
        "  Hedges gz=", round(.ape[["Hedges gz"]][1], 3),
        "  p ", fmt_p(.ape$p[1])
      )
    )
  }

  if (!is.null(reference_analysis$sign_permutation$table)) {
    .spt <- reference_analysis$sign_permutation$table
    session_record_result(
      "reference_analysis__sign_permutation",
      paste0(
        .spt$Test, " p ", vapply(.spt$p, fmt_p, character(1)),
        collapse = "; "
      )
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
if (!is.null(post_hoc_power)) {
  log_line("  POST-HOC POWER (restricted scoring)")
  log_line("    target_power=", sprintf("%.2f", post_hoc_power$target_power),
           "  alpha=", sprintf("%.2f", post_hoc_power$alpha),
           "  SD(diff)=", round(post_hoc_power$sd_diff, 3))
  log_line("    N for ", post_hoc_power$target_power_label, " power: ",
           paste0(post_hoc_power$table[["Target effect (percentage points)"]],
                  " pp=", post_hoc_power$table[["n_for_target_power"]],
                  collapse = ", "))
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

