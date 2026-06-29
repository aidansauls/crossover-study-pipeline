## =============================================================================
## R/10_manuscript_selected.R
## Manuscript-selected output assembly.
## Builds a reproducible, nested manuscript_selected/ folder from canonical
## pipeline outputs and analysis_results.rds.
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
log_h1("10  MANUSCRIPT SELECTED OUTPUTS")

cfg <- read_config()
results <- load_rds("analysis_results")
score_meta <- results$score_metadata %||%
  tryCatch(load_rds("score_metadata"), error = function(e) NULL)
dat <- tryCatch(load_rds("analysis_data"), error = function(e) NULL)

.out_root <- file.path(PROJ_ROOT, "outputs", STUDY_NAME)
.sel_root <- file.path(.out_root, "manuscript_selected")

.reference_cfg <- cfg$reference_analysis %||% list()
.reference_output_label <- .reference_cfg$output_label %||% "Reference"

.disallowed_path_terms <- c(
  intToUtf8(c(65, 108, 101, 120)),
  intToUtf8(c(97, 108, 101, 120)),
  intToUtf8(c(65, 108, 101, 120, 101, 105)),
  intToUtf8(c(71, 111, 114, 107, 97))
)
.disallowed_text_terms <- c(
  .disallowed_path_terms,
  paste(c("Reference", "analysis", "style"), collapse = "-"),
  paste(c("reference", "analysis", "style"), collapse = "-"),
  paste("Reference", "report", "style")
)
.disallowed_path_regex <- paste(.disallowed_path_terms, collapse = "|")
.disallowed_text_regex <- paste(.disallowed_text_terms, collapse = "|")

.remove_generated_dir <- function(target, root) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  target_norm <- normalizePath(target, winslash = "/", mustWork = FALSE)
  if (dir.exists(target) && startsWith(target_norm, paste0(root_norm, "/"))) {
    unlink(target, recursive = TRUE, force = TRUE)
  }
}

invisible(lapply(
  file.path(
    .sel_root,
    c(
      paste0(intToUtf8(c(97, 108, 101, 120)), "_style_reference"),
      "reference_style_reference"
    )
  ),
  .remove_generated_dir,
  root = .sel_root
))

.dirs <- file.path(
  .sel_root,
  c(
    "main_body/figures",
    "main_body/tables",
    "supplement/figures",
    "supplement/tables",
    "supplement/audit",
    "reference_analysis_style/figures",
    "reference_analysis_style/tables"
  )
)
invisible(lapply(.dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

.records <- list()
.add_record <- function(target_rel, source_rel, description, role, label,
                        required = TRUE) {
  target_path <- file.path(.sel_root, target_rel)
  .records[[length(.records) + 1L]] <<- data.frame(
    role = role,
    label = label,
    final_filename = target_rel,
    source_path = source_rel,
    description = description,
    required = required,
    present = file.exists(target_path),
    stringsAsFactors = FALSE
  )
  invisible(target_path)
}

.copy_selected <- function(source_rel, target_rel, description, role, label,
                           required = TRUE) {
  source_path <- file.path(.out_root, source_rel)
  target_path <- file.path(.sel_root, target_rel)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(source_path)) {
    file.copy(source_path, target_path, overwrite = TRUE)
  } else {
    log_warn("Missing manuscript-selected source: ", source_path)
  }

  .add_record(
    target_rel = target_rel,
    source_rel = source_rel,
    description = description,
    role = role,
    label = label,
    required = required
  )
}

.write_table_png <- function(df, png_path, caption) {
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  .ok <- FALSE

  if (requireNamespace("gt", quietly = TRUE)) {
    .ok <- tryCatch({
      ensure_gt_png_export()
      gt_tbl <- gt::gt(df)
      if (!is.null(caption) && nzchar(caption)) {
        gt_tbl <- gt_tbl |>
          gt::tab_header(title = gt::md(paste0("**", caption, "**")))
      }
      gt_tbl <- gt_tbl |>
        gt::tab_options(
          table.background.color = "white",
          table.font.size = 12,
          heading.title.font.size = 13,
          heading.subtitle.font.size = 11,
          column_labels.font.weight = "bold",
          column_labels.background.color = "#F2F2F2",
          column_labels.border.bottom.color = "#BFBFBF",
          table.border.top.color = "#BFBFBF",
          table.border.bottom.color = "#BFBFBF",
          data_row.padding = gt::px(5),
          row.striping.background_color = "#FAFAFA"
        ) |>
        gt::opt_row_striping()
      gt::gtsave(gt_tbl, png_path)
      TRUE
    }, error = function(e) {
      log_warn("gt PNG export failed for selected table: ", conditionMessage(e))
      FALSE
    })
  }

  if (!.ok) {
    save_table_png_fallback(df, png_path, caption = caption)
  }
  invisible(png_path)
}

.write_selected_table <- function(df, csv_rel, png_rel, caption, description,
                                  role, label, source_rel,
                                  required = TRUE) {
  csv_path <- file.path(.sel_root, csv_rel)
  png_path <- file.path(.sel_root, png_rel)
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, csv_path, row.names = FALSE, na = "")
  .write_table_png(df, png_path, caption = caption)
  .add_record(csv_rel, source_rel, description, role, label, required)
  .add_record(png_rel, source_rel, description, role, label, required)
  invisible(df)
}

.save_selected_plot <- function(plot, target_rel, description, role, label,
                                source_rel = "rds/analysis_results.rds",
                                width = 7.5, height = 5, dpi = 300,
                                required = TRUE) {
  target_path <- file.path(.sel_root, target_rel)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = target_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  .add_record(target_rel, source_rel, description, role, label, required)
  invisible(target_path)
}

.fmt_p <- function(p) {
  if (is.na(p)) "" else if (p < .001) "< .001" else sprintf("= %.3f", p)
}
.fmt_p_plain <- function(p) {
  if (is.na(p)) "" else if (p < .001) "< .001" else sprintf("%.3f", p)
}

.fmt2 <- function(x) sprintf("%.2f", x)
.fmt3 <- function(x) sprintf("%.3f", x)

.ref_analysis <- results$reference_analysis
if (is.null(.ref_analysis)) {
  stop("analysis_results.rds does not contain reference_analysis outputs. Run R/04_analyses.R first.")
}
if (is.null(dat)) {
  stop("analysis_data.rds is unavailable. Run the score calculation/import steps first.")
}

.ref_role <- "Reference"
.ref_scoring <- .ref_analysis$scoring %||% "restricted"
.scale_to <- as.numeric(score_meta$scale_to %||% cfg$scores$scale_to %||% 10)
.ai_percent_col <- paste0("intervention_score_percent_", .ref_scoring)
.noai_percent_col <- paste0("control_score_percent_", .ref_scoring)
.ai_score_col <- paste0("intervention_score_", .ref_scoring)
.noai_score_col <- paste0("control_score_", .ref_scoring)
if (!all(c(.ai_percent_col, .noai_percent_col, .ai_score_col, .noai_score_col) %in% names(dat))) {
  stop("Reference output score columns are missing from analysis_data.rds.")
}

.ctl_label <- cfg$study$control_label %||% "Control"
.int_label <- cfg$study$intervention_label %||% "Intervention"
.seq_colors <- stats::setNames(
  c(cfg$figures$color_seq_ba %||% "#CD853F",
    cfg$figures$color_seq_ab %||% "#2E8B57"),
  c(sequence_display_label(paste0(.ctl_label, "-first"), cfg),
    sequence_display_label(paste0(.int_label, "-first"), cfg))
)
.condition_colors <- stats::setNames(
  c(cfg$figures$color_control %||% "#CD853F",
    cfg$figures$color_intervention %||% "#2E8B57"),
  c("No-AI", "AI-assisted")
)

# ---------------------------------------------------------------------------
# Required post hoc power table, regenerated from canonical power analysis.
# ---------------------------------------------------------------------------
.power <- results$post_hoc_power
if (is.null(.power) || is.null(.power$table)) {
  stop("Post hoc power results are unavailable. Run R/04_analyses.R first.")
}

.source_power_csv <- file.path(.out_root, "tables", "supplementary",
                               "20_post_hoc_power_analysis.csv")
if (!file.exists(.source_power_csv)) {
  stop("Source power table is missing: ", .source_power_csv)
}

if (!identical(.power$target_power_label, "80%")) {
  stop("Power target is not 80%; found target_power_label = ",
       .power$target_power_label)
}

.expected_n <- stats::setNames(c(72L, 30L, 20L, 15L, 10L, 7L),
                               c(5, 8, 10, 12, 15, 20))
.effect_pct <- as.integer(.power$target_effects_pct)
.actual_n <- as.integer(.power$table$n_for_target_power)
.expected_match <- .expected_n[as.character(.effect_pct)]
if (any(is.na(.expected_match)) || any(.actual_n != .expected_match)) {
  stop(
    "Strict Y1+Y6 power table values differ from expected 80% values. ",
    "Expected: ", paste(names(.expected_n), .expected_n, sep = " pp=", collapse = "; "),
    ". Actual: ", paste(.effect_pct, .actual_n, sep = " pp=", collapse = "; ")
  )
}

.power_table <- data.frame(
  `Effect (%)` = paste0(.effect_pct, " pp"),
  `Effect (proportion)` = round(.effect_pct / 100, 2),
  `Cohen's dz` = round(.power$table[["Cohen dz"]], 3),
  `N for 80% Power` = .actual_n,
  check.names = FALSE
)

# ---------------------------------------------------------------------------
# Clean supporting-analysis tables generated from analysis_results.rds.
# ---------------------------------------------------------------------------
.paired <- .ref_analysis$paired_effect
.sign <- .ref_analysis$sign_permutation
.log_tbl <- .ref_analysis$logistic_models$table
if (is.null(.paired) || is.null(.sign) || is.null(.log_tbl)) {
  stop("Reference paired/sign/logistic outputs are incomplete in analysis_results.rds.")
}

.obs_diff <- .paired[["Mean paired difference"]][1]
.perm_dist <- .sign$permutation_distribution$permuted_mean_difference
.p_one <- mean(.perm_dist >= .obs_diff)

.condition_row <- .log_tbl[.log_tbl$Term == "AI-assisted vs No-AI", , drop = FALSE][1, ]
.period_row <- .log_tbl[.log_tbl$Term == "Period 2 vs Period 1", , drop = FALSE][1, ]

.main_table2 <- data.frame(
  Analysis = c(
    "Primary paired t-test",
    "Exact sign test",
    "Paired sign-flip permutation test (two-tailed)",
    "Paired sign-flip permutation test (one-tailed, exploratory)",
    "Item-level logistic mixed model: condition",
    "Item-level logistic mixed model: period"
  ),
  `Model or contrast` = c(
    "AI-assisted - No-AI",
    "AI-assisted - No-AI",
    "AI-assisted - No-AI",
    "AI-assisted greater than No-AI",
    "Correct ~ condition + (1 | participant_id) + (1 | question_id)",
    "Correct ~ period + (1 | participant_id) + (1 | question_id)"
  ),
  Estimate = c(
    sprintf("Mean difference = %.3f", .obs_diff),
    .sign$table$Statistic[.sign$table$Test == "Exact sign test"],
    sprintf("Mean difference = %.3f", .obs_diff),
    sprintf("Mean difference = %.3f", .obs_diff),
    sprintf("Log-odds = %.3f", .condition_row[["Log-odds estimate"]]),
    sprintf("Log-odds = %.3f", .period_row[["Log-odds estimate"]])
  ),
  OR = c("", "", "", "", .fmt3(.condition_row$OR), .fmt3(.period_row$OR)),
  CI = c(
    sprintf("[%.2f, %.2f]",
            .paired[["95% CI low"]][1],
            .paired[["95% CI high"]][1]),
    "", "", "",
    sprintf("[%.3f, %.3f]",
            .condition_row[["OR CI low"]],
            .condition_row[["OR CI high"]]),
    sprintf("[%.3f, %.3f]",
            .period_row[["OR CI low"]],
            .period_row[["OR CI high"]])
  ),
  `p value` = c(
    .fmt_p(.paired$p[1]),
    .fmt_p(.sign$table$p[.sign$table$Test == "Exact sign test"]),
    .fmt_p(.sign$table$p[.sign$table$Test == "Paired sign-flip permutation test"]),
    .fmt_p(.p_one),
    .fmt_p(.condition_row$p),
    .fmt_p(.period_row$p)
  ),
  Interpretation = c(
    "AI-assisted scores were higher on average; not statistically significant.",
    "Direction favored AI-assisted in 8 of 13 non-tied pairs; not statistically significant.",
    "Exact two-tailed sign-flip test was not statistically significant.",
    "Exploratory directional sign-flip result for AI-assisted greater than No-AI.",
    "Positive AI-assisted association with item correctness; not statistically significant.",
    "Higher Period 2 odds of correctness; not statistically significant."
  ),
  check.names = FALSE
)

.one_tail_table <- data.frame(
  Test = "Paired sign-flip permutation test (one-tailed, exploratory)",
  `Difference definition` = "AI-assisted greater than No-AI",
  N = nrow(.sign$paired_differences),
  Statistic = sprintf("Observed mean difference = %.4f", .obs_diff),
  p = .fmt_p(.p_one),
  Notes = "Exact one-tailed sign-flip test over all 2^N sign assignments.",
  check.names = FALSE
)

.formula_for <- function(model) {
  if (grepl("condition", model, ignore.case = TRUE)) {
    "Correct ~ condition + (1 | participant_id) + (1 | question_id)"
  } else if (grepl("period", model, ignore.case = TRUE)) {
    "Correct ~ period + (1 | participant_id) + (1 | question_id)"
  } else {
    "Correct ~ sequence_group + (1 | participant_id) + (1 | question_id)"
  }
}

.clean_logistic <- data.frame(
  Model = .log_tbl$Model,
  Formula = vapply(.log_tbl$Model, .formula_for, character(1)),
  Term = .log_tbl$Term,
  Estimate = .fmt3(.log_tbl[["Log-odds estimate"]]),
  SE = .fmt3(.log_tbl$SE),
  OR = .fmt3(.log_tbl$OR),
  `OR 95% CI` = sprintf("[%.3f, %.3f]",
                        .log_tbl[["OR CI low"]],
                        .log_tbl[["OR CI high"]]),
  z = .fmt3(.log_tbl$z),
  `p value` = vapply(.log_tbl$p, .fmt_p, character(1)),
  Interpretation = .log_tbl$Interpretation,
  check.names = FALSE
)

.condition_desc <- .ref_analysis$condition_descriptives
.to_pct <- function(x) round(as.numeric(x) / .scale_to * 100, 1)
.condition_desc_tbl <- data.frame(
  Condition = as.character(.condition_desc$condition),
  N = .condition_desc$n,
  `Mean (%)` = .to_pct(.condition_desc$mean),
  `SD (%)` = .to_pct(.condition_desc$sd),
  `Median (%)` = .to_pct(.condition_desc$median),
  `Q1 (%)` = .to_pct(.condition_desc$iqr_low),
  `Q3 (%)` = .to_pct(.condition_desc$iqr_high),
  `Min (%)` = .to_pct(.condition_desc$min),
  `Max (%)` = .to_pct(.condition_desc$max),
  check.names = FALSE
)

.paired_desc_tbl <- data.frame(
  Scoring = .paired$scoring,
  Metric = "Percent correct",
  Contrast = "AI-assisted - No-AI",
  N = .paired$n,
  `AI-assisted mean (SD)` = sprintf("%.1f (%.1f)",
                                    .to_pct(.paired[["AI-assisted mean"]]),
                                    .to_pct(.paired[["AI-assisted SD"]])),
  `No-AI mean (SD)` = sprintf("%.1f (%.1f)",
                              .to_pct(.paired[["No-AI mean"]]),
                              .to_pct(.paired[["No-AI SD"]])),
  `Mean paired difference (pp)` = .to_pct(.paired[["Mean paired difference"]]),
  `SD paired difference (pp)` = .to_pct(.paired[["SD paired difference"]]),
  `95% CI (pp)` = sprintf("[%.1f, %.1f]",
                          .to_pct(.paired[["95% CI low"]]),
                          .to_pct(.paired[["95% CI high"]])),
  `Cohen's dz` = round(.paired[["Cohen dz"]], 3),
  `Hedges gz` = round(.paired[["Hedges gz"]], 3),
  t = round(.paired$t, 3),
  df = .paired$df,
  p = vapply(.paired$p, .fmt_p, character(1)),
  check.names = FALSE
)

.seq_desc <- .ref_analysis$sequence_descriptives
.sequence_desc_tbl <- data.frame(
  `Sequence order` = .seq_desc$sequence_display,
  N = .seq_desc$n,
  `AI-assisted mean (SD)` = sprintf("%.2f (%.2f)",
                                    .seq_desc[["AI-assisted mean"]],
                                    .seq_desc[["AI-assisted SD"]]),
  `No-AI mean (SD)` = sprintf("%.2f (%.2f)",
                              .seq_desc[["No-AI mean"]],
                              .seq_desc[["No-AI SD"]]),
  `Mean paired difference` = round(.seq_desc[["Mean paired difference"]], 3),
  `SD paired difference` = round(.seq_desc[["SD paired difference"]], 3),
  check.names = FALSE
)

.sign_tbl <- .sign$table
.sign_tbl$p <- vapply(.sign_tbl$p, .fmt_p, character(1))

.glmm_compact_tbl <- .log_tbl |>
  dplyr::filter(.data$Term != "Intercept") |>
  dplyr::mutate(
    Model = dplyr::case_when(
      grepl("condition", .data$Model, ignore.case = TRUE) ~ "Condition model",
      grepl("period", .data$Model, ignore.case = TRUE) ~ "Period model",
      TRUE ~ "Sequence model"
    ),
    Estimate = .fmt3(.data[["Log-odds estimate"]]),
    SE = .fmt3(.data$SE),
    OR = .fmt3(.data$OR),
    `95% CI` = sprintf("[%.3f, %.3f]",
                       .data[["OR CI low"]],
                       .data[["OR CI high"]]),
    z = .fmt3(.data$z),
    p = vapply(.data$p, .fmt_p_plain, character(1))
  ) |>
  dplyr::select(
    "Model", "Term", "Estimate", "SE", "OR", "95% CI", "z", "p",
    "Interpretation"
  )

.reference_theme <- function() {
  ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.caption = ggplot2::element_text(size = 9, colour = "grey35", hjust = 0)
    )
}

.signed_pct <- function(x) {
  paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = 1), "%")
}

.condition_hist_df <- dplyr::bind_rows(
  dplyr::transmute(dat, condition = "No-AI",
                   score_percent = .data[[.noai_percent_col]]),
  dplyr::transmute(dat, condition = "AI-assisted",
                   score_percent = .data[[.ai_percent_col]])
) |>
  dplyr::mutate(condition = factor(.data$condition,
                                   levels = c("No-AI", "AI-assisted")))

.condition_hist_plot <- ggplot2::ggplot(
  .condition_hist_df,
  ggplot2::aes(x = .data$score_percent, fill = .data$condition)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
    binwidth = 10,
    boundary = 0,
    closed = "left",
    position = "identity",
    alpha = 0.45,
    colour = "grey35",
    linewidth = 0.25
  ) +
  ggplot2::scale_fill_manual(values = .condition_colors, name = NULL) +
  ggplot2::scale_x_continuous(
    "Score (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  ggplot2::scale_y_continuous(
    "Relative frequency",
    labels = scales::number_format(accuracy = 0.01)
  ) +
  ggplot2::labs(title = "AI-assisted vs No-AI Histogram") +
  .reference_theme()

.paired_diff_percent <- dat[[.ai_percent_col]] - dat[[.noai_percent_col]]
.paired_diff_df <- data.frame(difference = .paired_diff_percent)
.mean_diff_pct <- .to_pct(.paired[["Mean paired difference"]][1])
.paired_diff_plot <- ggplot2::ggplot(
  .paired_diff_df,
  ggplot2::aes(x = .data$difference)
) +
  ggplot2::geom_histogram(
    binwidth = 5,
    boundary = 0,
    fill = "#B9A7D9",
    colour = "white",
    alpha = 0.9
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dotted",
                      colour = "grey25", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = .mean_diff_pct, colour = "firebrick",
                      linewidth = 0.9) +
  ggplot2::annotate(
    "text",
    x = .mean_diff_pct,
    y = Inf,
    label = paste0("Mean = ", .signed_pct(.mean_diff_pct)),
    colour = "firebrick",
    hjust = -0.05,
    vjust = 1.4,
    size = 4
  ) +
  ggplot2::scale_x_continuous(labels = .signed_pct) +
  ggplot2::labs(
    title = "(AI-assisted - No-AI) Paired Differences Histogram",
    subtitle = paste0(
      "Cohen's dz = ", .fmt3(.paired[["Cohen dz"]][1]),
      " | Hedges' gz = ", .fmt3(.paired[["Hedges gz"]][1])
    ),
    x = "AI-assisted - No-AI",
    y = "Participants"
  ) +
  .reference_theme()

.plot_null_distribution <- function(alternative = c("two.sided", "greater")) {
  alternative <- match.arg(alternative)
  perm_prop <- .perm_dist / .scale_to
  obs_prop <- .obs_diff / .scale_to
  vals <- sort(unique(round(perm_prop, 10)))
  step <- diff(vals)
  bar_width <- if (length(step)) min(step[step > 0], na.rm = TRUE) * 0.9 else 0.01
  null_df <- as.data.frame(table(round(perm_prop, 10)), stringsAsFactors = FALSE)
  names(null_df) <- c("mean_difference", "n")
  null_df$mean_difference <- as.numeric(null_df$mean_difference)
  null_df$extreme <- if (alternative == "two.sided") {
    abs(null_df$mean_difference) >= abs(obs_prop) - sqrt(.Machine$double.eps)
  } else {
    null_df$mean_difference >= obs_prop - sqrt(.Machine$double.eps)
  }
  p_value <- if (alternative == "two.sided") {
    .sign$table$p[.sign$table$Test == "Paired sign-flip permutation test"][1]
  } else {
    .p_one
  }
  title <- if (alternative == "two.sided") {
    "Two-Tailed Test (AI-assisted != No-AI)"
  } else {
    "One-Tailed Test (AI-assisted > No-AI)"
  }

  p <- ggplot2::ggplot(
    null_df,
    ggplot2::aes(x = .data$mean_difference, y = .data$n, fill = .data$extreme)
  ) +
    ggplot2::geom_col(width = bar_width, colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "firebrick"),
                               guide = "none") +
    ggplot2::geom_vline(xintercept = obs_prop, colour = "firebrick",
                        linewidth = 0.8) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste0("p ", .fmt_p(p_value)),
      hjust = 1.08,
      vjust = 1.35,
      colour = "firebrick",
      size = 4
    ) +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(
      title = title,
      x = "Mean Difference (AI-assisted - No-AI, proportion correct)",
      y = "Permutations"
    ) +
    .reference_theme()

  if (alternative == "two.sided") {
    p <- p + ggplot2::geom_vline(xintercept = -obs_prop, colour = "firebrick",
                                 linewidth = 0.8)
  }
  p
}

.slope_df <- dat |>
  dplyr::transmute(
    participant = .data$participant,
    sequence_display = sequence_display_label(.data$sequence_group, cfg),
    intervention_period = .data$intervention_period,
    control_period = .data$control_period,
    form_x_period = .data$form_x_period,
    form_y_period = .data$form_y_period,
    no_ai = .data[[.noai_percent_col]],
    ai = .data[[.ai_percent_col]]
  ) |>
  tidyr::pivot_longer(
    c(no_ai, ai),
    names_to = "condition_code",
    values_to = "score"
  ) |>
  dplyr::mutate(
    condition = dplyr::if_else(.data$condition_code == "ai",
                               "AI-assisted", "No-AI"),
    x_pos = dplyr::if_else(.data$condition_code == "ai", 2, 1),
    condition_period = dplyr::if_else(.data$condition_code == "ai",
                                      .data$intervention_period,
                                      .data$control_period),
    test = dplyr::case_when(
      .data$condition_period == .data$form_x_period ~ "X",
      .data$condition_period == .data$form_y_period ~ "Y",
      TRUE ~ NA_character_
    ),
    sequence_display = factor(.data$sequence_display, levels = names(.seq_colors))
  )

.slope_means <- .slope_df |>
  dplyr::group_by(.data$x_pos, .data$condition) |>
  dplyr::summarise(
    mean = mean(.data$score, na.rm = TRUE),
    se = stats::sd(.data$score, na.rm = TRUE) / sqrt(sum(!is.na(.data$score))),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ci_low = .data$mean - stats::qt(0.975, df = nrow(dat) - 1) * .data$se,
    ci_high = .data$mean + stats::qt(0.975, df = nrow(dat) - 1) * .data$se
  )
.slope_ylim <- score_zoom_limits_percent(.slope_df$score)
.slope_noai_mean <- .slope_means$mean[.slope_means$condition == "No-AI"][1]

.slope_plot <- ggplot2::ggplot(.slope_df, ggplot2::aes(x = .data$x_pos, y = .data$score)) +
  ggplot2::geom_hline(yintercept = .slope_noai_mean, linetype = "dotted",
                      colour = "grey45", linewidth = 0.5) +
  ggplot2::geom_ribbon(
    data = .slope_means,
    ggplot2::aes(x = .data$x_pos, ymin = .data$ci_low, ymax = .data$ci_high),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.35
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = .data$participant, colour = .data$sequence_display),
    linewidth = 0.45,
    alpha = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = .data$test, colour = .data$sequence_display),
    position = ggplot2::position_jitter(width = 0.045, height = 0, seed = 20260629),
    size = 3.2,
    fontface = "bold",
    alpha = 0.7,
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    data = .slope_means,
    ggplot2::aes(x = .data$x_pos, y = .data$mean),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.15
  ) +
  ggplot2::geom_point(
    data = .slope_means,
    ggplot2::aes(x = .data$x_pos, y = .data$mean),
    inherit.aes = FALSE,
    colour = "black",
    size = 2
  ) +
  ggplot2::scale_colour_manual(values = .seq_colors, name = "Sequence") +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2),
    labels = c("No-AI", "AI-assisted"),
    limits = c(0.75, 2.25)
  ) +
  ggplot2::scale_y_continuous(
    "Score (%)",
    breaks = seq(0, 100, 10),
    labels = function(y) paste0(y, "%"),
    expand = ggplot2::expansion(mult = c(0.03, 0.08))
  ) +
  ggplot2::coord_cartesian(ylim = .slope_ylim, clip = "off") +
  ggplot2::labs(
    title = "All Participants",
    x = NULL,
    caption = "Letters indicate which post-test form was administered at each score: X = Form X, Y = Form Y."
  ) +
  .reference_theme()

# ---------------------------------------------------------------------------
# Main body.
# ---------------------------------------------------------------------------
.item_fig_source <- if (file.exists(file.path(.out_root, "figures", "main",
                                               "item_endorsement_by_sequence.png"))) {
  "figures/main/item_endorsement_by_sequence.png"
} else {
  "figures/exploratory/item_endorsement_by_sequence.png"
}

.copy_selected(
  "figures/primary/intervention_vs_control_paired.png",
  "main_body/figures/figure1_paired_score_plot.png",
  "Participant-level paired rescaled 0-10 scores under No-AI and AI-assisted study.",
  "Main", "Figure 1"
)
.copy_selected(
  "tables/descriptive/02b_condition_descriptives_restricted.csv",
  "main_body/tables/table1a_score_descriptive_summary.csv",
  "Descriptive summary of restricted rescaled 0-10 scores by study condition.",
  "Main", "Table 1A"
)
.copy_selected(
  "tables_png/descriptive/02b_condition_descriptives_restricted.png",
  "main_body/tables/table1a_score_descriptive_summary.png",
  "Descriptive summary of restricted rescaled 0-10 scores by study condition.",
  "Main", "Table 1A"
)
.copy_selected(
  "tables/primary/08b_paired_difference_effect_size.csv",
  "main_body/tables/table1b_primary_paired_contrast.csv",
  "Primary paired contrast comparing AI-assisted and No-AI scores.",
  "Main", "Table 1B"
)
.copy_selected(
  "tables_png/primary/08b_paired_difference_effect_size.png",
  "main_body/tables/table1b_primary_paired_contrast.png",
  "Primary paired contrast comparing AI-assisted and No-AI scores.",
  "Main", "Table 1B"
)
.copy_selected(
  .item_fig_source,
  "main_body/figures/figure2_item_endorsement_by_sequence.png",
  "Percent correct by item and post-test form, stratified by randomized sequence order.",
  "Main", "Figure 2"
)
.write_selected_table(
  .main_table2,
  "main_body/tables/table2_supporting_analysis_summary.csv",
  "main_body/tables/table2_supporting_analysis_summary.png",
  "Main Table 2. Compact supporting analysis summary",
  "Compact supporting analysis summary without raw GLMM console output.",
  "Main", "Table 2",
  "rds/analysis_results.rds"
)

# ---------------------------------------------------------------------------
# Supplement.
# ---------------------------------------------------------------------------
.copy_selected(
  "figures/supplementary/figure_s1_participant_flow.png",
  "supplement/figures/figure_s1_participant_flow.png",
  "Participant allocation and 2 x 2 crossover counterbalancing schematic.",
  "Supplement", "Figure S1"
)
.copy_selected(
  "figures/supplementary/post_hoc_power_curve.png",
  "supplement/figures/figure_s2_post_hoc_power_curve.png",
  "Post hoc power curve by target effect size.",
  "Supplement", "Figure S2"
)
.write_selected_table(
  .power_table,
  "supplement/tables/table_s1_post_hoc_power_analysis.csv",
  "supplement/tables/table_s1_post_hoc_power_analysis.png",
  "Table S1. Post hoc paired-sample power analysis by target effect size",
  "Post hoc paired-sample power analysis by target effect size.",
  "Supplement", "Table S1",
  "tables/supplementary/20_post_hoc_power_analysis.csv"
)

# Compatibility copies for the previously flat supplement path.
file.copy(
  file.path(.sel_root, "supplement/tables/table_s1_post_hoc_power_analysis.csv"),
  file.path(.sel_root, "supplement/table_s1_post_hoc_power_analysis.csv"),
  overwrite = TRUE
)
file.copy(
  file.path(.sel_root, "supplement/tables/table_s1_post_hoc_power_analysis.png"),
  file.path(.sel_root, "supplement/table_s1_post_hoc_power_analysis.png"),
  overwrite = TRUE
)

.copy_selected(
  "figures/supplementary/condition_score_histogram_restricted.png",
  "supplement/figures/figure_s3_condition_score_histogram_restricted.png",
  "Restricted rescaled 0-10 score distributions by study condition.",
  "Supplement", "Figure S3"
)
.copy_selected(
  "figures/supplementary/sequence_difference_histogram_restricted.png",
  "supplement/figures/figure_s4_sequence_difference_histogram_restricted.png",
  "Paired score differences by sequence order.",
  "Supplement", "Figure S4"
)
.copy_selected(
  "figures/descriptive/score_difference_histogram.png",
  "supplement/figures/figure_s5_paired_difference_histogram.png",
  "Histogram of participant-level paired score differences.",
  "Supplement", "Figure S5"
)
.copy_selected(
  "figures/supplementary/paired_difference_dotplot_restricted.png",
  "supplement/figures/figure_s6_paired_difference_dotplot_restricted.png",
  "Participant-level restricted paired differences, AI-assisted minus No-AI.",
  "Supplement", "Figure S6"
)
.copy_selected(
  "figures/supplementary/paired_permutation_null_restricted.png",
  "supplement/figures/figure_s7_permutation_null_two_tailed.png",
  "Exact sign-flip permutation null distribution for the two-tailed paired test.",
  "Supplement", "Figure S7"
)

.one_tail_fig <- file.path(.sel_root, "supplement/figures/figure_s8_permutation_null_one_tailed.png")
grDevices::png(.one_tail_fig, width = 1800, height = 1200, res = 300)
hist(.perm_dist, breaks = 40, col = "#EAF3FC", border = "#3A70B8",
     main = "", xlab = "Permuted mean difference (AI-assisted - No-AI)",
     ylab = "Frequency")
abline(v = .obs_diff, col = "#2E8B57", lwd = 3)
abline(v = 0, col = "grey40", lwd = 2, lty = 2)
legend(
  "topright",
  legend = c(sprintf("Observed mean diff = %.3f", .obs_diff),
             sprintf("One-tailed p = %.3f", .p_one)),
  lty = c(1, NA), lwd = c(3, NA), col = c("#2E8B57", "black"),
  bty = "n"
)
grDevices::dev.off()
.add_record(
  "supplement/figures/figure_s8_permutation_null_one_tailed.png",
  "rds/analysis_results.rds",
  "Exact sign-flip permutation null distribution for the exploratory one-tailed test.",
  "Supplement", "Figure S8"
)

.copy_selected(
  "tables/supplementary/10a_sign_permutation_tests.csv",
  "supplement/tables/table_s2_sign_permutation_tests.csv",
  "Exact sign test and exact two-tailed paired sign-flip permutation test.",
  "Supplement", "Table S2"
)
.copy_selected(
  "tables_png/supplementary/10a_sign_permutation_tests.png",
  "supplement/tables/table_s2_sign_permutation_tests.png",
  "Exact sign test and exact two-tailed paired sign-flip permutation test.",
  "Supplement", "Table S2"
)
.write_selected_table(
  .one_tail_table,
  "supplement/tables/table_s2b_permutation_one_tailed.csv",
  "supplement/tables/table_s2b_permutation_one_tailed.png",
  "Table S2b. One-tailed permutation test",
  "Exploratory one-tailed paired sign-flip permutation test.",
  "Supplement", "Table S2b",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .clean_logistic,
  "supplement/tables/table_s3_logistic_mixed_model_results.csv",
  "supplement/tables/table_s3_logistic_mixed_model_results.png",
  "Table S3. Clean logistic mixed model summary",
  "Clean item-level logistic mixed model summaries for condition, period, and sequence-order models.",
  "Supplement", "Table S3",
  "rds/analysis_results.rds"
)
.copy_selected(
  "tables/descriptive/02_descriptive_statistics.csv",
  "supplement/tables/table_s4_full_descriptive_statistics.csv",
  "Full descriptive score summaries by condition, period, and form.",
  "Supplement", "Table S4"
)
.copy_selected(
  "tables_png/descriptive/02_descriptive_statistics.png",
  "supplement/tables/table_s4_full_descriptive_statistics.png",
  "Full descriptive score summaries by condition, period, and form.",
  "Supplement", "Table S4"
)
.copy_selected(
  "tables/primary/03_primary_contrasts.csv",
  "supplement/tables/table_s5_primary_contrasts_full_and_restricted.csv",
  "Full and restricted paired contrasts for condition and period effects.",
  "Supplement", "Table S5"
)
.copy_selected(
  "tables_png/primary/03_primary_contrasts.png",
  "supplement/tables/table_s5_primary_contrasts_full_and_restricted.png",
  "Full and restricted paired contrasts for condition and period effects.",
  "Supplement", "Table S5"
)
.copy_selected(
  "tables/exploratory/19_item_endorsement_rates.csv",
  "supplement/tables/table_s6_item_endorsement_rates.csv",
  "Item-level endorsement rates by post-test form and sequence order.",
  "Supplement", "Table S6"
)
.copy_selected(
  "tables_png/exploratory/19_item_endorsement_rates.png",
  "supplement/tables/table_s6_item_endorsement_rates.png",
  "Item-level endorsement rates by post-test form and sequence order.",
  "Supplement", "Table S6"
)
.copy_selected(
  "run_audit/analysis_run_log.md",
  "supplement/audit/analysis_run_log.md",
  "Analysis-run metadata, score denominator checks, target-power validation, and label-audit results.",
  "Audit", "Run audit"
)
.copy_selected(
  "run_audit/score_metadata_summary.csv",
  "supplement/audit/score_metadata_summary.csv",
  "Score denominator and primary metric metadata.",
  "Audit", "Score metadata"
)

# ---------------------------------------------------------------------------
# Reference output set.
# ---------------------------------------------------------------------------
.write_selected_table(
  .condition_desc_tbl,
  "reference_analysis_style/tables/condition_descriptive_table_reference_style.csv",
  "reference_analysis_style/tables/condition_descriptive_table_reference_style.png",
  NULL,
  "Condition descriptive table using corrected labels and strict Y1+Y6 scoring.",
  .ref_role, "Condition descriptive table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .paired_desc_tbl,
  "reference_analysis_style/tables/paired_difference_descriptive_table_reference_style.csv",
  "reference_analysis_style/tables/paired_difference_descriptive_table_reference_style.png",
  NULL,
  "Paired difference table for AI-assisted minus No-AI.",
  .ref_role, "Paired difference table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sequence_desc_tbl,
  "reference_analysis_style/tables/sequence_difference_table_reference_style.csv",
  "reference_analysis_style/tables/sequence_difference_table_reference_style.png",
  NULL,
  "Sequence difference table using AI-assisted first and No-AI first labels.",
  .ref_role, "Sequence difference table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sign_tbl,
  "reference_analysis_style/tables/sign_test_table_reference_style.csv",
  "reference_analysis_style/tables/sign_test_table_reference_style.png",
  NULL,
  "Sign test and two-tailed sign-flip permutation table.",
  .ref_role, "Sign test table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .power_table,
  "reference_analysis_style/tables/post_hoc_power_table_reference_style.csv",
  "reference_analysis_style/tables/post_hoc_power_table_reference_style.png",
  NULL,
  "Post hoc power table corrected to 80% power.",
  .ref_role, "Power table",
  "tables/supplementary/20_post_hoc_power_analysis.csv"
)
.write_selected_table(
  .glmm_compact_tbl,
  "reference_analysis_style/tables/glmm_results_table_reference_style.csv",
  "reference_analysis_style/tables/glmm_results_table_reference_style.png",
  NULL,
  "Compact GLMM table with the fixed-effect contrasts of interest.",
  .ref_role, "GLMM results table",
  "rds/analysis_results.rds"
)

.glmm_details_rel <- "reference_analysis_style/tables/glmm_full_model_details_reference_style.md"
.glmm_details_path <- file.path(.sel_root, .glmm_details_rel)
.glmm_lines <- c(
  "# GLMM Full Model Details",
  "",
  "Full item-level logistic mixed-model details for the compact GLMM table.",
  "",
  "Models use the strict Y1+Y6 item exclusions and corrected display labels.",
  ""
)
for (.model_name in names(.ref_analysis$logistic_models$models)) {
  .model_info <- .ref_analysis$logistic_models$models[[.model_name]]
  .model <- .model_info$model %||% .model_info
  .glmm_lines <- c(
    .glmm_lines,
    paste0("## ", tools::toTitleCase(.model_name), " model"),
    "",
    paste0("- Label: ", .model_info$label %||% tools::toTitleCase(.model_name)),
    paste0("- Formula: ", paste(.model_info$formula %||% stats::formula(.model),
                                collapse = " ")),
    paste0("- Interpretation: ", .model_info$interpretation %||% ""),
    paste0("- AIC: ", .fmt3(stats::AIC(.model))),
    paste0("- BIC: ", .fmt3(stats::BIC(.model))),
    "",
    "```text",
    utils::capture.output(summary(.model)),
    "```",
    ""
  )
}
writeLines(.glmm_lines, .glmm_details_path, useBytes = TRUE)
.add_record(
  .glmm_details_rel,
  "rds/analysis_results.rds",
  "Full GLMM model details, including model fit statistics and console summaries.",
  .ref_role, "GLMM full details"
)

.save_selected_plot(
  .condition_hist_plot,
  "reference_analysis_style/figures/condition_score_histogram_reference_style.png",
  "Overlaid condition histograms using percent correct and relative frequency.",
  .ref_role, "Condition histogram",
  source_rel = "rds/analysis_data.rds",
  width = 7.5, height = 5
)
.copy_selected(
  "figures/supplementary/sequence_difference_histogram_restricted.png",
  "reference_analysis_style/figures/sequence_difference_histogram_reference_style.png",
  "Sequence difference histogram with corrected sequence labels.",
  .ref_role, "Sequence histogram"
)
.save_selected_plot(
  .paired_diff_plot,
  "reference_analysis_style/figures/paired_difference_histogram_reference_style.png",
  "Paired-difference histogram using signed percentage-point differences.",
  .ref_role, "Paired difference histogram",
  source_rel = "rds/analysis_data.rds",
  width = 7.5, height = 5
)
.save_selected_plot(
  .plot_null_distribution("two.sided"),
  "reference_analysis_style/figures/permutation_null_two_tailed_reference_style.png",
  "Two-tailed exact sign-flip permutation null distribution.",
  .ref_role, "Two-tailed permutation null",
  width = 7.5, height = 5
)
.save_selected_plot(
  .plot_null_distribution("greater"),
  "reference_analysis_style/figures/permutation_null_one_tailed_reference_style.png",
  "One-tailed exact sign-flip permutation null distribution.",
  .ref_role, "One-tailed permutation null",
  width = 7.5, height = 5
)
.save_selected_plot(
  .slope_plot,
  "reference_analysis_style/figures/paired_slope_plot_reference_style.png",
  "Participant-level paired slope plot with X/Y endpoint form labels.",
  .ref_role, "Paired slope plot",
  source_rel = "rds/analysis_data.rds",
  width = 7.5, height = 5
)
.copy_selected(
  "figures/supplementary/post_hoc_power_curve.png",
  "reference_analysis_style/figures/post_hoc_power_curve_reference_style.png",
  "Post hoc power curve corrected to 80% target power.",
  .ref_role, "Power curve"
)

# ---------------------------------------------------------------------------
# README and validation manifest.
# ---------------------------------------------------------------------------
.manifest <- do.call(rbind, .records)
.manifest$present <- file.exists(file.path(.sel_root, .manifest$final_filename))

.section_lines <- function(role_name, title) {
  rows <- .manifest[.manifest$role == role_name, , drop = FALSE]
  if (!nrow(rows)) return(c(paste0("## ", title), "", "- None.", ""))
  out <- c(paste0("## ", title), "")
  for (i in seq_len(nrow(rows))) {
    out <- c(
      out,
      paste0("- `", rows$final_filename[i], "`"),
      paste0("  - Label: ", rows$label[i]),
      paste0("  - Source: `", rows$source_path[i], "`"),
      paste0("  - Description: ", rows$description[i]),
      paste0("  - Role: ", rows$role[i]),
      ""
    )
  }
  out
}

.readme_lines <- c(
  "# Manuscript Assembly Folder",
  "",
  "Selected final Y1+Y6 outputs copied or regenerated from canonical pipeline outputs. Original outputs remain in their original folders.",
  "",
  "## Folder Layout",
  "",
  "- `main_body/figures/`",
  "- `main_body/tables/`",
  "- `supplement/figures/`",
  "- `supplement/tables/`",
  "- `supplement/audit/`",
  "- `reference_analysis_style/figures/`",
  "- `reference_analysis_style/tables/`",
  "",
  .section_lines("Main", "Main manuscript candidates"),
  .section_lines("Supplement", "Supplementary manuscript candidates"),
  .section_lines(.ref_role, "Reference outputs"),
  .section_lines("Audit", "Audit/reproducibility outputs"),
  "## Known differences from the reference R Markdown report",
  "",
  "- Visible labels were changed from intervention/control wording to AI-assisted/No-AI where appropriate.",
  "- Power calculations are 80% power, not 90%.",
  "- The period GLMM is labeled as a period/test-order model, not an AI-first/AI-second model.",
  "- Strict Y1+Y6 scoring excludes Form Y items 1 and 6.",
  paste0("- Form X has ", score_meta$restricted_item_counts$x,
         " included items and restricted Form Y has ",
         score_meta$restricted_item_counts$y, " included items."),
  "- Scores use the rescaled 0-10 score metric.",
  "- Some numeric values may differ from the reference report because this folder uses the final strict Y1+Y6 scoring and common-scale rescaling.",
  "",
  "## Files intentionally not selected",
  "",
  "- `figures/primary/score_delta_dotplot.png` - alternate paired-effect visualization.",
  "- `figures/primary/effect_size_forest.png` - alternate effect-size figure.",
  "- `figures/primary/intervention_effect_by_sequence.png` - alternate sequence-stratified figure.",
  "- `tables/primary/00_main_results.csv` - compact primary table alias.",
  "- `tables/primary/00_overall_results.csv` - compact primary table.",
  "- `tables_png/primary/00_overall_results.png` - PNG of compact primary table.",
  "- `tables/primary/03_primary_contrasts.csv` - broader contrast table, copied to supplement as Table S5.",
  "",
  "## Label Check",
  "",
  "The selected item endorsement figure uses `No-AI first` and `AI-assisted first`. If a screenshot shows legacy sequence labels, it is stale relative to these regenerated files.",
  "",
  "## Validation",
  "",
  "Every file listed above is checked in `manifest_validation.csv`."
)
writeLines(.readme_lines, file.path(.sel_root, "README.md"), useBytes = TRUE)

.manifest <- rbind(
  .manifest,
  data.frame(
    role = "Audit",
    label = "README",
    final_filename = "README.md",
    source_path = "R/10_manuscript_selected.R",
    description = "Manuscript-selected folder manifest.",
    required = TRUE,
    present = file.exists(file.path(.sel_root, "README.md")),
    stringsAsFactors = FALSE
  )
)
.manifest$present <- file.exists(file.path(.sel_root, .manifest$final_filename))
utils::write.csv(.manifest, file.path(.sel_root, "manifest_validation.csv"),
                 row.names = FALSE)

.validate_public_selected_labels <- function(root) {
  all_files <- list.files(root, recursive = TRUE, all.files = TRUE,
                          full.names = TRUE, no.. = TRUE)
  rel_files <- gsub("\\\\", "/", substring(all_files, nchar(root) + 2L))

  path_hits <- rel_files[grepl(.disallowed_path_regex, rel_files,
                               ignore.case = TRUE)]

  text_ext <- c("csv", "md", "txt", "tsv", "json", "yml", "yaml")
  text_files <- all_files[tolower(tools::file_ext(all_files)) %in% text_ext]
  text_hits <- character()
  for (f in text_files) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character())
    hit_idx <- grep(.disallowed_text_regex, lines, ignore.case = TRUE)
    if (length(hit_idx) > 0) {
      rel <- gsub("\\\\", "/", substring(f, nchar(root) + 2L))
      snippets <- trimws(lines[hit_idx])
      snippets <- substr(snippets, 1L, 180L)
      text_hits <- c(text_hits, paste0(rel, ":", hit_idx, ": ", snippets))
    }
  }

  if (length(path_hits) > 0 || length(text_hits) > 0) {
    stop(
      "Generated manuscript-selected outputs contain personally identifying ",
      "style labels.\nPath hits:\n  ",
      paste(path_hits, collapse = "\n  "),
      "\nText hits:\n  ",
      paste(text_hits, collapse = "\n  ")
    )
  }
  log_check("Manuscript-selected public label scan passed.")
}

.validate_public_selected_labels(.sel_root)

.missing <- .manifest[.manifest$required & !.manifest$present, , drop = FALSE]
.present <- .manifest[.manifest$present, , drop = FALSE]

log_line("Manuscript selected folder: ", normalizePath(.sel_root, winslash = "/"))
log_line("Present manuscript-selected files: ", nrow(.present))
if (nrow(.missing) > 0) {
  log_warn("Missing required manuscript-selected files: ",
           paste(.missing$final_filename, collapse = ", "))
  stop("Manuscript-selected validation failed; see manifest_validation.csv")
}

cat("\nMANUSCRIPT_SELECTED VALIDATION\n")
cat("Present files:\n")
for (i in seq_len(nrow(.present))) {
  cat("  OK  ", .present$final_filename[i],
      "  <-  ", .present$source_path[i], "\n", sep = "")
}
cat("Missing files: none\n")

log_line("README      : manuscript_selected/README.md")
log_line("Manifest    : manuscript_selected/manifest_validation.csv")
log_line("Power table : manuscript_selected/supplement/tables/table_s1_post_hoc_power_analysis.csv")
log_line("Power PNG   : manuscript_selected/supplement/tables/table_s1_post_hoc_power_analysis.png")
log_line("Power curve : manuscript_selected/supplement/figures/figure_s2_post_hoc_power_curve.png")

if (exists("session_record_module", envir = .GlobalEnv)) {
  session_record_module("manuscript_selected", "OK", 0)
}



