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

.remove_generated_file <- function(target, root) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  target_norm <- normalizePath(target, winslash = "/", mustWork = FALSE)
  if (file.exists(target) && startsWith(target_norm, paste0(root_norm, "/"))) {
    unlink(target, force = TRUE)
  }
}

.remove_generated_flat_files <- function(target_dir, root) {
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  dir_norm <- normalizePath(target_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(target_dir) ||
      !(identical(dir_norm, root_norm) || startsWith(dir_norm, paste0(root_norm, "/")))) {
    return(invisible(FALSE))
  }
  flat <- list.files(target_dir, full.names = TRUE, recursive = FALSE,
                     all.files = TRUE, no.. = TRUE)
  flat <- flat[file.exists(flat) & !dir.exists(flat)]
  unlink(flat, force = TRUE)
  invisible(TRUE)
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

invisible(lapply(
  c(.sel_root, file.path(.sel_root, c("main_body", "supplement"))),
  .remove_generated_flat_files,
  root = .sel_root
))

invisible(lapply(
  file.path(
    .sel_root,
    c(
      "supplement/figures/figure_s7_permutation_null_two_tailed.png",
      "supplement/figures/figure_s8_permutation_null_one_tailed.png",
      "supplement/tables/table_s2b_permutation_one_tailed.csv",
      "supplement/tables/table_s2b_permutation_one_tailed.png",
      "supplement/tables/table_s2b_permutation_one_sided.csv",
      "supplement/tables/table_s2b_permutation_one_sided.png",
      "supplement/tables/table_s5_primary_contrasts_full_and_restricted.csv",
      "supplement/tables/table_s5_primary_contrasts_full_and_restricted.png",
      "supplement/tables/table_s6_item_endorsement_rates.csv",
      "supplement/tables/table_s6_item_endorsement_rates.png",
      "reference_analysis_style/tables/condition_descriptive_table_reference_style.csv",
      "reference_analysis_style/tables/condition_descriptive_table_reference_style.png",
      "reference_analysis_style/figures/permutation_null_two_tailed_reference_style.png",
      "reference_analysis_style/figures/permutation_null_one_tailed_reference_style.png"
    )
  ),
  .remove_generated_file,
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
    save_table_png_fallback(df, png_path, caption = NULL)
  }
  invisible(png_path)
}

.read_table_body_csv <- function(source_path) {
  lines <- readLines(source_path, warn = FALSE, encoding = "UTF-8")
  note_start <- grep("^# --- Notes ---", lines)
  if (length(note_start) > 0) {
    lines <- lines[seq_len(note_start[1] - 1L)]
  }
  while (length(lines) > 0 && !nzchar(trimws(utils::tail(lines, 1)))) {
    lines <- utils::head(lines, -1L)
  }
  if (length(lines) == 0) {
    return(data.frame())
  }
  utils::read.csv(text = paste(lines, collapse = "\n"),
                  check.names = FALSE, stringsAsFactors = FALSE)
}

.copy_selected_table <- function(source_csv_rel, target_csv_rel, target_png_rel,
                                 description, role, label,
                                 required = TRUE) {
  source_path <- file.path(.out_root, source_csv_rel)
  csv_path <- file.path(.sel_root, target_csv_rel)
  png_path <- file.path(.sel_root, target_png_rel)
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(source_path)) {
    file.copy(source_path, csv_path, overwrite = TRUE)
    .body <- .read_table_body_csv(source_path)
    .write_table_png(.body, png_path, caption = NULL)
  } else {
    log_warn("Missing manuscript-selected source: ", source_path)
  }

  .add_record(target_csv_rel, source_csv_rel, description, role, label, required)
  .add_record(target_png_rel, source_csv_rel, description, role, label, required)
  invisible(csv_path)
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
  if (is.na(p)) "" else if (p < .001) "< .001" else sprintf("%.3f", p)
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

.n_power_col_selected <- paste0("N for ", .power$target_power_label, " Power")
.power_table <- data.frame(
  `Target difference (pp)` = paste0(.effect_pct, " pp"),
  `Target effect (rescaled 0-10 score units)` = round(
    .power$table[["Target effect (rescaled score units)"]], 2
  ),
  `Cohen's dz` = round(.power$table[["Cohen dz"]], 3),
  `Power at observed N` = scales::percent(
    .power$table[["Power at observed N"]],
    accuracy = 0.1
  ),
  n_for_target_power = .actual_n,
  check.names = FALSE
)
names(.power_table)[names(.power_table) == "n_for_target_power"] <- .n_power_col_selected

.supp_desc <- .ref_analysis$condition_descriptives
.supp_desc_tbl <- data.frame(
  Condition = as.character(.supp_desc$condition),
  N = .supp_desc$n,
  `Mean rescaled 0-10 score (SD)` = sprintf("%.2f (%.2f)",
                                            .supp_desc$mean,
                                            .supp_desc$sd),
  `Median rescaled 0-10 score [IQR]` = sprintf("%.2f [%.2f, %.2f]",
                                               .supp_desc$median,
                                               .supp_desc$iqr_low,
                                               .supp_desc$iqr_high),
  `Range (rescaled 0-10 score)` = sprintf("%.2f-%.2f",
                                          .supp_desc$min,
                                          .supp_desc$max),
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
.perm_two_p <- .sign$table$p[grepl("permutation", .sign$table$Test,
                                   ignore.case = TRUE)][1]

.condition_row <- .log_tbl[.log_tbl$Term == "AI-assisted vs No-AI", , drop = FALSE][1, ]
.period_row <- .log_tbl[.log_tbl$Term == "Period 2 vs Period 1", , drop = FALSE][1, ]

.sign_total_n <- nrow(.sign$paired_differences)
.sign_non_tied_n <- sum(.sign$paired_differences$paired_diff != 0,
                        na.rm = TRUE)
.sign_ties <- .sign_total_n - .sign_non_tied_n
.perm_assignments_label <- paste0(
  "2^", .sign_total_n, " = ",
  format(2^.sign_total_n, big.mark = ",", scientific = FALSE)
)
.exact_sign_p <- .sign$table$p[.sign$table$Test == "Exact sign test"][1]
.exact_sign_stat <- .sign$table$Statistic[.sign$table$Test == "Exact sign test"][1]

.exact_test_tbl <- data.frame(
  Test = c(
    "Exact sign test",
    "Fisher-Pitman permutation test (two-sided)",
    "Fisher-Pitman permutation test (exploratory one-sided)"
  ),
  `Difference definition` = c(
    "AI-assisted minus No-AI",
    "AI-assisted minus No-AI",
    "AI-assisted greater than No-AI"
  ),
  N = rep(.sign_total_n, 3),
  `Non-tied n` = c(.sign_non_tied_n, "", ""),
  Statistic = c(
    .exact_sign_stat,
    sprintf("Observed mean difference = %.4f", .obs_diff),
    sprintf("Observed mean difference = %.4f", .obs_diff)
  ),
  p = c(.fmt_p(.exact_sign_p), .fmt_p(.perm_two_p), .fmt_p(.p_one)),
  Notes = c(
    paste0(.sign_ties,
           " ties are excluded from the exact binomial calculation."),
    paste0("Exact sign-flip enumeration over ",
           .perm_assignments_label, " assignments."),
    paste0("Exploratory one-sided exact sign-flip enumeration over ",
           .perm_assignments_label, " assignments.")
  ),
  check.names = FALSE
)

.clean_logistic <- .log_tbl |>
  dplyr::filter(.data$Term != "Intercept") |>
  dplyr::mutate(
    Model = dplyr::case_when(
      grepl("condition", .data$Model, ignore.case = TRUE) ~ "Condition model",
      grepl("period", .data$Model, ignore.case = TRUE) ~ "Period model",
      TRUE ~ "Randomized-sequence model"
    ),
    Contrast = .data$Term,
    `Odds ratio (95% CI)` = sprintf("%.3f [%.3f, %.3f]",
                                    .data$OR,
                                    .data[["OR CI low"]],
                                    .data[["OR CI high"]]),
    z = .fmt3(.data$z),
    p = vapply(.data$p, .fmt_p, character(1)),
    Interpretation = gsub("sequence-order", "randomized-sequence",
                          .data$Interpretation, ignore.case = TRUE)
  ) |>
  dplyr::select(
    "Model", "Contrast", "Odds ratio (95% CI)", "z", "p", "Interpretation"
  )

.main_table2 <- .clean_logistic

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
  Contrast = "AI-assisted minus No-AI",
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
  `Randomized sequence` = .seq_desc$sequence_display,
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

.sign_tbl <- .exact_test_tbl
.glmm_compact_tbl <- .clean_logistic

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
  ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
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
    x = "AI-assisted - No-AI",
    y = "Participants",
    title = NULL,
    subtitle = NULL,
    caption = NULL
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
    .perm_two_p
  } else {
    .p_one
  }
  p <- ggplot2::ggplot(
    null_df,
    ggplot2::aes(x = .data$mean_difference, y = .data$n, fill = .data$extreme)
  ) +
    ggplot2::geom_col(width = bar_width, colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "firebrick"),
                               guide = "none") +
    ggplot2::geom_vline(xintercept = obs_prop, colour = "#2E8B57",
                        linewidth = 0.9) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste0("p = ", .fmt_p(p_value)),
      hjust = 1.08,
      vjust = 1.35,
      colour = "firebrick",
      size = 4
    ) +
    ggplot2::annotate(
      "text",
      x = obs_prop,
      y = Inf,
      label = "Observed mean difference",
      hjust = ifelse(obs_prop >= 0, 1.05, -0.05),
      vjust = 2.6,
      colour = "#2E8B57",
      size = 3.4
    ) +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(
      x = "Null mean difference (AI-assisted - No-AI, proportion correct)",
      y = "Null distribution count",
      title = NULL,
      subtitle = NULL,
      caption = NULL
    ) +
    .reference_theme()

  if (alternative == "two.sided") {
    p <- p + ggplot2::geom_vline(xintercept = -obs_prop, colour = "firebrick",
                                 linetype = "dotted", linewidth = 0.8)
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
.slope_diff_ci <- as.numeric(c(.paired[["95% CI low"]][1],
                               .paired[["95% CI high"]][1])) / .scale_to * 100
.slope_wedge <- data.frame(
  x_pos = c(1, 2, 2),
  score = c(
    .slope_noai_mean,
    .slope_noai_mean + .slope_diff_ci[1],
    .slope_noai_mean + .slope_diff_ci[2]
  )
)
.slope_labels_left <- dplyr::filter(.slope_df, .data$x_pos == 1)
.slope_labels_right <- dplyr::filter(.slope_df, .data$x_pos == 2)

.slope_plot <- ggplot2::ggplot(.slope_df, ggplot2::aes(x = .data$x_pos, y = .data$score)) +
  ggplot2::geom_hline(yintercept = .slope_noai_mean, linetype = "dotted",
                      colour = "grey45", linewidth = 0.5) +
  ggplot2::geom_polygon(
    data = .slope_wedge,
    ggplot2::aes(x = .data$x_pos, y = .data$score),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.35
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = .data$participant, colour = .data$sequence_display),
    linewidth = 0.45,
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(
    data = .slope_labels_left,
    ggplot2::aes(label = .data$test, colour = .data$sequence_display),
    nudge_x = -0.16,
    direction = "y",
    xlim = c(0.76, 0.88),
    hjust = 1,
    size = 2.9,
    fontface = "bold",
    alpha = 0.75,
    box.padding = 0.12,
    point.padding = 0.05,
    min.segment.length = 0,
    segment.size = 0.22,
    segment.alpha = 0.35,
    force = 0.9,
    force_pull = 0.08,
    seed = 20260629,
    max.overlaps = Inf,
    na.rm = TRUE,
    show.legend = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = .slope_labels_right,
    ggplot2::aes(label = .data$test, colour = .data$sequence_display),
    nudge_x = 0.16,
    direction = "y",
    xlim = c(2.12, 2.24),
    hjust = 0,
    size = 2.9,
    fontface = "bold",
    alpha = 0.75,
    box.padding = 0.12,
    point.padding = 0.05,
    min.segment.length = 0,
    segment.size = 0.22,
    segment.alpha = 0.35,
    force = 0.9,
    force_pull = 0.08,
    seed = 20260630,
    max.overlaps = Inf,
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
  ggplot2::scale_colour_manual(values = .seq_colors,
                               name = "Randomized sequence") +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2),
    labels = c("No-AI", "AI-assisted")
  ) +
  ggplot2::scale_y_continuous(
    "Score (%)",
    breaks = seq(0, 100, 10),
    labels = function(y) paste0(y, "%"),
    expand = ggplot2::expansion(mult = c(0.03, 0.08))
  ) +
  ggplot2::coord_cartesian(xlim = c(0.72, 2.28), ylim = .slope_ylim,
                           clip = "off") +
  ggplot2::labs(
    x = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  .reference_theme()

# ---------------------------------------------------------------------------
# Main body.
# ---------------------------------------------------------------------------
.item_fig_source <- "figures/exploratory/item_endorsement_by_sequence.png"
.item_fig_main_rel <- "figures/main/item_endorsement_by_sequence.png"
.item_fig_main_path <- file.path(.out_root, .item_fig_main_rel)
.item_fig_exploratory_path <- file.path(.out_root, .item_fig_source)
if (file.exists(.item_fig_exploratory_path)) {
  dir.create(dirname(.item_fig_main_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(.item_fig_exploratory_path, .item_fig_main_path, overwrite = TRUE)
  .item_fig_source <- .item_fig_main_rel
}

.copy_selected(
  "figures/primary/ai_assisted_vs_noai_paired.png",
  "main_body/figures/figure1_paired_score_plot.png",
  "Participant-level paired rescaled 0-10 scores under No-AI and AI-assisted study; the wedge shows uncertainty in the mean paired difference.",
  "Main", "Figure 1"
)
.copy_selected_table(
  "tables/descriptive/02b_condition_descriptives_restricted.csv",
  "main_body/tables/table1a_score_descriptive_summary.csv",
  "main_body/tables/table1a_score_descriptive_summary.png",
  "Descriptive summary of restricted rescaled 0-10 scores by study condition.",
  "Main", "Table 1A"
)
.copy_selected_table(
  "tables/primary/08b_paired_difference_effect_size.csv",
  "main_body/tables/table1b_primary_paired_contrast.csv",
  "main_body/tables/table1b_primary_paired_contrast.png",
  "AI-assisted minus No-AI paired contrast on the rescaled 0-10 metric.",
  "Main", "Table 1B"
)
.copy_selected(
  .item_fig_source,
  "main_body/figures/figure2_item_endorsement_by_sequence.png",
  "Item-level percent correct by item and post-test form, stratified by randomized sequence; Form Y items 1 and 6 were excluded from final restricted scoring.",
  "Main", "Figure 2"
)
.copy_selected(
  "figures/supplementary/post_hoc_power_curve.png",
  "main_body/figures/figure3_post_hoc_power_curve.png",
  "Post hoc paired-sample power curve for target effects; emphasizes that the observed sample was underpowered for small effects and future paired studies need larger sample sizes.",
  "Main", "Figure 3"
)
.write_selected_table(
  .main_table2,
  "main_body/tables/table2_supporting_analysis_summary.csv",
  "main_body/tables/table2_supporting_analysis_summary.png",
  "Main Table 2. Item-level logistic mixed-model summary",
  "Manuscript-facing GLMM summary using odds ratios and OR-scale 95% CIs only.",
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
  "Post hoc paired-sample power curve by target effect size.",
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

.copy_selected(
  "figures/supplementary/condition_score_histogram_restricted.png",
  "supplement/figures/figure_s3_condition_score_histogram_restricted.png",
  "Restricted rescaled 0-10 score distributions by study condition.",
  "Supplement", "Figure S3"
)
.copy_selected(
  "figures/supplementary/sequence_difference_histogram_restricted.png",
  "supplement/figures/figure_s4_sequence_difference_histogram_restricted.png",
  "Paired score differences by randomized sequence.",
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
.save_selected_plot(
  .plot_null_distribution("two.sided"),
  "supplement/figures/figure_s7_permutation_null_two_sided.png",
  "Fisher-Pitman permutation-test null distribution for the two-sided paired test with the observed mean difference marked.",
  "Supplement", "Figure S7",
  source_rel = "rds/analysis_results.rds",
  width = 7.5, height = 5
)
.save_selected_plot(
  .plot_null_distribution("greater"),
  "supplement/figures/figure_s8_permutation_null_one_sided.png",
  "Fisher-Pitman permutation-test null distribution for the exploratory one-sided paired test with the observed mean difference marked.",
  "Supplement", "Figure S8",
  source_rel = "rds/analysis_results.rds",
  width = 7.5, height = 5
)

.write_selected_table(
  .exact_test_tbl,
  "supplement/tables/table_s2_sign_permutation_tests.csv",
  "supplement/tables/table_s2_sign_permutation_tests.png",
  "Table S2. Exact sign and Fisher-Pitman permutation tests",
  "Exact sign test plus two-sided and exploratory one-sided Fisher-Pitman permutation tests.",
  "Supplement", "Table S2",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .clean_logistic,
  "supplement/tables/table_s3_logistic_mixed_model_results.csv",
  "supplement/tables/table_s3_logistic_mixed_model_results.png",
  "Table S3. Clean logistic mixed model summary",
  "Clean item-level logistic mixed model summaries using odds ratios and OR-scale 95% CIs.",
  "Supplement", "Table S3",
  "rds/analysis_results.rds"
)
.copy_selected_table(
  "tables/descriptive/02_descriptive_statistics.csv",
  "supplement/tables/table_s4_full_descriptive_statistics.csv",
  "supplement/tables/table_s4_full_descriptive_statistics.png",
  "Full descriptive rescaled 0-10 score summaries by condition, period, and form.",
  "Supplement", "Table S4"
)
.write_selected_table(
  .supp_desc_tbl,
  "supplement/tables/table_s5_descriptive_scores_rescaled_0_10.csv",
  "supplement/tables/table_s5_descriptive_scores_rescaled_0_10.png",
  "Table S5. Supplemental descriptive score summary on the rescaled 0-10 metric",
  "Supplemental descriptive score summary with headers explicitly identifying the rescaled 0-10 metric.",
  "Supplement", "Table S5",
  "rds/analysis_results.rds"
)
.copy_selected_table(
  "tables/primary/03_primary_contrasts.csv",
  "supplement/tables/table_s6_primary_contrasts_full_and_restricted.csv",
  "supplement/tables/table_s6_primary_contrasts_full_and_restricted.png",
  "Full and restricted paired contrasts for condition and period effects on the rescaled 0-10 metric.",
  "Supplement", "Table S6"
)
.copy_selected_table(
  "tables/exploratory/19_item_endorsement_rates.csv",
  "supplement/tables/table_s7_item_endorsement_rates.csv",
  "supplement/tables/table_s7_item_endorsement_rates.png",
  "Item-level endorsement rates by post-test form and randomized sequence.",
  "Supplement", "Table S7"
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
  "reference_analysis_style/tables/condition_percent_descriptive_table_reference_style.csv",
  "reference_analysis_style/tables/condition_percent_descriptive_table_reference_style.png",
  NULL,
  "Reference-style condition descriptive table using percent correct; Table S5 is the separate rescaled 0-10 manuscript table.",
  .ref_role, "Condition percent descriptive table",
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
  "Reference sequence table. Scores and paired differences use the rescaled 0-10 metric",
  "Sequence difference table using randomized-sequence labels; scores and paired differences use the rescaled 0-10 metric.",
  .ref_role, "Sequence difference table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sign_tbl,
  "reference_analysis_style/tables/sign_test_table_reference_style.csv",
  "reference_analysis_style/tables/sign_test_table_reference_style.png",
  NULL,
  "Exact sign test plus two-sided and exploratory one-sided Fisher-Pitman permutation tests.",
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
  "Compact GLMM table using odds ratios and OR-scale 95% CIs.",
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
  "reference_analysis_style/figures/permutation_null_two_sided_reference_style.png",
  "Two-sided Fisher-Pitman permutation-test null distribution.",
  .ref_role, "Two-sided permutation null",
  width = 7.5, height = 5
)
.save_selected_plot(
  .plot_null_distribution("greater"),
  "reference_analysis_style/figures/permutation_null_one_sided_reference_style.png",
  "One-sided Fisher-Pitman permutation-test null distribution.",
  .ref_role, "One-sided permutation null",
  width = 7.5, height = 5
)
.save_selected_plot(
  .slope_plot,
  "reference_analysis_style/figures/paired_slope_plot_reference_style.png",
  "Participant-level paired slope plot with X/Y endpoint form labels and a paired-difference uncertainty wedge.",
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
  "- Visible labels use AI-assisted/No-AI terminology where appropriate.",
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
  "- `tables/primary/03_primary_contrasts.csv` - broader contrast table, copied to supplement as Table S6.",
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
log_line("Power curve : manuscript_selected/main_body/figures/figure3_post_hoc_power_curve.png")

if (exists("session_record_module", envir = .GlobalEnv)) {
  session_record_module("manuscript_selected", "OK", 0)
}



