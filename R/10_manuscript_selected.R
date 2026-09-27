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
      "reference_style_reference",
      "supplement/audit"
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
      "supplement/figures/figure_s2_post_hoc_power_curve.png",
      "supplement/figures/figure_s3_condition_score_histogram_restricted.png",
      "supplement/figures/figure_s4_sequence_difference_histogram_restricted.png",
      "supplement/figures/figure_s5_paired_difference_histogram.png",
      "supplement/figures/figure_s6_paired_difference_dotplot_restricted.png",
      "supplement/figures/figure_s7_permutation_null_two_sided.png",
      "supplement/figures/figure_s8_permutation_null_one_sided.png",
      "main_body/figures/figure2_item_endorsement_by_sequence.png",
      "main_body/figures/figure3_post_hoc_power_curve.png",
      "supplement/tables/table_s2b_permutation_one_tailed.csv",
      "supplement/tables/table_s2b_permutation_one_tailed.png",
      "supplement/tables/table_s2b_permutation_one_sided.csv",
      "supplement/tables/table_s2b_permutation_one_sided.png",
      "supplement/tables/table_s4_full_descriptive_statistics.csv",
      "supplement/tables/table_s4_full_descriptive_statistics.png",
      "supplement/tables/table_s5_primary_contrasts_full_and_restricted.csv",
      "supplement/tables/table_s5_primary_contrasts_full_and_restricted.png",
      "supplement/tables/table_s6_item_endorsement_rates.csv",
      "supplement/tables/table_s6_item_endorsement_rates.png",
      "supplement/tables/table_s6_primary_contrasts_full_and_restricted.csv",
      "supplement/tables/table_s6_primary_contrasts_full_and_restricted.png",
      "supplement/tables/table_s7_item_endorsement_rates.csv",
      "supplement/tables/table_s7_item_endorsement_rates.png",
      "supplement/tables/table_s5_descriptive_scores_rescaled_0_10.csv",
      "supplement/tables/table_s5_descriptive_scores_rescaled_0_10.png",
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

.manuscript_table_style <- function(table_rel, df) {
  cols <- names(df)
  equal_widths <- stats::setNames(rep(100 / max(length(cols), 1), length(cols)), cols)
  key <- tolower(basename(table_rel %||% ""))

  style <- list(
    widths = equal_widths,
    left_cols = intersect(
      cols,
      c("Condition", "Model", "Contrast", "Test", "Difference definition",
        "Randomized sequence", "Measure", "Measure_1")
    ),
    center_cols = character(0),
    right_cols = character(0),
    labels = stats::setNames(cols, cols),
    font_size = 15,
    vwidth = 1100,
    row_padding = 5,
    header_padding = 7,
    horizontal_padding = 10,
    divider_after = character(0)
  )

  if (grepl("table1a_score_descriptive_summary|table_s5_descriptive_scores", key)) {
    style$widths <- stats::setNames(c(24, 9, 22, 29, 16), cols)
    style$left_cols <- intersect(cols, "Condition")
    style$center_cols <- setdiff(cols, style$left_cols)
    style$divider_after <- intersect(cols, "Condition")
  } else if (grepl("table1b_primary_paired_contrast", key)) {
    style$widths <- stats::setNames(c(67, 33), cols)
    style$left_cols <- intersect(cols, "Statistic")
    style$right_cols <- intersect(cols, "Estimate")
    style$font_size <- 15
    style$vwidth <- 700
    style$row_padding <- 4
    style$header_padding <- 5
    style$horizontal_padding <- 18
    style$divider_after <- intersect(cols, "Statistic")
  } else if (grepl("table2_supporting_analysis_summary", key)) {
    style$widths <- stats::setNames(c(24, 36, 24, 8, 8), cols)
    style$left_cols <- intersect(cols, c("Model", "Contrast"))
    style$center_cols <- setdiff(cols, style$left_cols)
    style$divider_after <- intersect(cols, "Contrast")
  } else if (grepl("table_s1_post_hoc_power_analysis", key)) {
    style$widths <- stats::setNames(c(25, 29, 14, 16, 16), cols)
    style$left_cols <- character(0)
    style$center_cols <- cols
    style$divider_after <- intersect(cols, "Target difference (score units)")
  } else if (grepl("table_s2_sign_permutation_tests", key)) {
    style$widths <- stats::setNames(c(29, 25, 7, 10, 23, 6), cols)
    style$left_cols <- intersect(cols, c("Test", "Difference definition"))
    style$center_cols <- setdiff(cols, style$left_cols)
    style$font_size <- 14
    style$divider_after <- intersect(cols, "Difference definition")
  } else if (grepl("table_s3_logistic_mixed_model_results", key)) {
    style$widths <- stats::setNames(c(20, 28, 14, 8, 18, 6, 6), cols)
    style$left_cols <- intersect(cols, c("Model", "Contrast"))
    style$center_cols <- setdiff(cols, style$left_cols)
    style$font_size <- 14
    style$divider_after <- intersect(cols, "Contrast")
  } else if (grepl("table_s4_randomized_sequence_paired_differences", key)) {
    style$widths <- stats::setNames(c(25, 8, 22, 22, 23), cols)
    style$left_cols <- intersect(cols, "Randomized sequence")
    style$center_cols <- setdiff(cols, style$left_cols)
    style$divider_after <- intersect(cols, "Randomized sequence")
  } else {
    style$center_cols <- setdiff(cols, style$left_cols)
  }

  style$widths <- style$widths[names(style$widths) %in% cols]
  style$left_cols <- intersect(style$left_cols, cols)
  style$center_cols <- intersect(style$center_cols, cols)
  style$right_cols <- intersect(style$right_cols, cols)
  style$labels <- style$labels[names(style$labels) %in% cols]
  style$divider_after <- intersect(style$divider_after, utils::head(cols, -1L))
  style
}

.write_table_png <- function(df, png_path, caption = NULL, notes = NULL,
                             table_rel = NULL) {
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  .ok <- FALSE

  if (requireNamespace("gt", quietly = TRUE)) {
    .ok <- tryCatch({
      ensure_gt_png_export()
      display_names <- names(df)
      gt_df <- df
      if (anyDuplicated(names(gt_df))) {
        names(gt_df) <- make.unique(names(gt_df), sep = "_")
      }
      style <- .manuscript_table_style(table_rel %||% png_path, gt_df)
      style$labels <- stats::setNames(display_names, names(gt_df))

      gt_tbl <- gt::gt(gt_df)
      if (length(style$labels) > 0) {
        gt_tbl <- gt::cols_label(gt_tbl, .list = as.list(style$labels))
      }
      if (length(style$widths) > 0) {
        width_formulas <- lapply(names(style$widths), function(col) {
          rlang::new_formula(rlang::sym(col), gt::pct(style$widths[[col]]))
        })
        gt_tbl <- gt::cols_width(gt_tbl, .list = width_formulas)
      }
      if (length(style$left_cols) > 0) {
        gt_tbl <- gt::cols_align(
          gt_tbl, align = "left", columns = dplyr::all_of(style$left_cols)
        )
      }
      if (length(style$center_cols) > 0) {
        gt_tbl <- gt::cols_align(
          gt_tbl, align = "center", columns = dplyr::all_of(style$center_cols)
        )
      }
      if (length(style$right_cols) > 0) {
        gt_tbl <- gt::cols_align(
          gt_tbl, align = "right", columns = dplyr::all_of(style$right_cols)
        )
      }
      gt_tbl <- gt_tbl |>
        gt::tab_options(
          table.width = gt::pct(100),
          table.layout = "fixed",
          table.align = "left",
          container.width = gt::px(style$vwidth),
          container.padding.x = gt::px(0),
          container.padding.y = gt::px(0),
          table.background.color = "white",
          table.font.size = style$font_size,
          heading.title.font.size = 13,
          heading.subtitle.font.size = 11,
          column_labels.font.weight = "bold",
          column_labels.background.color = "white",
          column_labels.padding = gt::px(style$header_padding),
          column_labels.padding.horizontal = gt::px(style$horizontal_padding),
          column_labels.vlines.style = "none",
          column_labels.border.top.style = "none",
          column_labels.border.bottom.style = "solid",
          column_labels.border.bottom.width = gt::px(1.3),
          column_labels.border.bottom.color = "#4D4D4D",
          table.border.top.style = "solid",
          table.border.top.width = gt::px(1.5),
          table.border.top.color = "#4D4D4D",
          table.border.bottom.style = "solid",
          table.border.bottom.width = gt::px(1.5),
          table.border.bottom.color = "#4D4D4D",
          table.border.left.style = "none",
          table.border.right.style = "none",
          table_body.hlines.style = "solid",
          table_body.hlines.width = gt::px(0.7),
          table_body.hlines.color = "#D8D8D8",
          table_body.vlines.style = "none",
          table_body.border.top.style = "none",
          table_body.border.bottom.style = "none",
          data_row.padding = gt::px(style$row_padding),
          data_row.padding.horizontal = gt::px(style$horizontal_padding),
          source_notes.font.size = style$font_size - 1,
          source_notes.padding = gt::px(7),
          source_notes.border.bottom.style = "none"
        ) |>
        gt::tab_style(
          style = gt::cell_text(align = "center"),
          locations = gt::cells_column_labels(columns = dplyr::everything())
        )
      if (length(style$divider_after) > 0) {
        divider_col <- style$divider_after[1]
        divider_rule <- gt::cell_borders(
          sides = "right",
          color = "#E6E6E6",
          style = "solid",
          weight = gt::px(0.6)
        )
        gt_tbl <- gt_tbl |>
          gt::tab_style(
            style = divider_rule,
            locations = gt::cells_column_labels(columns = dplyr::all_of(divider_col))
          ) |>
          gt::tab_style(
            style = divider_rule,
            locations = gt::cells_body(columns = dplyr::all_of(divider_col))
          )
      }
      if (!is.null(notes) && length(notes) > 0) {
        for (.note in notes) {
          gt_tbl <- gt_tbl |> gt::tab_source_note(gt::md(.note))
        }
      }
      .saved <- FALSE
      .save_error <- NULL
      for (.attempt in seq_len(2L)) {
        .saved <- tryCatch({
          gt::gtsave(gt_tbl, png_path, vwidth = style$vwidth, expand = 5)
          TRUE
        }, error = function(e) {
          .save_error <<- e
          FALSE
        })
        if (.saved) break
        Sys.sleep(0.75)
      }
      if (!.saved) stop(.save_error)
      TRUE
    }, error = function(e) {
      log_warn("gt PNG export failed for selected table: ", conditionMessage(e))
      FALSE
    })
  }

  if (!.ok) {
    save_table_png_fallback(df, png_path, caption = NULL, notes = notes)
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
    .write_table_png(.body, png_path, caption = NULL, table_rel = target_png_rel)
  } else {
    log_warn("Missing manuscript-selected source: ", source_path)
  }

  .add_record(target_csv_rel, source_csv_rel, description, role, label, required)
  .add_record(target_png_rel, source_csv_rel, description, role, label, required)
  invisible(csv_path)
}

.write_selected_table <- function(df, csv_rel, png_rel, caption, description,
                                  role, label, source_rel,
                                  required = TRUE, notes = NULL) {
  csv_path <- file.path(.sel_root, csv_rel)
  png_path <- file.path(.sel_root, png_rel)
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, csv_path, row.names = FALSE, na = "")
  .write_table_png(df, png_path, caption = caption, notes = notes,
                   table_rel = png_rel)
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
.scale_to_label <- format(.scale_to, trim = TRUE, scientific = FALSE)
.score_metric_short <- paste0("rescaled 0-", .scale_to_label, " score")
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
# Post hoc power table, regenerated from canonical analysis results.
# No study-specific N values or target-power level are hard-coded here.
# ---------------------------------------------------------------------------
.power <- results$post_hoc_power
if (is.null(.power) || is.null(.power$table)) {
  stop("Post hoc power results are unavailable. Run R/04_analyses.R first.")
}

.effect_pct <- as.integer(.power$target_effects_pct)
.actual_n <- as.integer(.power$table$n_for_target_power)
if (length(.effect_pct) != nrow(.power$table) ||
    length(.actual_n) != nrow(.power$table) ||
    any(!is.finite(.effect_pct)) || any(!is.finite(.actual_n))) {
  stop(
    "Post hoc power results are incomplete or inconsistent with their table."
  )
}

.n_power_col_selected <- paste0("N for ", .power$target_power_label, " power")
.observed_power_col_selected <- paste0(
  "Power at N = ", as.integer(.power$n_pairs %||% 15L)
)
.power_table <- data.frame(
  `Target difference (pp)` = paste0(.effect_pct, " pp"),
  `Target difference (score units)` = round(
    .power$table[["Target effect (rescaled score units)"]], 2
  ),
  `Cohen's dz` = round(.power$table[["Cohen dz"]], 3),
  observed_power = scales::percent(
    .power$table[["Power at observed N"]],
    accuracy = 0.1
  ),
  n_for_target_power = .actual_n,
  check.names = FALSE
)
names(.power_table)[names(.power_table) == "observed_power"] <-
  .observed_power_col_selected
names(.power_table)[names(.power_table) == "n_for_target_power"] <- .n_power_col_selected

.supp_desc <- .ref_analysis$condition_descriptives
.supp_desc_tbl <- data.frame(
  Condition = as.character(.supp_desc$condition),
  N = .supp_desc$n,
  `Mean (SD)` = sprintf("%.2f (%.2f)", .supp_desc$mean, .supp_desc$sd),
  `Median [IQR]` = sprintf("%.2f [%.2f, %.2f]",
                           .supp_desc$median,
                           .supp_desc$iqr_low,
                           .supp_desc$iqr_high),
  Range = sprintf("%.2f-%.2f", .supp_desc$min, .supp_desc$max),
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
    "Fisher-Pitman, two-sided",
    "Fisher-Pitman, exploratory one-sided"
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
  check.names = FALSE
)

.log_non_intercept <- .log_tbl |>
  dplyr::filter(.data$Term != "Intercept") |>
  dplyr::mutate(
    Model = dplyr::case_when(
      grepl("condition", .data$Model, ignore.case = TRUE) ~ "Condition model",
      grepl("period", .data$Model, ignore.case = TRUE) ~ "Period model",
      TRUE ~ "Sequence model"
    )
  )

.glmm_or_tbl <- data.frame(
  Model = .log_non_intercept$Model,
  Contrast = .log_non_intercept$Term,
  `Odds ratio (95% CI)` = sprintf(
    "%.3f [%.3f, %.3f]",
    .log_non_intercept$OR,
    .log_non_intercept[["OR CI low"]],
    .log_non_intercept[["OR CI high"]]
  ),
  z = .fmt3(.log_non_intercept$z),
  p = vapply(.log_non_intercept$p, .fmt_p, character(1)),
  Interpretation = gsub(
    "sequence-order", "randomized-sequence",
    .log_non_intercept$Interpretation,
    ignore.case = TRUE
  ),
  check.names = FALSE
)

.main_table2 <- .glmm_or_tbl |>
  dplyr::select("Model", "Contrast", "Odds ratio (95% CI)", "z", "p")

.log_est <- .log_non_intercept[["Log-odds estimate"]]
.log_se <- .log_non_intercept$SE
.supp_glmm_log_odds_tbl <- data.frame(
  Model = .log_non_intercept$Model,
  Contrast = .log_non_intercept$Term,
  `Log-odds estimate` = .fmt3(.log_est),
  SE = .fmt3(.log_se),
  `95% CI for log odds` = sprintf(
    "[%.3f, %.3f]",
    .log_est - stats::qnorm(0.975) * .log_se,
    .log_est + stats::qnorm(0.975) * .log_se
  ),
  z = .fmt3(.log_non_intercept$z),
  p = vapply(.log_non_intercept$p, .fmt_p, character(1)),
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

.main_table1b <- data.frame(
  Statistic = c(
    "Paired difference (95% CI), pp",
    "Cohen's dz",
    "Hedges' gz",
    sprintf("t(%s)", .paired$df),
    "p"
  ),
  Estimate = c(
    sprintf(
      "%.1f [%.1f, %.1f]",
      .to_pct(.paired[["Mean paired difference"]]),
      .to_pct(.paired[["95% CI low"]]),
      .to_pct(.paired[["95% CI high"]])
    ),
    sprintf("%.3f", .paired[["Cohen dz"]]),
    sprintf("%.3f", .paired[["Hedges gz"]]),
    sprintf("%.3f", .paired$t),
    vapply(.paired$p, .fmt_p, character(1))
  ),
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
  `Paired difference, mean (SD)` = sprintf(
    "%.3f (%.3f)",
    .seq_desc[["Mean paired difference"]],
    .seq_desc[["SD paired difference"]]
  ),
  check.names = FALSE
)

.sign_tbl <- .exact_test_tbl
.glmm_compact_tbl <- .glmm_or_tbl

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
.signed_number <- function(x, accuracy = 1) {
  paste0(ifelse(x > 0, "+", ""), scales::number(x, accuracy = accuracy))
}
.signed_pp <- function(x, accuracy = 1) {
  paste0(.signed_number(x, accuracy = accuracy), " pp")
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
    breaks = seq(50, 100, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  ggplot2::scale_y_continuous(
    "Relative frequency",
    labels = scales::number_format(accuracy = 0.01)
  ) +
  ggplot2::coord_cartesian(xlim = c(50, 100)) +
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
    label = paste0("Mean = ", .signed_pp(.mean_diff_pct, accuracy = 0.1)),
    colour = "firebrick",
    hjust = -0.05,
    vjust = 1.4,
    size = 4
  ) +
  ggplot2::scale_x_continuous(labels = .signed_number) +
  ggplot2::labs(
    x = "AI-assisted - No-AI (percentage points)",
    y = "Participants",
    title = NULL,
    subtitle = NULL,
    caption = NULL
  ) +
  .reference_theme()

.plot_null_distribution <- function(alternative = c("two.sided", "greater")) {
  alternative <- match.arg(alternative)
  perm_pp <- .perm_dist / .scale_to * 100
  obs_pp <- .obs_diff / .scale_to * 100
  vals <- sort(unique(round(perm_pp, 10)))
  step <- diff(vals)
  bar_width <- if (length(step)) min(step[step > 0], na.rm = TRUE) * 0.9 else 1
  null_df <- as.data.frame(table(round(perm_pp, 10)), stringsAsFactors = FALSE)
  names(null_df) <- c("mean_difference", "n")
  null_df$mean_difference <- as.numeric(null_df$mean_difference)
  null_df$extreme <- if (alternative == "two.sided") {
    abs(null_df$mean_difference) >= abs(obs_pp) - sqrt(.Machine$double.eps)
  } else {
    null_df$mean_difference >= obs_pp - sqrt(.Machine$double.eps)
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
    ggplot2::geom_vline(xintercept = obs_pp, colour = "#2E8B57",
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
      x = obs_pp,
      y = Inf,
      label = paste0("Observed = ", .signed_pp(obs_pp, accuracy = 0.01)),
      hjust = ifelse(obs_pp >= 0, 1.05, -0.05),
      vjust = 2.6,
      colour = "#2E8B57",
      size = 3.4
    ) +
    ggplot2::scale_x_continuous(labels = .signed_number) +
    ggplot2::labs(
      x = "Null mean difference (AI-assisted - No-AI, percentage points)",
      y = "Null distribution count",
      title = NULL,
      subtitle = NULL,
      caption = NULL
    ) +
    .reference_theme()

  if (alternative == "two.sided") {
    p <- p + ggplot2::geom_vline(xintercept = -obs_pp, colour = "firebrick",
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

.perm_null_composite <- {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The patchwork package is required to build Figure S4.")
  }
  .p_two <- .plot_null_distribution("two.sided") +
    ggplot2::labs(title = "Two-sided") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0))
  .plot_one <- .plot_null_distribution("greater") +
    ggplot2::labs(title = "Exploratory one-sided") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0))
  patchwork::wrap_plots(.p_two, .plot_one, ncol = 1)
}

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
  paste0("Participant-level paired ", .score_metric_short,
         "s under No-AI and AI-assisted study; the wedge shows uncertainty in the mean paired difference."),
  "Main", "Figure 1"
)
.write_selected_table(
  .supp_desc_tbl,
  "main_body/tables/table1a_score_descriptive_summary.csv",
  "main_body/tables/table1a_score_descriptive_summary.png",
  NULL,
  paste0("Descriptive summary of restricted ", .score_metric_short,
         "s by study condition."),
  "Main", "Table 1A",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .main_table1b,
  "main_body/tables/table1b_primary_paired_contrast.csv",
  "main_body/tables/table1b_primary_paired_contrast.png",
  NULL,
  paste0("AI-assisted minus No-AI paired contrast on the ",
         .score_metric_short, " metric."),
  "Main", "Table 1B",
  "rds/analysis_results.rds"
)
.copy_selected(
  "figures/supplementary/post_hoc_power_curve.png",
  "main_body/figures/figure2_power_curve.png",
  "Post hoc paired-sample power curve for target effects; emphasizes that the observed sample was underpowered for small effects and future paired studies need larger sample sizes.",
  "Main", "Figure 2"
)
.copy_selected(
  .item_fig_source,
  "main_body/figures/figure3_item_endorsement_by_sequence.png",
  "Item-level percent correct by item and post-test form, stratified by randomized sequence; Form Y items 1 and 6 were excluded from final restricted scoring.",
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
.generate_flow_selected <- isTRUE(cfg$flow_diagram$generate %||% FALSE)
if (.generate_flow_selected) {
  .copy_selected(
    "figures/supplementary/figure_s1_participant_flow.png",
    "supplement/figures/figure_s1_participant_flow.png",
    "Participant allocation and 2 x 2 crossover counterbalancing schematic.",
    "Supplement", "Figure S1"
  )
} else {
  .remove_generated_file(
    file.path(.sel_root, "supplement/figures/figure_s1_participant_flow.png"),
    .sel_root
  )
}
.write_selected_table(
  .power_table,
  "supplement/tables/table_s1_post_hoc_power_analysis.csv",
  "supplement/tables/table_s1_post_hoc_power_analysis.png",
  "Table S1. Post hoc paired-sample power analysis by target effect size",
  "Post hoc paired-sample power analysis by target effect size.",
  "Supplement", "Table S1",
  "tables/supplementary/20_post_hoc_power_analysis.csv"
)

.save_selected_plot(
  .condition_hist_plot,
  "supplement/figures/figure_s2_score_distributions_by_condition.png",
  "Restricted score distributions by study condition.",
  "Supplement", "Figure S2",
  source_rel = "rds/analysis_results.rds",
  width = 6.5, height = 4.5
)
.save_selected_plot(
  .paired_diff_plot,
  "supplement/figures/figure_s3_paired_condition_difference_distribution.png",
  "Participant-level paired-condition-difference distribution.",
  "Supplement", "Figure S3",
  source_rel = "rds/analysis_results.rds",
  width = 6.5, height = 4.5
)
.save_selected_plot(
  .perm_null_composite,
  "supplement/figures/figure_s4_fisher_pitman_null_distributions.png",
  "Two-sided and exploratory one-sided Fisher-Pitman permutation null distributions.",
  "Supplement", "Figure S4",
  source_rel = "rds/analysis_results.rds",
  width = 7.5, height = 7.5
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
  .supp_glmm_log_odds_tbl,
  "supplement/tables/table_s3_logistic_mixed_model_results.csv",
  "supplement/tables/table_s3_logistic_mixed_model_results.png",
  "Table S3. Detailed logistic mixed-model coefficients on the log-odds scale",
  "Detailed GLMM coefficients on the log-odds scale; the null value is 0.",
  "Supplement", "Table S3",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sequence_desc_tbl,
  "supplement/tables/table_s4_randomized_sequence_paired_differences.csv",
  "supplement/tables/table_s4_randomized_sequence_paired_differences.png",
  "Table S4. Randomized-sequence-specific paired condition differences",
  paste0("Randomized-sequence-specific paired condition differences on the ",
         .score_metric_short, " metric, with mean and SD combined for compact display."),
  "Supplement", "Table S4",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .supp_desc_tbl,
  "supplement/tables/table_s5_descriptive_scores.csv",
  "supplement/tables/table_s5_descriptive_scores.png",
  paste0("Table S5. Supplemental descriptive score summary on the ",
         .score_metric_short, " metric"),
  paste0("Supplemental descriptive score summary on the ",
         .score_metric_short, " metric with compact headers."),
  "Supplement", "Table S5",
  "rds/analysis_results.rds"
)

# ---------------------------------------------------------------------------
# Reference output set.
# ---------------------------------------------------------------------------
.write_selected_table(
  .condition_desc_tbl,
  "reference_analysis_style/tables/condition_percent_descriptive_table_reference_style.csv",
  "reference_analysis_style/tables/condition_percent_descriptive_table_reference_style.png",
  NULL,
  paste0("Reference-style condition descriptive table using percent correct; Table S5 is the separate ",
         .score_metric_short, " manuscript table."),
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
  paste0("Reference sequence table. Scores and paired differences use the ",
         .score_metric_short, " metric"),
  paste0("Sequence difference table using randomized-sequence labels; scores and paired differences use the ",
         .score_metric_short, " metric."),
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
  paste0("Post hoc power table using the configured ",
         .power$target_power_label, " target."),
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
  "Models use the configured item exclusions and display labels.",
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
  "Selected outputs copied or regenerated from canonical pipeline outputs. Original outputs remain in their original folders.",
  "",
  "## Folder Layout",
  "",
  "- `main_body/figures/`",
  "- `main_body/tables/`",
  "- `supplement/figures/`",
  "- `supplement/tables/`",
  "- `reference_analysis_style/figures/`",
  "- `reference_analysis_style/tables/`",
  "",
  .section_lines("Main", "Main manuscript candidates"),
  .section_lines("Supplement", "Supplementary manuscript candidates"),
  .section_lines(.ref_role, "Reference outputs"),
  "## Known differences from the reference R Markdown report",
  "",
  "- Visible labels use AI-assisted/No-AI terminology where appropriate.",
  paste0("- Power calculations use the configured target of ",
         .power$target_power_label, "."),
  "- The period GLMM is labeled as a period/test-order model, not an AI-first/AI-second model.",
  "- Restricted scoring follows the configured item exclusions for this run.",
  paste0("- Form X has ", score_meta$restricted_item_counts$x,
         " included items and restricted Form Y has ",
         score_meta$restricted_item_counts$y, " included items."),
  paste0("- Scores use the ", .score_metric_short, " metric."),
  "- Numeric values reflect this run's configured exclusions and common-scale rescaling.",
  "",
  "## Files intentionally not selected",
  "",
  "- `figures/primary/score_delta_dotplot.png` - alternate paired-effect visualization.",
  "- `figures/primary/effect_size_forest.png` - alternate effect-size figure.",
  "- `figures/primary/intervention_effect_by_sequence.png` - alternate sequence-stratified figure.",
  "- `tables/primary/00_main_results.csv` - compact primary table alias.",
  "- `tables/primary/00_overall_results.csv` - compact primary table.",
  "- `tables_png/primary/00_overall_results.png` - PNG of compact primary table.",
  "- `tables/primary/03_primary_contrasts.csv` - broader contrast table retained in canonical outputs.",
  "- `figures/supplementary/post_hoc_power_curve.png` - selected as main Figure 2, not duplicated in the supplement.",
  "- `tables/exploratory/19_item_endorsement_rates.csv` - retained in canonical outputs; the manuscript-selected item endorsement output is main Figure 3.",
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

.validate_selected_manuscript_structure <- function(root) {
  main_figs <- sort(gsub(
    "\\\\", "/",
    list.files(file.path(root, "main_body", "figures"),
               pattern = "\\.png$", full.names = FALSE)
  ))
  expected_main_figs <- c(
    "figure1_paired_score_plot.png",
    "figure2_power_curve.png",
    "figure3_item_endorsement_by_sequence.png"
  )
  if (!identical(main_figs, expected_main_figs)) {
    stop(
      "Main figure filenames do not match the finalized manuscript order.\n",
      "Expected: ", paste(expected_main_figs, collapse = ", "), "\n",
      "Actual: ", paste(main_figs, collapse = ", ")
    )
  }

  supp_figs <- sort(gsub(
    "\\\\", "/",
    list.files(file.path(root, "supplement", "figures"),
               pattern = "\\.png$", full.names = FALSE)
  ))
  expected_supp_figs <- c(
    if (.generate_flow_selected) "figure_s1_participant_flow.png",
    "figure_s2_score_distributions_by_condition.png",
    "figure_s3_paired_condition_difference_distribution.png",
    "figure_s4_fisher_pitman_null_distributions.png"
  )
  if (!identical(supp_figs, expected_supp_figs)) {
    stop(
      "Supplementary figure filenames do not match Appendix F.\n",
      "Expected: ", paste(expected_supp_figs, collapse = ", "), "\n",
      "Actual: ", paste(supp_figs, collapse = ", ")
    )
  }

  supp_csv <- sort(gsub(
    "\\\\", "/",
    list.files(file.path(root, "supplement", "tables"),
               pattern = "\\.csv$", full.names = FALSE)
  ))
  expected_supp_csv <- c(
    "table_s1_post_hoc_power_analysis.csv",
    "table_s2_sign_permutation_tests.csv",
    "table_s3_logistic_mixed_model_results.csv",
    "table_s4_randomized_sequence_paired_differences.csv",
    "table_s5_descriptive_scores.csv"
  )
  if (!identical(supp_csv, expected_supp_csv)) {
    stop(
      "Supplementary table CSV filenames do not match Appendix F.\n",
      "Expected: ", paste(expected_supp_csv, collapse = ", "), "\n",
      "Actual: ", paste(supp_csv, collapse = ", ")
    )
  }

  supp_png <- sort(gsub(
    "\\\\", "/",
    list.files(file.path(root, "supplement", "tables"),
               pattern = "\\.png$", full.names = FALSE)
  ))
  expected_supp_png <- sub("\\.csv$", ".png", expected_supp_csv)
  if (!identical(supp_png, expected_supp_png)) {
    stop(
      "Supplementary table PNG filenames do not match Appendix F.\n",
      "Expected: ", paste(expected_supp_png, collapse = ", "), "\n",
      "Actual: ", paste(supp_png, collapse = ", ")
    )
  }

  if (any(grepl("power", supp_figs, ignore.case = TRUE))) {
    stop("The power curve is duplicated in selected supplementary figures.")
  }

  stale_terms <- c(
    "paired sign-flip", "two-tailed", "one-tailed",
    "Sequence order", "Control-first", "Intervention-first"
  )
  text_files <- list.files(
    file.path(root, c("main_body", "supplement")),
    recursive = TRUE,
    full.names = TRUE
  )
  text_files <- text_files[tolower(tools::file_ext(text_files)) %in%
                             c("csv", "md", "txt", "tsv", "json", "yml", "yaml")]
  stale_hits <- character()
  for (f in text_files) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character())
    for (term in stale_terms) {
      hit_idx <- grep(term, lines, ignore.case = FALSE, fixed = TRUE)
      if (length(hit_idx) > 0) {
        rel <- gsub("\\\\", "/", substring(f, nchar(root) + 2L))
        stale_hits <- c(
          stale_hits,
          paste0(rel, ":", hit_idx, ": ", term)
        )
      }
    }
  }
  if (length(stale_hits) > 0) {
    stop(
      "Selected manuscript-facing text contains stale terminology:\n  ",
      paste(stale_hits, collapse = "\n  ")
    )
  }

  log_check("Selected manuscript structure validation passed.")
}

.validate_selected_manuscript_structure(.sel_root)

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
log_line("Power curve : manuscript_selected/main_body/figures/figure2_power_curve.png")

if (exists("session_record_module", envir = .GlobalEnv)) {
  session_record_module("manuscript_selected", "OK", 0)
}



