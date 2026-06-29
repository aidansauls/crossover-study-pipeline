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

.out_root <- file.path(PROJ_ROOT, "outputs", STUDY_NAME)
.sel_root <- file.path(.out_root, "manuscript_selected")
.dirs <- file.path(
  .sel_root,
  c(
    "main_body/figures",
    "main_body/tables",
    "supplement/figures",
    "supplement/tables",
    "supplement/audit",
    "alex_style_reference/figures",
    "alex_style_reference/tables"
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
      gt_tbl <- gt::gt(df) |>
        gt::tab_header(title = gt::md(paste0("**", caption, "**"))) |>
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

.fmt_p <- function(p) {
  if (is.na(p)) "" else if (p < .001) "< .001" else sprintf("= %.3f", p)
}

.fmt2 <- function(x) sprintf("%.2f", x)
.fmt3 <- function(x) sprintf("%.3f", x)

.alex <- results$alex_supporting
if (is.null(.alex)) {
  stop("analysis_results.rds does not contain alex_supporting outputs. Run R/04_analyses.R first.")
}

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
.paired <- .alex$paired_effect
.sign <- .alex$sign_permutation
.log_tbl <- .alex$logistic_models$table
if (is.null(.paired) || is.null(.sign) || is.null(.log_tbl)) {
  stop("Alex-style paired/sign/logistic outputs are incomplete in analysis_results.rds.")
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

.condition_desc <- .alex$condition_descriptives
.condition_desc_tbl <- data.frame(
  Condition = as.character(.condition_desc$condition),
  N = .condition_desc$n,
  Mean = round(.condition_desc$mean, 2),
  SD = round(.condition_desc$sd, 2),
  Median = round(.condition_desc$median, 2),
  IQR = sprintf("[%.2f, %.2f]",
                .condition_desc$iqr_low, .condition_desc$iqr_high),
  Range = sprintf("%.2f-%.2f", .condition_desc$min, .condition_desc$max),
  check.names = FALSE
)

.paired_desc_tbl <- data.frame(
  Scoring = .paired$scoring,
  Metric = .paired$score_metric,
  Contrast = "AI-assisted - No-AI",
  N = .paired$n,
  `AI-assisted mean (SD)` = sprintf("%.2f (%.2f)",
                                    .paired[["AI-assisted mean"]],
                                    .paired[["AI-assisted SD"]]),
  `No-AI mean (SD)` = sprintf("%.2f (%.2f)",
                              .paired[["No-AI mean"]],
                              .paired[["No-AI SD"]]),
  `Mean paired difference` = round(.paired[["Mean paired difference"]], 3),
  `SD paired difference` = round(.paired[["SD paired difference"]], 3),
  `95% CI` = sprintf("[%.2f, %.2f]",
                     .paired[["95% CI low"]],
                     .paired[["95% CI high"]]),
  `Cohen's dz` = round(.paired[["Cohen dz"]], 3),
  `Hedges gz` = round(.paired[["Hedges gz"]], 3),
  t = round(.paired$t, 3),
  df = .paired$df,
  p = vapply(.paired$p, .fmt_p, character(1)),
  check.names = FALSE
)

.seq_desc <- .alex$sequence_descriptives
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
# Alex-style reference outputs.
# ---------------------------------------------------------------------------
.write_selected_table(
  .condition_desc_tbl,
  "alex_style_reference/tables/alex_condition_descriptive_table.csv",
  "alex_style_reference/tables/alex_condition_descriptive_table.png",
  "Alex-style condition descriptive table",
  "Alex-style condition descriptive table using corrected labels and strict Y1+Y6 scoring.",
  "Alex-style reference", "Alex condition descriptive table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .paired_desc_tbl,
  "alex_style_reference/tables/alex_paired_difference_descriptive_table.csv",
  "alex_style_reference/tables/alex_paired_difference_descriptive_table.png",
  "Alex-style paired difference descriptive table",
  "Alex-style paired difference table for AI-assisted minus No-AI.",
  "Alex-style reference", "Alex paired difference table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sequence_desc_tbl,
  "alex_style_reference/tables/alex_sequence_difference_table.csv",
  "alex_style_reference/tables/alex_sequence_difference_table.png",
  "Alex-style sequence difference table",
  "Alex-style sequence difference table using AI-assisted first and No-AI first labels.",
  "Alex-style reference", "Alex sequence difference table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .sign_tbl,
  "alex_style_reference/tables/alex_sign_test_table.csv",
  "alex_style_reference/tables/alex_sign_test_table.png",
  "Alex-style sign and permutation test table",
  "Alex-style sign test and two-tailed sign-flip permutation table.",
  "Alex-style reference", "Alex sign test table",
  "rds/analysis_results.rds"
)
.write_selected_table(
  .power_table,
  "alex_style_reference/tables/alex_post_hoc_power_table.csv",
  "alex_style_reference/tables/alex_post_hoc_power_table.png",
  "Alex-style post hoc power table",
  "Alex-style post hoc power table corrected to 80% power.",
  "Alex-style reference", "Alex power table",
  "tables/supplementary/20_post_hoc_power_analysis.csv"
)
.write_selected_table(
  .clean_logistic,
  "alex_style_reference/tables/alex_glmm_results_table.csv",
  "alex_style_reference/tables/alex_glmm_results_table.png",
  "Alex-style GLMM results table",
  "Alex-style GLMM table with corrected model labels and interpretations.",
  "Alex-style reference", "Alex GLMM results table",
  "rds/analysis_results.rds"
)

.copy_selected(
  "figures/supplementary/condition_score_histogram_restricted.png",
  "alex_style_reference/figures/alex_condition_histogram.png",
  "Alex-style condition score histogram with corrected condition labels.",
  "Alex-style reference", "Alex condition histogram"
)
.copy_selected(
  "figures/supplementary/sequence_difference_histogram_restricted.png",
  "alex_style_reference/figures/alex_sequence_histogram.png",
  "Alex-style sequence difference histogram with corrected sequence labels.",
  "Alex-style reference", "Alex sequence histogram"
)
.copy_selected(
  "figures/descriptive/score_difference_histogram.png",
  "alex_style_reference/figures/alex_paired_difference_histogram.png",
  "Alex-style paired difference histogram.",
  "Alex-style reference", "Alex paired difference histogram"
)
.copy_selected(
  "figures/supplementary/paired_permutation_null_restricted.png",
  "alex_style_reference/figures/alex_permutation_null_two_tailed.png",
  "Alex-style two-tailed permutation null distribution.",
  "Alex-style reference", "Alex two-tailed permutation null"
)
file.copy(
  file.path(.sel_root, "supplement/figures/figure_s8_permutation_null_one_tailed.png"),
  file.path(.sel_root, "alex_style_reference/figures/alex_permutation_null_one_tailed.png"),
  overwrite = TRUE
)
.add_record(
  "alex_style_reference/figures/alex_permutation_null_one_tailed.png",
  "rds/analysis_results.rds",
  "Alex-style one-tailed permutation null distribution.",
  "Alex-style reference", "Alex one-tailed permutation null"
)
.copy_selected(
  "figures/supplementary/post_hoc_power_curve.png",
  "alex_style_reference/figures/alex_post_hoc_power_curve.png",
  "Alex-style post hoc power curve corrected to 80% target power.",
  "Alex-style reference", "Alex power curve"
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
  "- `alex_style_reference/figures/`",
  "- `alex_style_reference/tables/`",
  "",
  .section_lines("Main", "Main manuscript candidates"),
  .section_lines("Supplement", "Supplementary manuscript candidates"),
  .section_lines("Alex-style reference", "Alex-style reference outputs"),
  .section_lines("Audit", "Audit/reproducibility outputs"),
  "## Known differences from Alex's original R Markdown report",
  "",
  "- Visible labels were changed from intervention/control wording to AI-assisted/No-AI where appropriate.",
  "- Power calculations are 80% power, not 90%.",
  "- The period GLMM is labeled as a period/test-order model, not an AI-first/AI-second model.",
  "- Strict Y1+Y6 scoring excludes Form Y items 1 and 6.",
  paste0("- Form X has ", score_meta$restricted_item_counts$x,
         " included items and restricted Form Y has ",
         score_meta$restricted_item_counts$y, " included items."),
  "- Scores use the rescaled 0-10 score metric.",
  "- Some numeric values may differ from Alex's original report because this folder uses the final strict Y1+Y6 scoring and common-scale rescaling.",
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

