## =============================================================================
## R/09_audit.R
## Per-run output inventory.
## Writes AUDIT.csv and AUDIT.md to the output root of a pipeline run or
## comparison run.
## Sourced from run_all.R (regular runs) and 08_comparison_figures.R (comparison).
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

local({

  # ---------------------------------------------------------------------------
  # Resolve output directory
  # ---------------------------------------------------------------------------
  .is_cmp <- exists("out_name") && nzchar(get("out_name") %||% "")

  if (.is_cmp) {
    .name <- out_name
    .dir  <- file.path(proj_root, "outputs", out_name)
  } else if (exists("study_name") && nzchar(study_name %||% "")) {
    .name <- study_name
    .dir  <- file.path(proj_root, "outputs", study_name)
  } else {
    .sn <- Sys.getenv("STUDY_NAME", unset = "")
    if (!nzchar(.sn)) {
      message("[AUDIT] Cannot resolve study name — skipping.")
      return(invisible(NULL))
    }
    .name <- .sn
    .dir  <- file.path(proj_root, "outputs", .name)
  }

  if (!dir.exists(.dir)) {
    message("[AUDIT] Output directory not found: ", .dir, " — skipping.")
    return(invisible(NULL))
  }

  # Collect comparison run info (available when sourced from 08_comparison_figures.R)
  .cmp_runs <- if (.is_cmp && exists("runs") && is.list(runs))
    runs else list()

  cat(strrep("-", 72), "\n")
  cat("  AUDIT:", .name, "\n")
  if (length(.cmp_runs) > 0) {
    .run_labels <- vapply(.cmp_runs, function(r) {
      nm <- r[["name"]] %||% "?"
      lb <- r[["label"]] %||% nm
      if (lb == nm) nm else paste0(lb, " (", nm, ")")
    }, character(1))
    cat("  Comparing:", paste(.run_labels, collapse = " vs "), "\n")
  }

  # ---------------------------------------------------------------------------
  # Registry: known file stem → (desc, scoring, role, note)
  #
  # scoring: Full | Restricted | Full+Restricted | Cross-run | N/A
  # role:    Core | Secondary | Descriptive | Exploratory |
  #          Supplementary | Psychometrics | Cross-run
  # ---------------------------------------------------------------------------
  .REG <- list(
    # Figures
    "intervention_vs_control_paired"          = list(desc = "Paired spaghetti: Intervention vs Control (per-participant change)",     scoring = "Full",            role = "Core",          note = ""),
    "intervention_effect_by_sequence"         = list(desc = "Intervention effect by sequence group (AB vs BA)",                      scoring = "Full",            role = "Core",          note = ""),
    "effect_size_forest"                      = list(desc = "Forest plot of Cohen's dz effect sizes",                               scoring = "Full+Restricted", role = "Core",          note = ""),
    "score_delta_dotplot"                     = list(desc = "Intervention minus control score delta: dot plot",                      scoring = "Full",            role = "Core",          note = ""),
    "period_effects_by_sequence"              = list(desc = "Period effects by sequence group",                                      scoring = "Full",            role = "Secondary",     note = ""),
    "form_x_vs_form_y_correlation"            = list(desc = "Form X vs Form Y score correlation (scatter)",                         scoring = "Full",            role = "Psychometrics", note = ""),
    "mixed_model_fixed_effects"               = list(desc = "Mixed-model fixed-effect coefficients (bar chart)",                    scoring = "Full+Restricted", role = "Core",          note = ""),
    "lme_residual_diagnostic"                 = list(desc = "LME residual diagnostics: QQ + fitted-vs-residual",                    scoring = "Full",            role = "Secondary",     note = ""),
    "per_participant_condition_scores"        = list(desc = "Per-participant condition score profiles",                              scoring = "Full",            role = "Supplementary", note = ""),
    "score_distributions_violin_box"          = list(desc = "Score distributions (violin + box) by condition",                      scoring = "Full",            role = "Descriptive",   note = ""),
    "score_distributions_by_form"             = list(desc = "Score distributions by form (X vs Y)",                                scoring = "Full",            role = "Descriptive",   note = ""),
    "subgroup4_delta_dotplot"                 = list(desc = "Score delta dot plot: subgroup-4 participants",                        scoring = "Full",            role = "Exploratory",   note = ""),
    "subgroup4_intervention_spaghetti"        = list(desc = "Spaghetti plot for subgroup-4 (possible outlier group)",               scoring = "Full",            role = "Exploratory",   note = ""),
    "item_difficulty_by_form"                 = list(desc = "Item difficulty (p-value) by form X and Y",                           scoring = "N/A",             role = "Psychometrics", note = ""),
    "item_discrimination_boxplot_form_x"      = list(desc = "Item discrimination (point-biserial r) - Form X",                     scoring = "N/A",             role = "Psychometrics", note = ""),
    "item_discrimination_boxplot_form_y"      = list(desc = "Item discrimination (point-biserial r) - Form Y",                     scoring = "N/A",             role = "Psychometrics", note = ""),
    "item_rest_correlations"                  = list(desc = "Item-rest correlations by form",                                      scoring = "N/A",             role = "Psychometrics", note = ""),
    "ability_stratified_difficulty_heatmap"   = list(desc = "Item difficulty heatmap stratified by participant ability",            scoring = "N/A",             role = "Psychometrics", note = ""),
    "suspicious_items_scatter"                = list(desc = "Scatter of suspicious items (low discrimination or off-trend)",        scoring = "N/A",             role = "Psychometrics", note = ""),
    "full_vs_restricted_scoring"              = list(desc = "Full vs restricted scoring: side-by-side boxplots",                   scoring = "Full+Restricted", role = "Supplementary", note = "Within-run comparison only"),
    # Tables
    "01_participant_flow"                 = list(desc = "Participant flow and sequence allocation",                        scoring = "N/A",             role = "Descriptive",   note = ""),
    "02_descriptive_statistics"           = list(desc = "Descriptive statistics by condition and period",                 scoring = "Full",            role = "Descriptive",   note = ""),
    "03_primary_contrasts"                = list(desc = "Paired contrasts: intervention + period effects",                scoring = "Full+Restricted", role = "Core",          note = ""),
    "04a_carryover_test"                  = list(desc = "Grizzle carryover test",                                         scoring = "Full",            role = "Secondary",     note = ""),
    "04b_sequence_period_interaction"     = list(desc = "Sequence x period interaction",                                  scoring = "Full",            role = "Secondary",     note = ""),
    "05_item_analysis_summary"            = list(desc = "Item-level difficulty and discrimination",                       scoring = "N/A",             role = "Psychometrics", note = ""),
    "06_reliability_summary"              = list(desc = "Internal consistency (KR-20, alpha, omega)",                     scoring = "Full+Restricted", role = "Psychometrics", note = ""),
    "07_mixed_model_results"              = list(desc = "Linear mixed-effects model results",                             scoring = "Full+Restricted", role = "Core",          note = ""),
    "08_effect_size_summary"              = list(desc = "Cohen's dz effect sizes",                                       scoring = "Full+Restricted", role = "Core",          note = ""),
    "09_period_condition_cell_means"      = list(desc = "Cell means by period and sequence",                              scoring = "Restricted",      role = "Descriptive",   note = ""),
    "10_normality_tests"                  = list(desc = "Normality tests (Shapiro-Wilk)",                                 scoring = "Full",            role = "Supplementary", note = ""),
    "11_ceiling_effects"                  = list(desc = "Ceiling effects analysis",                                       scoring = "Full",            role = "Supplementary", note = ""),
    "12_model_comparison"                 = list(desc = "LME model comparison (full vs reduced)",                        scoring = "Full",            role = "Exploratory",   note = ""),
    "13_full_vs_restricted_comparison"    = list(desc = "Within-run: full vs restricted scoring — means, SDs, r",        scoring = "Full+Restricted", role = "Supplementary", note = "Within-run comparison only"),
    "13b_full_vs_restricted_effect_sizes" = list(desc = "Within-run: Cohen's dz under full vs restricted scoring",       scoring = "Full+Restricted", role = "Supplementary", note = "Within-run comparison only"),
    # Comparison outputs
    "variant_comparison_restricted_results"             = list(desc = "Cross-run restricted-score summary (all compared variants)",       scoring = "Restricted",      role = "Cross-run",   note = "Core cross-run comparison"),
    "variant_comparison_full_vs_restricted"             = list(desc = "Cross-run: Cohen dz under full vs restricted scoring, per variant", scoring = "Full+Restricted", role = "Cross-run",   note = "Core cross-run comparison"),
    "variant_comparison_period_effect"                  = list(desc = "Cross-run: Period effect (P2 − P1) under restricted scoring",        scoring = "Restricted",      role = "Cross-run",   note = "Period contrast + mixed-model corroboration"),
    "variant_comparison_period_cell_means"              = list(desc = "Cross-run: Period 1/2 cell means by sequence group",               scoring = "Restricted",      role = "Cross-run",   note = "Descriptive cross-run"),
    "variant_comparison_condition_by_sequence"          = list(desc = "Cross-run: AI vs Control means within each sequence group (descriptive)",   scoring = "Restricted",      role = "Cross-run",   note = "Descriptive within-person contrast by group — no inferential statistics"),
    "variant_comparison_condition_by_sequence_inferential" = list(desc = "Cross-run: within-subject AI vs Control paired contrasts by sequence group", scoring = "Restricted",      role = "Cross-run",   note = "Within-subject paired contrasts only — NOT a between-group or moderation test"),
    "variant_comparison_period_by_sequence"             = list(desc = "Cross-run: within-subject P2 − P1 period effect by sequence group",            scoring = "Restricted",      role = "Cross-run",   note = "Within-subject paired contrasts only — NOT a between-group or moderation test"),
    # variant_comparison_EXPLORATORY_sequence_moderation intentionally not generated.
    # The period-specific / between-sequence table (Table 15) is suppressed from
    # the comparison package. See 08_comparison_figures.R section 8d comment.
    "FIGURE_CLASSIFICATION"                             = list(desc = "Per-figure comparison tier classification and outcome",            scoring = "N/A",        role = "Cross-run",   note = "Auto-generated by 08_comparison_figures.R")
  )

  # ---------------------------------------------------------------------------
  # Helpers: infer metadata when file is not in registry
  # ---------------------------------------------------------------------------
  .infer_scoring <- function(stem) {
    s <- tolower(stem)
    if (grepl("full_vs_restricted|full.*restricted|restricted.*full", s)) return("Full+Restricted")
    if (grepl("restricted", s)) return("Restricted")
    if (grepl("comparison", s)) return("Cross-run")
    "Full"
  }

  .infer_role <- function(category) {
    switch(category,
      "primary"        = "Core",
      "mixed_models"   = "Core",
      "secondary"      = "Secondary",
      "period_effects" = "Secondary",
      "psychometrics"  = "Psychometrics",
      "item_analysis"  = "Psychometrics",
      "descriptive"    = "Descriptive",
      "exploratory"    = "Exploratory",
      "supplementary"  = "Supplementary",
      ""
    )
  }

  # ---------------------------------------------------------------------------
  # Walk output directories: figures/ and tables/
  # ---------------------------------------------------------------------------
  .specs <- list(
    list(dir = "figures", type = "Figure", ext = "\\.png$"),
    list(dir = "tables",  type = "Table",  ext = "\\.csv$")
  )

  .rows <- list()

  for (.sp in .specs) {
    .base <- file.path(.dir, .sp$dir)
    if (!dir.exists(.base)) next
    .files <- list.files(.base, pattern = .sp$ext,
                         recursive = TRUE, full.names = FALSE)
    for (.f in .files) {
      .stem <- tools::file_path_sans_ext(basename(.f))
      .cat  <- dirname(.f)
      if (.cat == ".") .cat <- ""
      .reg  <- .REG[[.stem]] %||% list(desc = "", scoring = "", role = "", note = "")
      # In comparison context: all stitched figures are cross-run panels
      .scoring <- if (nzchar(.reg$scoring)) .reg$scoring else .infer_scoring(.stem)
      .role    <- if (nzchar(.reg$role))    .reg$role    else .infer_role(if (nzchar(.cat)) .cat else "")
      if (.is_cmp && .sp$type == "Figure") {
        .scoring <- "Cross-run"
        .role    <- paste0(.role, " [cross-run]")
      }
      .rows[[length(.rows) + 1]] <- data.frame(
        File        = .stem,
        Path        = file.path(.sp$dir, .f),
        Type        = .sp$type,
        Category    = if (nzchar(.cat)) .cat else "(root)",
        Scoring     = .scoring,
        Role        = .role,
        Description = .reg$desc,
        Note        = .reg$note,
        stringsAsFactors = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Add suppressed outputs (regular runs with cfg available)
  # ---------------------------------------------------------------------------
  .n_suppressed <- 0L

  if (!.is_cmp &&
      exists("cfg", envir = .GlobalEnv) && is.list(cfg) &&
      exists("cfg_get", envir = .GlobalEnv)) {

    .present <- if (length(.rows) > 0)
      as.character(do.call(rbind, .rows)$File)
    else
      character(0)

    .maybe_suppress <- function(stems, note) {
      for (.s in stems) {
        if (.s %in% .present) next
        .r <- .REG[[.s]] %||% list(desc = "", scoring = "", role = "", note = "")
        .rows[[length(.rows) + 1]] <<- data.frame(
          File        = .s,
          Path        = "(suppressed)",
          Type        = "\u2014",
          Category    = "(suppressed)",
          Scoring     = .r$scoring,
          Role        = .r$role,
          Description = .r$desc,
          Note        = paste0("Suppressed: ", note),
          stringsAsFactors = FALSE
        )
        .n_suppressed <<- .n_suppressed + 1L
      }
    }

    .has_excl <- length(cfg$item_exclusions$y %||% character(0)) > 0 ||
                 length(cfg$item_exclusions$x %||% character(0)) > 0

    if (.has_excl &&
        !isTRUE(cfg_get("optional_analyses", "run_restricted_comparison", default = TRUE))) {
      .maybe_suppress(
        c("full_vs_restricted_scoring",
          "13_full_vs_restricted_comparison",
          "13b_full_vs_restricted_effect_sizes"),
        "run_restricted_comparison = false"
      )
    }

    if (!isTRUE(cfg_get("optional_analyses", "run_normality_tests", default = TRUE)))
      .maybe_suppress("10_normality_tests", "run_normality_tests = false")

    if (!isTRUE(cfg_get("optional_analyses", "run_ceiling_effects_figure", default = TRUE)))
      .maybe_suppress("11_ceiling_effects", "run_ceiling_effects_figure = false")

    if (!isTRUE(cfg_get("optional_analyses", "run_model_comparison", default = TRUE)))
      .maybe_suppress("12_model_comparison", "run_model_comparison = false")
  }

  # ---------------------------------------------------------------------------
  # Build data frame
  # ---------------------------------------------------------------------------
  if (length(.rows) == 0) {
    cat("  No output files found.\n")
    cat(strrep("-", 72), "\n")
    return(invisible(NULL))
  }

  .df          <- do.call(rbind, .rows)
  rownames(.df) <- NULL
  n_gen        <- sum(.df$Path != "(suppressed)")

  # ---------------------------------------------------------------------------
  # Write CSV
  # ---------------------------------------------------------------------------
  .csv_path <- file.path(.dir, "AUDIT.csv")
  write.csv(.df, .csv_path, row.names = FALSE)

  cat(sprintf("  %d files inventoried", n_gen))
  if (.n_suppressed > 0L) cat(sprintf(" + %d suppressed by config", .n_suppressed))
  cat(sprintf(" -> AUDIT.csv\n"))

  # ---------------------------------------------------------------------------
  # Write Markdown
  # ---------------------------------------------------------------------------
  .md_header <- c(
    paste0("# Output Audit: ", .name),
    paste0("_Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "_"),
    ""
  )
  if (length(.cmp_runs) > 0) {
    .md_header <- c(.md_header,
      "**Type:** Cross-run comparison",
      paste0("**Comparing:** ", paste(vapply(.cmp_runs, function(r) {
        lb <- r[["label"]] %||% r[["name"]] %||% "?"
        nm <- r[["name"]]  %||% "?"
        if (lb == nm) nm else paste0(lb, " (`", nm, "`)")
      }, character(1)), collapse = " vs ")),
      paste0("**Figures:** Each panel shows both runs side by side"),
      ""
    )
  }
  .md <- c(.md_header,
    paste0("**", n_gen, " files generated**",
           if (.n_suppressed > 0L)
             paste0(" | **", .n_suppressed, " suppressed by config**")
           else ""),
    ""
  )

  for (.type in c("Figure", "Table")) {
    .sub <- .df[.df$Type == .type, , drop = FALSE]
    if (nrow(.sub) == 0L) next
    .md <- c(.md, paste0("## ", .type, "s"), "",
             "| File | Category | Scoring | Role | Description | Note |",
             "|---|---|---|---|---|---|")
    for (.i in seq_len(nrow(.sub))) {
      .r <- .sub[.i, ]
      .md <- c(.md, paste0(
        "| `", .r$File, "` | ", .r$Category,
        " | ", .r$Scoring, " | ", .r$Role,
        " | ", .r$Description, " | ", .r$Note, " |"
      ))
    }
    .md <- c(.md, "")
  }

  if (.n_suppressed > 0L) {
    .sup <- .df[.df$Path == "(suppressed)", , drop = FALSE]
    .md  <- c(.md, "## Suppressed (config-gated)", "",
              "| File | Description | Note |",
              "|---|---|---|")
    for (.i in seq_len(nrow(.sup))) {
      .r <- .sup[.i, ]
      .md <- c(.md, paste0(
        "| `", .r$File, "` | ", .r$Description, " | ", .r$Note, " |"
      ))
    }
    .md <- c(.md, "")
  }

  .md_con <- file(file.path(.dir, "AUDIT.md"), encoding = "UTF-8")
  writeLines(.md, .md_con)
  close(.md_con)
  cat("  Markdown summary   -> AUDIT.md\n")
  cat(strrep("-", 72), "\n")
})
