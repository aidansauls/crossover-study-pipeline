## =============================================================================
## R/09_audit.R
## Per-run output inventory.
## Writes AUDIT.csv and AUDIT.md to the output root of a pipeline run or
## comparison run.
## Sourced from run_all.R (regular runs) and 08_comparison_figures.R (comparison).
## Copyright (c) 2026 Aidan Sauls â€” see LICENSE for terms.
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
      message("[AUDIT] Cannot resolve study name â€” skipping.")
      return(invisible(NULL))
    }
    .name <- .sn
    .dir  <- file.path(proj_root, "outputs", .name)
  }

  if (!dir.exists(.dir)) {
    message("[AUDIT] Output directory not found: ", .dir, " â€” skipping.")
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
  # Registry: known file stem â†’ (desc, scoring, role, note)
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
    "condition_score_histogram_restricted"    = list(desc = "Restricted score histograms by condition",                              scoring = "Restricted",      role = "Supplementary", note = "Reference supporting figure"),
    "sequence_difference_histogram_restricted"= list(desc = "Restricted paired differences by sequence order",                       scoring = "Restricted",      role = "Supplementary", note = "Reference supporting figure"),
    "paired_difference_dotplot_restricted"    = list(desc = "Restricted participant-level AI-assisted minus No-AI differences",      scoring = "Restricted",      role = "Supplementary", note = "Reference supporting figure"),
    "sign_test_counts_restricted"             = list(desc = "Sign-test direction counts for restricted paired differences",          scoring = "Restricted",      role = "Supplementary", note = "Supporting sign test"),
    "paired_permutation_null_restricted"      = list(desc = "Paired sign-flip permutation null distribution",                       scoring = "Restricted",      role = "Supplementary", note = "Supporting permutation test"),
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
    "post_hoc_power_curve"                    = list(desc = "Post-hoc paired-test power curve by target effect",                   scoring = "Restricted",      role = "Supplementary", note = "Uses restricted AI minus No-AI paired differences"),
    # Tables
    "01_participant_flow"                 = list(desc = "Participant flow and sequence allocation",                        scoring = "N/A",             role = "Descriptive",   note = ""),
    "02_descriptive_statistics"           = list(desc = "Descriptive statistics by condition and period",                 scoring = "Full",            role = "Descriptive",   note = ""),
    "02b_condition_descriptives_restricted" = list(desc = "Restricted descriptive summary: AI-assisted vs No-AI",          scoring = "Restricted",      role = "Descriptive",   note = "Reference supporting table"),
    "02c_sequence_descriptives_restricted"  = list(desc = "Restricted descriptive summary by sequence order",              scoring = "Restricted",      role = "Descriptive",   note = "Reference supporting table"),
    "03_primary_contrasts"                = list(desc = "Paired contrasts: intervention + period effects",                scoring = "Full+Restricted", role = "Core",          note = ""),
    "04a_carryover_test"                  = list(desc = "Grizzle carryover test",                                         scoring = "Full",            role = "Secondary",     note = ""),
    "04b_sequence_period_interaction"     = list(desc = "Sequence x period interaction",                                  scoring = "Full",            role = "Secondary",     note = ""),
    "05_item_analysis_summary"            = list(desc = "Item-level difficulty and discrimination",                       scoring = "N/A",             role = "Psychometrics", note = ""),
    "06_reliability_summary"              = list(desc = "Internal consistency (KR-20, alpha, omega)",                     scoring = "Full+Restricted", role = "Psychometrics", note = ""),
    "07_mixed_model_results"              = list(desc = "Linear mixed-effects model results",                             scoring = "Full+Restricted", role = "Core",          note = ""),
    "07b_logistic_mixed_model_results"     = list(desc = "Exploratory item-level logistic mixed model results",            scoring = "Restricted",      role = "Exploratory",   note = "Models are condition, period, and sequence effects"),
    "08_effect_size_summary"              = list(desc = "Cohen's dz effect sizes",                                       scoring = "Full+Restricted", role = "Core",          note = ""),
    "08b_paired_difference_effect_size"    = list(desc = "Restricted paired difference and effect size summary",           scoring = "Restricted",      role = "Core",          note = "Includes Cohen dz and Hedges gz"),
    "09_period_condition_cell_means"      = list(desc = "Cell means by period and sequence",                              scoring = "Restricted",      role = "Descriptive",   note = ""),
    "10_normality_tests"                  = list(desc = "Normality tests (Shapiro-Wilk)",                                 scoring = "Full",            role = "Supplementary", note = ""),
    "10a_sign_permutation_tests"           = list(desc = "Exact sign test and paired sign-flip permutation test",          scoring = "Restricted",      role = "Supplementary", note = "Supporting nonparametric analyses"),
    "11_ceiling_effects"                  = list(desc = "Ceiling effects analysis",                                       scoring = "Full",            role = "Supplementary", note = ""),
    "12_model_comparison"                 = list(desc = "LME model comparison (full vs reduced)",                        scoring = "Full",            role = "Exploratory",   note = ""),
    "13_full_vs_restricted_comparison"    = list(desc = "Within-run: full vs restricted scoring â€” means, SDs, r",        scoring = "Full+Restricted", role = "Supplementary", note = "Within-run comparison only"),
    "13b_full_vs_restricted_effect_sizes" = list(desc = "Within-run: Cohen's dz under full vs restricted scoring",       scoring = "Full+Restricted", role = "Supplementary", note = "Within-run comparison only"),
    "20_post_hoc_power_analysis"          = list(desc = "Post-hoc paired-test sample sizes by target effect",            scoring = "Restricted",      role = "Supplementary", note = "Uses restricted AI minus No-AI paired differences"),
    # Comparison outputs
    "variant_comparison_restricted_results"             = list(desc = "Cross-run restricted-score summary (all compared variants)",       scoring = "Restricted",      role = "Cross-run",   note = "Core cross-run comparison"),
    "variant_comparison_full_vs_restricted"             = list(desc = "Cross-run: Cohen dz under full vs restricted scoring, per variant", scoring = "Full+Restricted", role = "Cross-run",   note = "Core cross-run comparison"),
    "variant_comparison_period_effect"                  = list(desc = "Cross-run: Period effect (P2 âˆ’ P1) under restricted scoring",        scoring = "Restricted",      role = "Cross-run",   note = "Period contrast + mixed-model corroboration"),
    "variant_comparison_period_cell_means"              = list(desc = "Cross-run: Period 1/2 cell means by sequence group",               scoring = "Restricted",      role = "Cross-run",   note = "Descriptive cross-run"),
    "variant_comparison_condition_by_sequence"          = list(desc = "Cross-run: AI vs Control means within each sequence group (descriptive)",   scoring = "Restricted",      role = "Cross-run",   note = "Descriptive within-person contrast by group â€” no inferential statistics"),
    "variant_comparison_condition_by_sequence_inferential" = list(desc = "Cross-run: within-subject AI vs Control paired contrasts by sequence group", scoring = "Restricted",      role = "Cross-run",   note = "Within-subject paired contrasts only â€” NOT a between-group or moderation test"),
    "variant_comparison_period_by_sequence"             = list(desc = "Cross-run: within-subject P2 âˆ’ P1 period effect by sequence group",            scoring = "Restricted",      role = "Cross-run",   note = "Within-subject paired contrasts only â€” NOT a between-group or moderation test"),
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

  # ---------------------------------------------------------------------------
  # Reproducibility run audit
  # ---------------------------------------------------------------------------
  .run_audit_dir <- file.path(.dir, "run_audit")
  dir.create(.run_audit_dir, recursive = TRUE, showWarnings = FALSE)

  .cfg <- if (exists("read_config", mode = "function")) {
    tryCatch(read_config(), error = function(e) NULL)
  } else NULL
  .score_meta <- tryCatch(readRDS(file.path(.dir, "rds", "score_metadata.rds")),
                          error = function(e) NULL)
  .results <- tryCatch(readRDS(file.path(.dir, "rds", "analysis_results.rds")),
                       error = function(e) NULL)

  .cfg_path <- if (exists(".config_path", inherits = TRUE)) {
    get(".config_path", inherits = TRUE)
  } else {
    Sys.getenv("PIPELINE_CONFIG", unset = "(default)")
  }

  .x_excl <- if (!is.null(.cfg)) as.character(unlist(.cfg$item_exclusions$x %||% list())) else character(0)
  .y_excl <- if (!is.null(.cfg)) as.character(unlist(.cfg$item_exclusions$y %||% list())) else character(0)
  .x_excl <- .x_excl[nzchar(.x_excl)]
  .y_excl <- .y_excl[nzchar(.y_excl)]

  .score_summary <- data.frame(
    form = c("Form X", "Form Y"),
    full_item_count = c(.score_meta$full_item_counts$x %||% NA_integer_,
                        .score_meta$full_item_counts$y %||% NA_integer_),
    restricted_item_count = c(.score_meta$restricted_item_counts$x %||% NA_integer_,
                              .score_meta$restricted_item_counts$y %||% NA_integer_),
    primary_metric = .score_meta$primary_metric %||% NA_character_,
    primary_label = .score_meta$primary_label %||% NA_character_,
    stringsAsFactors = FALSE
  )
  write.csv(.score_summary,
            file.path(.run_audit_dir, "score_metadata_summary.csv"),
            row.names = FALSE)

  .session_path <- file.path(.run_audit_dir, "session_info.txt")
  writeLines(capture.output(utils::sessionInfo()), .session_path)

  .git_hash <- tryCatch(
    system2("git", c("-C", PROJ_ROOT, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  if (length(.git_hash) == 0) .git_hash <- NA_character_

  .validations <- character(0)
  .add_val <- function(ok, msg_ok, msg_bad) {
    .validations <<- c(.validations, paste0(if (isTRUE(ok)) "PASS: " else "WARN: ",
                                            if (isTRUE(ok)) msg_ok else msg_bad))
  }

  .denom_diff <- isTRUE(.score_meta$any_unequal_denominators %||% FALSE)
  .raw_metric <- grepl("raw|total", .score_meta$primary_metric %||% "", ignore.case = TRUE)
  .add_val(!(.denom_diff && .raw_metric),
           "Unequal denominators are not using raw total-correct scores.",
           "Unequal denominators detected with a raw/total primary metric.")

  .power <- .results$post_hoc_power %||% NULL
  if (!is.null(.power)) {
    .expected_power_label <- scales::percent(.power$target_power, accuracy = 1)
    .add_val(identical(.power$target_power_label, .expected_power_label),
             paste0("Target-power label matches target_power (", .expected_power_label, ")."),
             paste0("Target-power label mismatch: target_power=", .power$target_power,
                    ", label=", .power$target_power_label,
                    ", expected=", .expected_power_label, "."))
  }

  .log_tbl <- .results$reference_analysis$logistic_models$table %||% NULL
  if (!is.null(.log_tbl) && nrow(.log_tbl) > 0) {
    .bad_period_wording <- any(
      grepl("period_fac", .log_tbl$Formula, ignore.case = TRUE) &
        grepl("AI first|AI second", .log_tbl$Model, ignore.case = TRUE)
    )
    .add_val(!.bad_period_wording,
             "Period-formula logistic models are described as period effects.",
             "A Period logistic model appears to be described as AI first/AI second.")
  }

  .tables_base <- file.path(.dir, "tables")
  .tpng_base <- file.path(.dir, "tables_png")
  .csv_rel <- if (dir.exists(.tables_base)) {
    list.files(.tables_base, pattern = "\\.csv$", recursive = TRUE, full.names = FALSE)
  } else character(0)
  .missing_png <- .csv_rel[!file.exists(file.path(.tpng_base, sub("\\.csv$", ".png", .csv_rel)))]
  .legacy_suppressed_csv <- c(
    "psychometrics/05_item_analysis_summary.csv",
    "psychometrics/06_reliability_summary.csv",
    "psychometrics/ability_stratified_item_difficulty.csv",
    "psychometrics/item_analysis.csv",
    "psychometrics/reliability_summary.csv",
    "psychometrics/suspicious_items.csv"
  )
  .missing_png <- setdiff(.missing_png, .legacy_suppressed_csv)
  .add_val(length(.missing_png) == 0,
           "Every CSV table has a corresponding PNG table.",
           paste0("Missing PNG table output(s): ", paste(.missing_png, collapse = ", ")))

  .display_labels <- .cfg$display_labels %||% list()
  .expect_display <- identical(.display_labels$condition_control %||% "", "No-AI") &&
    identical(.display_labels$condition_ai %||% "", "AI-assisted")
  if (.expect_display) {
    .scan_dirs <- c(file.path(.dir, "tables"), file.path(.dir, "manuscript_selected"))
    .scan_files <- unlist(lapply(.scan_dirs, function(d) {
      if (dir.exists(d)) list.files(d, pattern = "\\.(csv|md|txt)$",
                                    recursive = TRUE, full.names = TRUE)
      else character(0)
    }), use.names = FALSE)
    .bad_label_files <- character(0)
    for (.sf in .scan_files) {
      .txt <- tryCatch(readLines(.sf, warn = FALSE, encoding = "UTF-8"),
                       error = function(e) character(0))
      if (any(grepl("\\b(Intervention|Control|Intervention-first|Control-first)\\b",
                    .txt))) {
        .bad_label_files <- c(.bad_label_files, normalizePath(.sf, winslash = "/", mustWork = FALSE))
      }
    }
    .add_val(length(.bad_label_files) == 0,
             "Configured display labels are used in scanned manuscript-facing text outputs.",
             paste0("Internal labels remain in scanned output(s): ",
                    paste(.bad_label_files, collapse = "; ")))
  }

  .modules_txt <- if (exists(".sess") && is.environment(.sess) &&
                      exists("modules", envir = .sess, inherits = FALSE)) {
    vapply(names(.sess$modules), function(mid) {
      m <- .sess$modules[[mid]]
      paste0(mid, "=", m$status, " (", round(m$elapsed_s %||% 0, 1), "s)")
    }, character(1))
  } else character(0)

  .warnings_txt <- if (exists(".sess") && is.environment(.sess) &&
                       exists("warnings_list", envir = .sess, inherits = FALSE) &&
                       length(.sess$warnings_list) > 0) .sess$warnings_list else "none"
  .errors_txt <- if (exists(".sess") && is.environment(.sess) &&
                     exists("errors_list", envir = .sess, inherits = FALSE) &&
                     length(.sess$errors_list) > 0) .sess$errors_list else "none"

  .key_paths <- c(
    file.path(.dir, "tables", "descriptive", "02b_condition_descriptives_restricted.csv"),
    file.path(.dir, "tables", "primary", "08b_paired_difference_effect_size.csv"),
    file.path(.dir, "tables", "supplementary", "10a_sign_permutation_tests.csv"),
    file.path(.dir, "tables", "mixed_models", "07b_logistic_mixed_model_results.csv"),
    file.path(.dir, "tables", "supplementary", "20_post_hoc_power_analysis.csv"),
    file.path(.dir, "figures", "primary", "intervention_vs_control_paired.png"),
    file.path(.dir, "figures", "supplementary", "post_hoc_power_curve.png")
  )

  .audit_lines <- c(
    paste0("# Analysis Run Log: ", .name),
    "",
    paste0("- Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("- Study name: ", .name),
    paste0("- Config file: ", .cfg_path),
    paste0("- Git commit: ", .git_hash[1]),
    paste0("- Item exclusions Form X: ", if (length(.x_excl)) paste(toupper(.x_excl), collapse = ", ") else "none"),
    paste0("- Item exclusions Form Y: ", if (length(.y_excl)) paste(toupper(.y_excl), collapse = ", ") else "none"),
    paste0("- Restricted item counts: Form X = ", .score_meta$restricted_item_counts$x %||% NA,
           "; Form Y = ", .score_meta$restricted_item_counts$y %||% NA),
    paste0("- Score formula: ", .score_meta$formula$score_prop %||% "correct_included / n_items_included",
           "; score = score_prop * ", .score_meta$scale_to %||% NA),
    paste0("- Score metric: ", .score_meta$primary_label %||% NA),
    paste0("- Primary score variable: intervention_score_", .results$reference_analysis$scoring %||% "restricted",
           " and control_score_", .results$reference_analysis$scoring %||% "restricted"),
    "- Paired difference definition: AI-assisted minus No-AI",
    paste0("- Target power: ", .power$target_power %||% NA,
           " (", .power$target_power_label %||% NA, "); alpha = ", .power$alpha %||% NA),
    paste0("- Target effects: ", paste0(.power$target_effects_pct %||% NA, " percentage points", collapse = ", ")),
    "",
    "## Analyses Run",
    if (length(.modules_txt)) paste0("- ", .modules_txt) else "- Module timing unavailable",
    "",
    "## Key Output Paths",
    paste0("- ", normalizePath(.key_paths, winslash = "/", mustWork = FALSE)),
    "",
    "## Validation Checks",
    paste0("- ", .validations),
    "",
    "## Warnings",
    paste0("- ", .warnings_txt),
    "",
    "## Errors",
    paste0("- ", .errors_txt),
    ""
  )

  writeLines(.audit_lines, file.path(.run_audit_dir, "analysis_run_log.md"),
             useBytes = TRUE)
  writeLines(gsub("^# |^## ", "", .audit_lines),
             file.path(.run_audit_dir, "analysis_run_log.txt"),
             useBytes = TRUE)
  cat("  Run audit folder  -> run_audit/\n")
  cat(strrep("-", 72), "\n")
})

