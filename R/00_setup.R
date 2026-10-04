## =============================================================================
## R/00_setup.R
## Core setup: packages, config, helpers, logging, save utilities
## Source this file at the top of every analysis script.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# =============================================================================
# PACKAGES
# =============================================================================

.pkgs_required <- c(
  "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble",
  "ggplot2", "scales", "broom", "digest", "yaml", "janitor"
)

.pkgs_optional <- c(
  "patchwork", # Multi-panel figures
  "ggbeeswarm", # Beeswarm jitter
  "gt", # Publication tables
  "webshot2", # PNG export for gt tables
  "kableExtra", # Fallback table PNG
  "lme4", # Mixed-effects models
  "lmerTest", # p-values for lme4
  "psych", # Psychometrics (alpha, omega, tetrachoric)
  "ragg", # High-quality PNG renderer
  "ggrepel", # Non-overlapping labels (suspicious items scatter)
  "magick", # PNG stitching for comparison figures (08_comparison_figures.R)
  "ggforce" # Rounded rectangles in Figure S1
)

.install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg) # message intentional here — before log fns defined
    install.packages(pkg,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org", quiet = TRUE
    )
  }
}

invisible(lapply(.pkgs_required, .install_if_missing))
invisible(lapply(.pkgs_optional, .install_if_missing))

suppressPackageStartupMessages(
  invisible(lapply(.pkgs_required, library, character.only = TRUE))
)

# Load optional packages silently
for (.p in .pkgs_optional) {
  suppressPackageStartupMessages(
    try(library(.p, character.only = TRUE), silent = TRUE)
  )
}

# =============================================================================
# PROJECT ROOT & PATH HELPERS
# =============================================================================

.get_script_dir <- function() {
  this_file <- tryCatch(
    normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
  if (!is.na(this_file) && nzchar(this_file)) {
    d <- dirname(this_file)
    # If sourced from R/ subfolder, go up one level
    if (basename(d) %in% c("R", "scripts")) {
      return(dirname(d))
    }
    return(d)
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

.project_root_override <- Sys.getenv("PIPELINE_PROJECT_ROOT", unset = "")
PROJ_ROOT <- normalizePath(
  if (nzchar(.project_root_override)) .project_root_override else .get_script_dir(),
  winslash = "/",
  mustWork = FALSE
)

# p() builds absolute paths from project root
p <- function(...) file.path(PROJ_ROOT, ...)

# =============================================================================
# CONFIGURATION
# =============================================================================

## Study/analysis config — override with PIPELINE_CONFIG.
.config_path <- Sys.getenv("PIPELINE_CONFIG", unset = "")
if (!nzchar(.config_path)) {
  .config_path <- p("config", "study_config.yml")
}

## Shared visual config — override with PIPELINE_VISUAL_CONFIG.
.visual_config_path <- Sys.getenv("PIPELINE_VISUAL_CONFIG", unset = "")
if (!nzchar(.visual_config_path)) {
  .visual_config_path <- p("config", "visual_config.yml")
}

# Recursively merge named lists. Values from `override` take precedence.
.deep_merge_lists <- function(base, override) {
  if (is.null(override)) {
    return(base)
  }

  if (!is.list(base) || !is.list(override) || is.null(names(override))) {
    return(override)
  }

  out <- base
  for (nm in names(override)) {
    if (
      nm %in% names(out) &&
        is.list(out[[nm]]) &&
        is.list(override[[nm]]) &&
        !is.null(names(override[[nm]]))
    ) {
      out[[nm]] <- .deep_merge_lists(out[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

read_visual_config <- function() {
  if (!file.exists(.visual_config_path)) {
    return(list())
  }

  out <- yaml::read_yaml(.visual_config_path)
  if (is.null(out)) list() else out
}

## Read config (re-parsed each call so edits take effect on the next run).
read_config <- function() {
  if (!file.exists(.config_path)) {
    warning(
      "Config file not found at: ", .config_path,
      "\nCreating sensible defaults."
    )
    cfg <- .default_config()
  } else {
    cfg <- yaml::read_yaml(.config_path)
    if (is.null(cfg)) cfg <- list()
    cfg <- .deep_merge_lists(.default_config(), cfg)
  }

  # Apply presentation settings after the study config. visual_config.yml is
  # intentionally authoritative for presentation; it does not define study
  # semantics such as scoring rules or intervention wording.
  visual_cfg <- read_visual_config()
  if (length(visual_cfg)) {
    cfg <- .deep_merge_lists(cfg, visual_cfg)
  }

  # YAML 1.1 treats bare single-letter keys 'y' and 'n' as boolean TRUE/FALSE.
  # Normalise item_exclusions names so downstream code can use $x and $y safely.
  ie <- cfg[["item_exclusions"]]
  if (!is.null(ie) && !is.null(names(ie))) {
    knames <- names(ie)
    knames[knames == "TRUE"] <- "y"
    knames[knames == "FALSE"] <- "n"
    names(ie) <- knames
    cfg[["item_exclusions"]] <- ie
  }

  ## Env-var override: ITEM_EXCLUSIONS=y1,y6  or  ITEM_EXCLUSIONS=NONE
  ## Set by the BAT multi-run mode so each variant uses different exclusions
  ## without needing separate config files.
  ## - "NONE" clears all exclusions -> full scoring
  ## - "y1,y6" etc. splits on commas/spaces; x-prefix items -> x list,
  ##   y-prefix items -> y list
  .env_excl <- Sys.getenv("ITEM_EXCLUSIONS", unset = "")
  if (nzchar(.env_excl)) {
    if (toupper(trimws(.env_excl)) == "NONE") {
      cfg[["item_exclusions"]] <- list(x = list(), y = list())
    } else {
      .excl_items <- tolower(trimws(strsplit(.env_excl, "[, ]+")[[1]]))
      .excl_items <- .excl_items[nzchar(.excl_items)]
      cfg[["item_exclusions"]] <- list(
        x = as.list(.excl_items[grepl("^x", .excl_items)]),
        y = as.list(.excl_items[grepl("^y", .excl_items)])
      )
    }
  }

  ## Env-var override: RUN_RESTRICTED_COMPARISON=0 suppresses Figure 9 and
  ## Tables 13/13b for exclusion-variant runs where that comparison is not
  ## the focus.
  .env_restr_cmp <- Sys.getenv("RUN_RESTRICTED_COMPARISON", unset = "")
  if (nzchar(.env_restr_cmp) && trimws(.env_restr_cmp) == "0") {
    if (is.null(cfg[["optional_analyses"]])) {
      cfg[["optional_analyses"]] <- list()
    }
    cfg[["optional_analyses"]][["run_restricted_comparison"]] <- FALSE
  }

  cfg
}

.default_config <- function() {
  list(
    study = list(
      name = "study",
      intervention_label = "Intervention",
      control_label = "Control",
      form_x_label = "Form X",
      form_y_label = "Form Y"
    ),
    columns = list(
      participant_id = "Participant_ID",
      intervention_order = "Intervention_Order",
      control_order = "Control_Order",
      form_x_order = "X_Order",
      form_y_order = "Y_Order",
      time_taken = "Time_taken"
    ),
    item_exclusions = list(x = list(), y = list()),
    scores = list(scale_to = 10),
    analysis = list(alpha = 0.05, ci_level = 0.95, min_n_carryover = 4),
    power_analysis = list(
      target_power = 0.80,
      target_effects_pct = c(5, 8, 10, 12, 15, 20),
      max_n = NULL
    ),
    display_labels = list(
      condition_control = "No-AI",
      condition_ai = "AI-assisted",
      condition_control_short = "No-AI",
      condition_ai_short = "AI-assisted",
      condition_control_session = "No-AI study",
      condition_ai_session = "AI-assisted study",
      sequence_control_first = "No-AI first",
      sequence_ai_first = "AI-assisted first"
    ),
    figures = list(
      dpi = 300, width_in = 7.5, height_in = 5.0,
      include_titles = FALSE, base_font_size = 15, font_family = "sans",
      color_intervention = "#2E8B57", color_control = "#CD853F",
      color_period1 = "#4682B4", color_period2 = "#B22222",
      color_seq_ab = "#2E8B57", color_seq_ba = "#8B4513",
      color_form_x = "#4682B4", color_form_y = "#B22222",
      point_alpha = 0.75, point_size = 2.2, line_alpha = 0.55,
      errorbar_width = 0.18, show_ceiling_floor_lines = TRUE,
      significance_style = "none",
      y_axis_score_label = "Rescaled 0-10 score", x_axis_form_label = "Test Form",
      paired_score_plot = list(endpoint_label_size = 3.6),
      power_curve = list(
        show_observed_sample = TRUE,
        observed_sample_label = "Observed sample (n = {n})",
        observed_sample_label_y = 0.075,
        observed_sample_label_size = 3.8,
        observed_sample_x_nudge_fraction = 0.0125,
        observed_sample_x_nudge_min = 0.9,
        legend_nrow = 2, legend_byrow = TRUE,
        legend_title_position = "top", legend_title_hjust = 0.5,
        legend_key_width_lines = 1.35, palette = "Dark2",
        width_in = 7.5, height_in = 5.5, export_pdf = TRUE
      ),
      item_endorsement = list(
        axis_text_x_size = 10.5, width_in = 7.5, height_in = 8.0
      ),
      manuscript_selected = list(
        base_font_size = 15, caption_size = 11,
        condition_hist_width_in = 6.5, condition_hist_height_in = 4.5,
        paired_difference_width_in = 6.5, paired_difference_height_in = 4.5,
        fisher_pitman = list(
          width_in = 7.5, height_in = 7.2,
          observed_label_size = 3.7, observed_label_nudge_pp = 0.35,
          observed_label_y_fraction = 0.94, observed_label_fill = "white",
          observed_label_fill_alpha = 0.90, null_fill = "grey75",
          extreme_fill = "firebrick", observed_color = "#2E8B57",
          reflected_threshold_color = "firebrick", p_value_color = "firebrick",
          p_value_size = 4.0, panel_title_size = 15
        )
      )
    ),
    flow_diagram = list(
      generate = FALSE,
      output_filename = "figure_s1_participant_flow.png",
      width_in = 7.5,
      height_in = 5.0,
      dpi = 300,
      setup_label = paste0(
        "Participants (n = {n}) were randomized to sequence before the session\n",
        "and completed the lecture + AI guidance before Block 1"
      ),
      control_sequence_label = "No-AI first",
      ai_sequence_label = "AI-assisted first",
      period1_control_label = "No-AI study (20 min)",
      period1_ai_label = "AI-assisted study (20 min)",
      control_short_label = "No-AI study (20 min)",
      ai_short_label = "AI-assisted study (20 min)",
      period2_control_label = "No-AI study\n(20 min)",
      period2_ai_label = "AI-assisted study\n(20 min)",
      show_row_labels = FALSE,
      colors = list(
        control_fill = "#FFF3E6", control_border = "#D8893A",
        control_text = "#6B3A10", ai_fill = "#EAF7EF",
        ai_border = "#2E8B57", ai_text = "#174A2D",
        posttest_fill = "#EAF3FC", posttest_border = "#3A70B8",
        posttest_text = "#17376D", panel_fill = "#FAFAFA",
        panel_border = "#D5D5D5", subgroup_fill = "#FFFFFF",
        subgroup_border = "#B8B8B8", dark_text = "#242424",
        arrow = "#AFAFAF"
      ),
      font_sizes = list(
        setup = 3.60, header = 3.05, subgroup = 2.70,
        cell = 3.15, posttest = 2.90, row_label = 2.20
      )
    ),
    tables = list(
      export_csv = TRUE, export_png = TRUE,
      include_titles = FALSE, digits_default = 3, digits_percent = 1,
      canonical = list(
        font_size = 14, row_padding_px = 6, export_expand_px = 8,
        fallback_font_size = 11, fallback_line_height_px = 28
      ),
      manuscript = list(
        font_size = 17, vwidth = 1120, table_width_pct = 94,
        row_padding_px = 6, header_padding_px = 8,
        horizontal_padding_px = 10, export_expand_px = 12,
        source_note_size_offset = 1
      )
    )
  )
}

# Convenience: access nested config values safely
cfg_get <- function(..., default = NULL) {
  val <- tryCatch(
    purrr::pluck(read_config(), ...),
    error = function(e) NULL
  )
  if (is.null(val)) default else val
}

condition_display_map <- function(cfg = read_config()) {
  int_label <- cfg$study$intervention_label %||% "Intervention"
  ctl_label <- cfg$study$control_label %||% "Control"
  labels <- cfg$display_labels %||% list()

  c(
    stats::setNames(
      labels$condition_control %||% labels$condition_control_short %||% ctl_label,
      ctl_label
    ),
    stats::setNames(
      labels$condition_ai %||% labels$condition_ai_short %||% int_label,
      int_label
    )
  )
}

condition_display_label <- function(x, cfg = read_config()) {
  x_chr <- as.character(x)
  map <- condition_display_map(cfg)
  out <- unname(map[x_chr])
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}

sequence_display_map <- function(cfg = read_config()) {
  int_label <- cfg$study$intervention_label %||% "Intervention"
  ctl_label <- cfg$study$control_label %||% "Control"
  labels <- cfg$display_labels %||% list()

  c(
    stats::setNames(
      labels$sequence_control_first %||% paste0(ctl_label, " first"),
      paste0(ctl_label, "-first")
    ),
    stats::setNames(
      labels$sequence_ai_first %||% paste0(int_label, " first"),
      paste0(int_label, "-first")
    )
  )
}

sequence_display_label <- function(x, cfg = read_config()) {
  x_chr <- as.character(x)
  map <- sequence_display_map(cfg)
  out <- unname(map[x_chr])
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}

# =============================================================================
# STUDY NAME & DATA DIRECTORY
# =============================================================================

STUDY_NAME <- Sys.getenv("STUDY_NAME", unset = "")
if (!nzchar(STUDY_NAME)) {
  STUDY_NAME <- cfg_get("study", "name", default = "study")
}

DATA_DIR <- Sys.getenv("STUDY_DATA_PATH", unset = "")
if (!nzchar(DATA_DIR)) {
  DATA_DIR <- p("study_data", STUDY_NAME)
  if (!dir.exists(DATA_DIR)) DATA_DIR <- PROJ_ROOT
} else {
  DATA_DIR <- normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE)
}

# data_file() looks in DATA_DIR first, then project root
data_file <- function(...) {
  in_data <- file.path(DATA_DIR, ...)
  if (file.exists(in_data)) {
    return(in_data)
  }
  in_root <- file.path(PROJ_ROOT, ...)
  if (file.exists(in_root)) {
    return(in_root)
  }
  in_data # Return expected path even if missing (for clean error messages)
}

# =============================================================================
# OUTPUT DIRECTORIES
# =============================================================================
#
# A study run has two human-facing top-level routes:
#   manuscript_selected/  final numbered manuscript + supplement material
#   extra/                everything else (diagnostics, exploratory output,
#                         analysis objects, logs, and conditional PII handling)
#
# Existing analysis scripts still pass their historical subfolder names to
# save_figure()/save_table().  The helpers below classify each output by WHAT IT
# IS rather than by whether it is "primary", "supplementary", or "exploratory".
# This keeps the analysis code stable while making the filesystem sensible.

.output_root <- function() p("outputs", STUDY_NAME)

.figure_output_category <- function(name, legacy_subfolder = NULL) {
  nm <- tolower(tools::file_path_sans_ext(basename(name %||% "")))

  if (grepl("participant_flow|figure_s1|study_design", nm)) {
    return("study_design")
  }
  if (grepl("power", nm)) {
    return("power_and_precision")
  }
  if (grepl("item_response|item_endorsement|alpha_if_deleted|intercorrelation", nm)) {
    return("item_level")
  }
  if (grepl("item_difficulty|item_rest|ability_stratified|suspicious_items|form_x_vs_form_y|score_distributions_by_form|discrimination", nm)) {
    return("psychometrics")
  }
  if (grepl("mixed_model|lme_|normality_qq|residual|model_diagnostic", nm)) {
    return("model_diagnostics")
  }
  if (grepl("time_taken|time_vs_score|completion_time", nm)) {
    return("timing")
  }
  if (grepl("ceiling|floor", nm)) {
    return("ceiling_and_floor")
  }
  if (grepl("period|sequence_period|crossover|carryover|period_specific|by_sequence", nm)) {
    return("sequence_and_period")
  }
  if (grepl("histogram|distribution|ecdf|violin|per_participant|paired_difference|score_delta|full_vs_restricted|sign_test|permutation", nm)) {
    return("score_distributions")
  }
  if (grepl("intervention|ai_assisted|effect_size|subgroup4|condition", nm)) {
    return("condition_effects")
  }
  if (grepl("age|gender|training|sf36|demographic", nm)) {
    return("sample_characteristics")
  }

  legacy <- tolower(legacy_subfolder %||% "")
  switch(legacy,
    psychometrics = "psychometrics",
    item_analysis = "item_level",
    mixed_models = "model_diagnostics",
    period_effects = "sequence_and_period",
    descriptive = "score_distributions",
    primary = "condition_effects",
    exploratory = "other",
    supplementary = "other",
    "other"
  )
}

.table_output_category <- function(name, legacy_subfolder = NULL) {
  nm <- tolower(tools::file_path_sans_ext(basename(name %||% "")))

  if (grepl("participant_flow|study_design", nm)) {
    return("study_design")
  }
  if (grepl("power", nm)) {
    return("power_and_precision")
  }
  if (grepl("item_endorsement|item_analysis|suspicious_items|ability_stratified", nm)) {
    return("item_level")
  }
  if (grepl("reliability|dif_|psychometric|missing_patterns", nm)) {
    return("psychometrics")
  }
  if (grepl("mixed_model|logistic|model_comparison", nm)) {
    return("models")
  }
  if (grepl("time", nm)) {
    return("timing")
  }
  if (grepl("carryover|sequence_period|period_specific|period_condition|subgroup4|sequence_descriptives", nm)) {
    return("sequence_and_period")
  }
  if (grepl("sign|permutation|normality|full_vs_restricted", nm)) {
    return("robustness_checks")
  }
  if (grepl("primary|overall_results|main_results|effect_size|condition_descriptives", nm)) {
    return("condition_effects")
  }
  if (grepl("descriptive|cell_means", nm)) {
    return("descriptive_scores")
  }
  if (grepl("sample_characteristics|sf36|demographic", nm)) {
    return("sample_characteristics")
  }

  legacy <- tolower(legacy_subfolder %||% "")
  switch(legacy,
    psychometrics = "psychometrics",
    item_analysis = "item_level",
    mixed_models = "models",
    period_effects = "sequence_and_period",
    descriptive = "descriptive_scores",
    primary = "condition_effects",
    exploratory = "other",
    supplementary = "robustness_checks",
    "other"
  )
}

out_path <- function(...) {
  parts <- as.character(unlist(list(...), use.names = FALSE))
  root <- .output_root()
  if (!length(parts)) {
    return(root)
  }

  head <- parts[[1L]]
  rest <- parts[-1L]

  # Manuscript-selected is intentionally the only publication-role directory.
  if (identical(head, "manuscript_selected")) {
    return(do.call(file.path, c(list(root, "manuscript_selected"), as.list(rest))))
  }

  # Historical figure/table paths are transparently redirected into extra/ and
  # semantically classified by filename.
  if (head %in% c("figures", "figures_comparison")) {
    base <- if (identical(head, "figures_comparison")) {
      file.path(root, "extra", "comparison_figures")
    } else {
      file.path(root, "extra", "figures")
    }

    if (!length(rest)) {
      return(base)
    }
    filename <- utils::tail(rest, 1L)
    legacy <- if (length(rest) >= 2L) rest[[1L]] else NULL
    if (nzchar(tools::file_ext(filename))) {
      cat_dir <- .figure_output_category(filename, legacy)
      return(file.path(base, cat_dir, filename))
    }
    return(base)
  }

  if (head %in% c("tables", "tables_png")) {
    base <- file.path(root, "extra", "tables")
    if (!length(rest)) {
      return(base)
    }
    filename <- utils::tail(rest, 1L)
    legacy <- if (length(rest) >= 2L) rest[[1L]] else NULL
    if (nzchar(tools::file_ext(filename))) {
      cat_dir <- .table_output_category(filename, legacy)
      return(file.path(base, cat_dir, filename))
    }
    return(base)
  }

  if (identical(head, "rds")) {
    return(do.call(file.path, c(list(root, "extra", "analysis_objects"), as.list(rest))))
  }
  if (identical(head, "logs")) {
    return(do.call(file.path, c(list(root, "extra", "logs"), as.list(rest))))
  }
  if (identical(head, "run_provenance")) {
    return(do.call(file.path, c(list(root, "extra", "logs"), as.list(rest))))
  }
  if (identical(head, "InternalUse")) {
    return(do.call(file.path, c(list(root, "extra", "data", "identified"), as.list(rest))))
  }

  # Unknown legacy outputs are still kept under extra rather than creating new
  # top-level routes beside manuscript_selected.
  do.call(file.path, c(list(root, "extra"), as.list(parts)))
}

.output_relative_path <- function(path) {
  root <- normalizePath(.output_root(), winslash = "/", mustWork = FALSE)
  target <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(target, prefix)) {
    return(substring(target, nchar(prefix) + 1L))
  }
  target
}

initialize_output_tree <- function(clean = FALSE) {
  root <- .output_root()
  if (isTRUE(clean) && dir.exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "extra", "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "extra", "analysis_objects"), recursive = TRUE, showWarnings = FALSE)
  invisible(root)
}

# Directly executed modules still get a usable output tree. run_all.R calls
# initialize_output_tree(clean = TRUE) once before the first module.
initialize_output_tree(clean = FALSE)

#' Write the exact effective configuration after environment overrides.
#'
#' The snapshot deliberately excludes machine-specific paths and user details.
#' It can therefore be copied into a reviewer reproducibility package as a
#' directly runnable study configuration.
write_effective_config <- function() {
  effective <- read_config()
  if (is.null(effective$study)) effective$study <- list()
  effective$study$name <- STUDY_NAME

  if (is.null(effective$item_exclusions)) {
    effective$item_exclusions <- list(x = character(), y = character())
  }
  effective$item_exclusions$x <- as.character(unlist(
    effective$item_exclusions$x %||% character(),
    use.names = FALSE
  ))
  effective$item_exclusions$y <- as.character(unlist(
    effective$item_exclusions$y %||% character(),
    use.names = FALSE
  ))
  effective$runtime <- list(
    analysis_modules = Sys.getenv("ANALYSIS_MODULES", unset = "all"),
    reuse_data = identical(Sys.getenv("REUSE_DATA", unset = "0"), "1"),
    item_exclusions_override_applied = nzchar(
      Sys.getenv("ITEM_EXCLUSIONS", unset = "")
    )
  )

  values <- as.character(unlist(effective, recursive = TRUE, use.names = FALSE))
  forbidden <- c(
    "^[A-Za-z]:[/\\\\]", paste0("^/", "Users/"), "^/home/[^/]+/",
    "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,}"
  )
  if (any(vapply(forbidden, function(pattern) {
    any(grepl(pattern, values, perl = TRUE, ignore.case = TRUE))
  }, logical(1)))) {
    stop("Effective configuration contains a machine-specific path or email address.")
  }

  target <- out_path("logs", "effective_config.yml")
  yaml::write_yaml(effective, target, fileEncoding = "UTF-8")
  log_line("Effective config: extra/logs/effective_config.yml")
  invisible(target)
}

# =============================================================================
# LOGGING
# =============================================================================

if (!exists(".log_file_path", envir = .GlobalEnv, inherits = FALSE)) .log_file_path <- NULL
if (!exists(".log_archive_path", envir = .GlobalEnv, inherits = FALSE)) .log_archive_path <- NULL
if (!exists(".log_con", envir = .GlobalEnv, inherits = FALSE)) .log_con <- NULL
if (!exists(".log_output_depth", envir = .GlobalEnv, inherits = FALSE)) .log_output_depth <- NULL

log_start <- function() {
  # Do not open a second transcript if 00_setup.R is sourced again downstream.
  if (!is.null(.log_con)) {
    return(invisible(list(
      path = .log_file_path,
      archive = .log_archive_path,
      con = .log_con
    )))
  }

  dir.create(out_path("logs"), recursive = TRUE, showWarnings = FALSE)

  # Write directly to latest_run.log while the analysis is running. This makes
  # the partial transcript available even if R exits unexpectedly before
  # log_stop() can create the timestamped archival copy.
  latest_path <- out_path("logs", "latest_run.log")
  archive_path <- out_path(
    "logs",
    paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_run.log")
  )

  con <- file(latest_path, open = "wt", encoding = "UTF-8")

  .log_file_path <<- latest_path
  .log_archive_path <<- archive_path
  .log_con <<- con
  .log_output_depth <<- sink.number(type = "output")

  # Tee standard output to both the terminal and the run log. R supports
  # split = TRUE only for the normal output sink, not for type = "message".
  # The master runner converts module warnings/messages to cat() output via
  # withCallingHandlers(), so they remain visible in the terminal and are also
  # captured here without redirecting R's stderr/message connection.
  sink(con, split = TRUE, type = "output")

  log_h1(paste("PIPELINE RUN —", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("  Crossover Study Analysis Pipeline\n", strrep("-", 70), "\n", sep = "")
  log_line("Study      : ", STUDY_NAME)
  log_line("Config     : ", normalizePath(.config_path, winslash = "/", mustWork = FALSE))
  log_line("Modules    : ", Sys.getenv("ANALYSIS_MODULES", "all"))
  log_line("Mode       : ", Sys.getenv("PIPELINE_MODE", unset = "standard"))
  log_line("R version  : ", R.version$version.string)
  log_line("Platform   : ", R.version$platform)
  log_line("OS         : ", Sys.info()[["sysname"]])

  invisible(list(
    path = latest_path,
    archive = archive_path,
    con = con
  ))
}

log_stop <- function() {
  if (is.null(.log_con)) {
    return(invisible(NULL))
  }

  log_h1("END OF RUN")

  # Return output sinking to the depth that existed before log_start(). In the
  # normal pipeline this closes exactly one sink. The loop also protects us if
  # a downstream routine accidentally leaves an additional output sink open.
  target_depth <- .log_output_depth %||% 0L
  while (sink.number(type = "output") > target_depth) {
    try(sink(type = "output"), silent = TRUE)
  }

  try(flush(.log_con), silent = TRUE)
  try(close(.log_con), silent = TRUE)

  # Preserve a timestamped archival transcript after the live latest_run.log
  # has been closed. latest_run.log remains the obvious pointer for the BAT UI.
  if (
    !is.null(.log_file_path) &&
      file.exists(.log_file_path) &&
      !is.null(.log_archive_path)
  ) {
    try(
      file.copy(.log_file_path, .log_archive_path, overwrite = TRUE),
      silent = TRUE
    )
  }

  .log_con <<- NULL
  .log_output_depth <<- NULL

  invisible(NULL)
}

log_line <- function(...) cat("[INFO]  ", paste0(...), "\n", sep = "")
log_warn <- function(...) cat("[WARN]  ", paste0(...), "\n", sep = "")
log_check <- function(...) cat("[CHECK] ", paste0(...), "\n", sep = "")
log_h1 <- function(title) cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
log_h2 <- function(title) cat("\n--- ", title, " ---\n", sep = "")

#' Log a single calculation step with formula, inputs, and result.
#' Use this for every non-trivial number that ends up in a table or figure.
log_calc <- function(label, formula = NULL, inputs = NULL, result = NULL) {
  cat("[CALC]  ", label, "\n", sep = "")
  if (!is.null(formula)) cat("         formula   : ", formula, "\n", sep = "")
  if (!is.null(inputs) && length(inputs) > 0) {
    nms <- if (!is.null(names(inputs))) names(inputs) else seq_along(inputs)
    for (i in seq_along(inputs)) {
      cat("         ", formatC(as.character(nms[i]), width = -14, flag = "-"),
        " : ", paste(inputs[[i]], collapse = ", "), "\n",
        sep = ""
      )
    }
  }
  if (!is.null(result)) cat("         result    : ", result, "\n", sep = "")
}

#' Log a full statistical result block — every named value printed.
#' Pass either named arguments or a single named list.
log_stat <- function(label, ...) {
  vals <- list(...)
  if (length(vals) == 1L && is.list(vals[[1L]]) && is.null(names(vals))) {
    vals <- vals[[1L]]
  }
  cat("\n[STAT]  ", label, "\n", sep = "")
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (is.null(v) || is.list(v)) next
    if (is.numeric(v) && length(v) == 1L) v <- round(v, 6L)
    cat("        ", formatC(nm, width = -22L, flag = "-"), " = ",
      paste(head(v, 10L), collapse = ", "), "\n",
      sep = ""
    )
  }
}

#' Convenience wrapper: log the complete output of paired_summary().
#' Call this immediately after every paired_summary() result.
log_paired_result <- function(res) {
  if (is.null(res)) {
    return(invisible(NULL))
  }
  log_stat(
    res$label,
    n                   = res$n,
    mean_a              = round(res$mean_a, 4L),
    sd_a                = round(res$sd_a, 4L),
    mean_b              = round(res$mean_b, 4L),
    sd_b                = round(res$sd_b, 4L),
    mean_difference     = round(res$mean_diff, 4L),
    sd_of_differences   = round(res$sd_diff, 4L),
    `95pct CI`          = fmt_ci(res$ci_lo, res$ci_hi),
    `Cohen's dz`        = round(res$dz, 4L),
    `t-statistic`       = round(res$t, 4L),
    df                  = res$n - 1L,
    `p-value`           = sub("^= ", "", fmt_p(res$p)),
    significant_a0.05   = if (!is.na(res$p)) res$p < 0.05 else NA
  )
}

# =============================================================================
# NULL COALESCING & FORMATTING HELPERS
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

fmt_num <- function(x, d = 2) round(x, d)
fmt_pct <- function(x, d = 1) paste0(round(x * 100, d), "%")
fmt_p <- function(p) {
  if (is.na(p)) {
    return("NA")
  }
  if (p < 0.001) {
    return("< 0.001")
  }
  if (p < 0.01) {
    return(sprintf("= 0.%03.0f", p * 1000))
  }
  sprintf("= %.3f", p)
}
fmt_ci <- function(lo, hi, d = 2) {
  sprintf("[%s, %s]", fmt_num(lo, d), fmt_num(hi, d))
}

# =============================================================================
# SCORE SCALE HELPERS
# =============================================================================

score_metric_label <- function(score_meta = NULL, scale_to = NULL) {
  if (is.null(scale_to)) {
    scale_to <- suppressWarnings(as.numeric(score_meta$scale_to %||% NA_real_))
  }
  if (is.na(scale_to)) scale_to <- 10

  if (isTRUE(all.equal(scale_to, 100))) {
    "Percent correct"
  } else if (isTRUE(all.equal(scale_to, 10))) {
    "Rescaled 0-10 score"
  } else {
    paste0("Rescaled 0-", format(scale_to, trim = TRUE), " score")
  }
}

score_metric_note <- function(score_meta = NULL) {
  if (is.null(score_meta) || !isTRUE(score_meta$any_unequal_denominators)) {
    return(NULL)
  }
  paste0(
    "Scores are common-scale equivalents computed as correct_included / ",
    "n_items_included, then rescaled; unrescaled item-count values are retained ",
    "only for audit and item-level diagnostics."
  )
}

validate_score_columns <- function(cols, score_meta = NULL,
                                   context = "score analysis") {
  cols <- as.character(cols %||% character(0))
  if (length(cols) == 0 || is.null(score_meta) ||
    !isTRUE(score_meta$any_unequal_denominators)) {
    return(invisible(TRUE))
  }

  raw_like <- grep(
    "(^|_)(raw|total_correct|correct_included)(_|$)|(^|_)total$",
    cols,
    value = TRUE,
    ignore.case = TRUE
  )
  if (length(raw_like) > 0) {
    stop(
      "Unsafe score columns in ", context, ": ",
      paste(raw_like, collapse = ", "),
      "\nForm X and Form Y have unequal included-item counts in at least one ",
      "scoring set. Use score_prop, score_percent, score_10_equiv, or the ",
      "common-scale *_score_full / *_score_restricted columns; do not analyze ",
      "unrescaled item-count columns."
    )
  }
  invisible(TRUE)
}

# =============================================================================
# STATISTICAL HELPERS
# =============================================================================

#' Paired summary for two numeric vectors
#' Returns: n, mean/sd of each, mean difference, 95% CI, Cohen's dz, t, p
paired_summary <- function(a, b, label = "Comparison", ci = 0.95) {
  stopifnot(length(a) == length(b))
  complete <- !is.na(a) & !is.na(b)
  a <- a[complete]
  b <- b[complete]
  n <- length(a)
  d <- a - b
  mean_d <- mean(d)
  sd_d <- sd(d)
  se_d <- sd_d / sqrt(n)
  t_val <- mean_d / se_d
  p_val <- 2 * pt(-abs(t_val), df = n - 1)
  alpha <- 1 - ci
  t_crit <- qt(1 - alpha / 2, df = n - 1)
  ci_lo <- mean_d - t_crit * se_d
  ci_hi <- mean_d + t_crit * se_d
  dz <- mean_d / sd_d
  list(
    label = label,
    n = n,
    mean_a = mean(a), sd_a = sd(a),
    mean_b = mean(b), sd_b = sd(b),
    mean_diff = mean_d, sd_diff = sd_d,
    ci_lo = ci_lo, ci_hi = ci_hi,
    dz = dz,
    t = t_val,
    p = p_val
  )
}

#' Cohen's dz (within-subjects effect size)
cohen_dz <- function(a, b) {
  d <- a - b
  mean(d, na.rm = TRUE) / sd(d, na.rm = TRUE)
}

#' Proportion correct and 95% Wilson CI for a binary vector
prop_correct <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  k <- sum(x)
  p <- k / n
  z <- qnorm(0.975)
  lo <- (p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
  hi <- (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
  list(p = p, lo = lo, hi = hi, n = n)
}

#' Point-biserial correlation between a binary item and a total score
point_biserial <- function(item, total) {
  # Exclude the item from the total if summed (use item-rest)
  suppressWarnings(cor(item, total, use = "complete.obs"))
}

#' KR-20 (Cronbach's alpha for binary items)
kr20 <- function(item_matrix) {
  item_matrix <- as.matrix(item_matrix)
  k <- ncol(item_matrix)
  n <- nrow(item_matrix)
  p <- colMeans(item_matrix, na.rm = TRUE)
  q <- 1 - p
  var_total <- var(rowSums(item_matrix, na.rm = TRUE), na.rm = TRUE)
  (k / (k - 1)) * (1 - sum(p * q) / var_total)
}

# =============================================================================
# DATA HELPERS
# =============================================================================

#' Standardize the participant ID column to "participant" (integer)
standardize_participant_id <- function(df, dataset_name = "") {
  cfg <- read_config()
  id_col <- tolower(cfg$columns$participant_id %||% "participant_id")

  # Candidate names in preference order
  candidates <- c(
    id_col, "participant_id", "participant", "id",
    "subject_id", "subject"
  )

  found <- intersect(candidates, tolower(names(df)))
  if (length(found) == 0) {
    stop(
      "Cannot find participant ID column in ", dataset_name,
      "\nFound columns: ", paste(names(df), collapse = ", "),
      "\nExpected one of: ", paste(candidates, collapse = ", ")
    )
  }

  # Use the original-case column name that maps to the found lowercase match
  orig_name <- names(df)[tolower(names(df)) == found[1]][1]

  df |>
    dplyr::rename(participant = dplyr::all_of(orig_name)) |>
    dplyr::mutate(participant = as.integer(.data$participant))
}

#' Standardize intervention/order column names using config mapping
#' Returns df with columns: intervention_order, form_x_order, form_y_order
standardize_assignment_cols <- function(df) {
  cfg <- read_config()

  # Build mapping: canonical name -> list of accepted aliases
  aliases <- list(
    intervention_order = c(
      tolower(cfg$columns$intervention_order %||% "intervention_order"),
      "intervention_order", "ai_order", "treatment_order", "condition_order"
    ),
    control_order = c(
      tolower(cfg$columns$control_order %||% "control_order"),
      "control_order", "ctl_order"
    ),
    form_x_order = c(
      tolower(cfg$columns$form_x_order %||% "x_order"),
      "form_x_order", "x_order", "formx_order"
    ),
    form_y_order = c(
      tolower(cfg$columns$form_y_order %||% "y_order"),
      "form_y_order", "y_order", "formy_order"
    )
  )

  df_lower <- df
  names(df_lower) <- tolower(names(df))

  for (canon in names(aliases)) {
    if (canon %in% names(df_lower)) next # Already correctly named
    found <- intersect(aliases[[canon]], names(df_lower))
    if (length(found) > 0) {
      df_lower <- dplyr::rename(df_lower, !!canon := dplyr::all_of(found[1]))
    } else {
      stop(
        "Cannot find column for '", canon,
        "' in assignment.csv\nTried: ", paste(aliases[[canon]], collapse = ", "),
        "\nFound: ", paste(names(df_lower), collapse = ", ")
      )
    }
  }

  names(df_lower) <- make.names(names(df_lower))
  df_lower
}

#' Convert "1st" / "2nd" order strings to integer periods 1 / 2
order_to_period <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("1st", "first", "1", "period1", "period 1") ~ 1L,
    x %in% c("2nd", "second", "2", "period2", "period 2") ~ 2L,
    TRUE ~ NA_integer_
  )
}

#' Return question columns for a given prefix in numerically sorted order
get_question_cols_ordered <- function(df, prefix) {
  pat <- paste0("^", tolower(prefix), "\\d+$")
  cols <- grep(pat, tolower(names(df)), value = FALSE)
  orig_names <- names(df)[cols]
  nums <- as.integer(gsub(paste0("^", prefix), "", tolower(orig_names),
    ignore.case = TRUE
  ))
  orig_names[order(nums)]
}

#' Parse "X min Y sec" or "HH:MM:SS" strings to total seconds (numeric)
parse_time_taken <- function(x) {
  x <- as.character(x)
  sapply(x, function(s) {
    if (is.na(s) || s == "") {
      return(NA_real_)
    }
    # "X min Y sec" pattern
    m <- regmatches(s, regexpr("(\\d+)\\s*min.*?(\\d+)\\s*sec", s, perl = TRUE))
    if (length(m) > 0 && nzchar(m)) {
      parts <- as.numeric(regmatches(m, gregexpr("\\d+", m))[[1]])
      return(parts[1] * 60 + parts[2])
    }
    # "MM:SS" or "HH:MM:SS"
    m2 <- regmatches(s, regexpr("\\d+:\\d+(:\\d+)?", s))
    if (length(m2) > 0 && nzchar(m2)) {
      parts <- as.numeric(strsplit(m2, ":")[[1]])
      if (length(parts) == 2) {
        return(parts[1] * 60 + parts[2])
      }
      if (length(parts) == 3) {
        return(parts[1] * 3600 + parts[2] * 60 + parts[3])
      }
    }
    # Bare number (assume seconds)
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n)) {
      return(n)
    }
    NA_real_
  }, USE.NAMES = FALSE)
}

# =============================================================================
# PUBLICATION THEME
# =============================================================================

#' Clean publication theme — no title, no subtitle, minimal chrome
theme_clean <- function(base_size = NULL, base_family = NULL) {
  cfg <- read_config()
  bs <- base_size %||% as.numeric(cfg$figures$base_font_size %||% 15)
  bf <- base_family %||% as.character(cfg$figures$font_family %||% "sans")

  theme_minimal(base_size = bs, base_family = bf) +
    theme(
      # Axes
      axis.title = element_text(size = rel(1.05), colour = "grey20"),
      axis.text = element_text(size = rel(0.90), colour = "grey30"),
      axis.line = element_line(colour = "grey70", linewidth = 0.4),
      axis.ticks = element_line(colour = "grey70", linewidth = 0.3),
      # Grid
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      # Legend
      legend.title = element_text(size = rel(0.95), colour = "grey20"),
      legend.text = element_text(size = rel(0.88), colour = "grey30"),
      legend.position = "bottom",
      legend.key.size = unit(0.9, "lines"),
      # Facets
      strip.text = element_text(
        size = rel(0.95), colour = "grey20",
        margin = margin(4, 4, 4, 4)
      ),
      strip.background = element_rect(fill = "grey96", colour = NA),
      # Remove title/subtitle/caption
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      # Margins
      plot.margin = margin(10, 12, 10, 12)
    )
}

score_zoom_limits_percent <- function(scores, pad = 5, step = 10, min_span = 40) {
  scores <- scores[is.finite(scores)]

  if (length(scores) == 0) {
    return(c(0, 100))
  }

  lo <- floor((min(scores) - pad) / step) * step
  hi <- ceiling((max(scores) + pad) / step) * step

  lo <- max(0, lo)
  hi <- min(100, hi)

  if ((hi - lo) < min_span) {
    mid <- mean(range(scores))
    lo <- floor((mid - min_span / 2) / step) * step
    hi <- ceiling((mid + min_span / 2) / step) * step
    lo <- max(0, lo)
    hi <- min(100, hi)
  }

  c(lo, hi)
}

score_zoom_limits_common <- function(scores, scale_to = 10, pad = 5,
                                     step = 10, min_span = 40) {
  scale_to <- as.numeric(scale_to %||% 10)
  if (!is.finite(scale_to) || scale_to <= 0) scale_to <- 10

  score_zoom_limits_percent(
    scores / scale_to * 100,
    pad = pad,
    step = step,
    min_span = min_span
  ) / 100 * scale_to
}

#' Config-aware condition colors
condition_colors <- function() {
  cfg <- read_config()
  c(
    Intervention = cfg$figures$color_intervention %||% "#2E8B57",
    Control      = cfg$figures$color_control %||% "#CD853F"
  )
}

#' Config-aware period colors
period_colors <- function() {
  cfg <- read_config()
  c(
    "Period 1" = cfg$figures$color_period1 %||% "#4682B4",
    "Period 2" = cfg$figures$color_period2 %||% "#B22222"
  )
}

# =============================================================================
# SAVE HELPERS
# =============================================================================

#' Save a ggplot with a descriptive filename
#' @param plot   ggplot object
#' @param name   base file name (no extension)
#' @param subfolder  one of the output subfolders (e.g., "primary")
#' @param width  width in inches (NULL = config default)
#' @param height height in inches (NULL = config default)
#' @param formats one or more of "png" and "pdf" (default: "png")
save_figure <- function(plot, name, subfolder = "supplementary",
                        width = NULL, height = NULL, formats = "png") {
  cfg <- read_config()

  # Reviewer reproduction only needs the three canonical source figures that are
  # promoted into manuscript_selected by R/10_manuscript_selected.R.  All other
  # figure code may still execute because it can share intermediate objects, but
  # non-selected files are not written to disk.
  .reviewer_mode <- identical(
    tolower(trimws(Sys.getenv("PIPELINE_MODE", unset = "standard"))),
    "reviewer"
  )
  .reviewer_source_figures <- c(
    "ai_assisted_vs_noai_paired",
    "post_hoc_power_curve",
    "item_endorsement_by_sequence"
  )
  if (.reviewer_mode && !(name %in% .reviewer_source_figures)) {
    return(invisible(character()))
  }

  dpi <- as.numeric(cfg$figures$dpi %||% 300)
  w <- width %||% as.numeric(cfg$figures$width_in %||% 7.5)
  h <- height %||% as.numeric(cfg$figures$height_in %||% 5.0)

  formats <- unique(tolower(as.character(formats)))
  unsupported <- setdiff(formats, c("png", "pdf"))
  if (!length(formats) || length(unsupported)) {
    stop(
      "save_figure formats must contain only 'png' and/or 'pdf'.",
      if (length(unsupported)) paste0(" Unsupported: ", paste(unsupported, collapse = ", ")) else ""
    )
  }

  # When FIGURES_ROOT_SUFFIX is set (e.g. "figures_comparison"), write there
  # instead of the default "figures/" dir.  Used by the comparison script.
  .figs_root <- Sys.getenv("FIGURES_ROOT_SUFFIX", unset = "figures")
  paths <- character()

  for (format in formats) {
    path <- out_path(.figs_root, subfolder, paste0(name, ".", format))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

    if (identical(format, "png")) {
      # Use ragg if available for better anti-aliasing.
      if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_png(path, width = w, height = h, units = "in", res = dpi)
        suppressWarnings(print(plot))
        dev.off()
      } else {
        suppressWarnings(
          ggplot2::ggsave(path,
            plot = plot, width = w, height = h,
            dpi = dpi, bg = "white"
          )
        )
      }
    } else {
      .pdf_device <- if (isTRUE(capabilities("cairo"))) {
        grDevices::cairo_pdf
      } else {
        grDevices::pdf
      }
      suppressWarnings(
        ggplot2::ggsave(path,
          plot = plot, width = w, height = h,
          device = .pdf_device, bg = "white"
        )
      )
    }

    paths <- c(paths, path)
    sz_kb <- tryCatch(round(file.size(path) / 1024, 1L), error = function(e) NA_real_)
    log_line("Figure saved : ", .output_relative_path(path))
    log_line(
      "             : ", w, " x ", h, " in  |  ",
      if (identical(format, "png")) paste0(dpi, " dpi  |  ") else "vector PDF  |  ",
      sz_kb, " KB"
    )
  }

  invisible(paths)
}

ensure_gt_png_export <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  cache_dir <- file.path(PROJ_ROOT, ".r-cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (!nzchar(Sys.getenv("R_USER_CACHE_DIR", unset = ""))) {
    Sys.setenv(R_USER_CACHE_DIR = cache_dir)
  }
  if (!nzchar(Sys.getenv("XDG_CACHE_HOME", unset = ""))) {
    Sys.setenv(XDG_CACHE_HOME = cache_dir)
  }

  if (!isTRUE(getOption("crossover.gt_png_export_ready", FALSE))) {
    chrome_args <- unique(c(
      "--headless=new",
      "--disable-gpu",
      "--force-color-profile=srgb",
      "--disable-extensions",
      "--mute-audio",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--remote-allow-origins=*",
      "--disable-background-networking",
      "--disable-sync"
    ))
    chromote::set_default_chromote_object(
      chromote::Chromote$new(
        browser = chromote::Chrome$new(args = chrome_args)
      )
    )
    options(crossover.gt_png_export_ready = TRUE)
  }

  invisible(TRUE)
}

save_table_png_fallback <- function(df, png_path, caption = NULL, notes = NULL) {
  lines <- c(
    if (!is.null(caption) && nzchar(caption)) caption else character(0),
    if (!is.null(caption) && nzchar(caption)) "" else character(0),
    utils::capture.output(print(df, row.names = FALSE, right = FALSE)),
    if (!is.null(notes) && length(notes) > 0) c("", "Notes:", paste0("- ", notes)) else character(0)
  )
  lines <- ifelse(nchar(lines) == 0, " ", lines)
  max_chars <- max(nchar(lines), na.rm = TRUE)
  width_px <- max(900L, min(5000L, as.integer(max_chars * 8.5 + 80)))
  cfg <- read_config()
  fallback_line_height <- as.numeric(
    cfg$tables$canonical$fallback_line_height_px %||% 28
  )
  fallback_font_size <- as.numeric(
    cfg$tables$canonical$fallback_font_size %||% 11
  )
  height_px <- max(
    500L,
    min(12000L, as.integer(length(lines) * fallback_line_height_px + 80))
  )

  grDevices::png(png_path, width = width_px, height = height_px, res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.text(
    paste(lines, collapse = "\n"),
    x = grid::unit(0.03, "npc"),
    y = grid::unit(0.97, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontfamily = "mono", fontsize = fallback_font_size, col = "grey15")
  )
  invisible(png_path)
}

#' Save a data frame as CSV and optionally PNG (via gt)
#' @param df        data frame
#' @param name      base file name
#' @param subfolder output subfolder
#' @param caption   optional caption string (used as PNG title when include_titles is true)
#' @param digits    rounding for numeric columns
#' @param notes     optional character vector of note lines appended to the CSV after the data
save_table <- function(df, name, subfolder = "supplementary",
                       caption = NULL, digits = NULL, notes = NULL) {
  cfg <- read_config()

  # Reviewer mode builds the numbered manuscript tables directly in
  # R/10_manuscript_selected.R.  Generic psychometric/diagnostic table exports
  # are therefore intentionally suppressed from the reviewer reproduction.
  .reviewer_mode <- identical(
    tolower(trimws(Sys.getenv("PIPELINE_MODE", unset = "standard"))),
    "reviewer"
  )
  if (.reviewer_mode) {
    return(invisible(NULL))
  }

  digs <- digits %||% as.integer(cfg$tables$digits_default %||% 3)

  # Round numeric columns
  df_out <- df |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digs)))

  # ---- CSV ----
  csv_path <- out_path("tables", subfolder, paste0(name, ".csv"))
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df_out, csv_path, na = "")
  # Append human-readable notes block after the data rows
  if (!is.null(notes) && length(notes) > 0) {
    note_txt <- c("", "# --- Notes ---", paste0("# ", notes))
    cat(note_txt, file = csv_path, sep = "\n", append = TRUE)
  }
  log_line("Table CSV   : ", .output_relative_path(csv_path))
  log_line("            : ", nrow(df_out), " rows x ", ncol(df_out), " cols")
  log_line("            : columns: ", paste(names(df_out), collapse = ", "))
  if (!is.null(notes)) log_line("            : ", length(notes), " note line(s) appended")

  # ---- PNG via gt, with grid fallback ----
  # Defaults to TRUE when cfg$tables$export_png is unset; set to false to disable.
  if (!isFALSE(cfg$tables$export_png)) {
    png_path <- out_path("tables_png", subfolder, paste0(name, ".png"))
    dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
    .png_ok <- FALSE

    if (requireNamespace("gt", quietly = TRUE)) {
      gt_tbl <- gt::gt(df_out)

      if (!is.null(caption) && isTRUE(cfg$tables$include_titles)) {
        gt_tbl <- gt_tbl |> gt::tab_header(title = caption)
      }

      .canonical_table_cfg <- cfg$tables$canonical %||% list()
      gt_tbl <- gt_tbl |>
        gt::tab_options(
          table.font.size = as.numeric(
            .canonical_table_cfg$font_size %||% 14
          ),
          column_labels.font.weight = "bold",
          table.border.top.color = "grey30",
          table.border.bottom.color = "grey30",
          column_labels.border.bottom.color = "grey50",
          data_row.padding = gt::px(
            as.numeric(.canonical_table_cfg$row_padding_px %||% 6)
          )
        ) |>
        gt::opt_table_lines("none") |>
        gt::opt_row_striping()

      if (!is.null(notes) && length(notes) > 0) {
        for (.note in notes) {
          gt_tbl <- gt_tbl |> gt::tab_source_note(gt::md(.note))
        }
      }

      .png_ok <- tryCatch(
        {
          ensure_gt_png_export()
          gt::gtsave(
            gt_tbl,
            png_path,
            expand = as.numeric(
              .canonical_table_cfg$export_expand_px %||% 8
            )
          )
          TRUE
        },
        error = function(e) {
          log_warn("gt PNG export failed for '", name, "': ", conditionMessage(e))
          FALSE
        }
      )
    }

    if (!.png_ok) {
      tryCatch(
        {
          save_table_png_fallback(df_out, png_path, caption = caption, notes = notes)
          .png_ok <- TRUE
          log_warn("Used fallback PNG table renderer for '", name, "'.")
        },
        error = function(e) {
          log_warn("Fallback PNG export failed for '", name, "': ", conditionMessage(e))
        }
      )
    }

    if (.png_ok) {
      log_line("Table PNG   : ", .output_relative_path(png_path))
    } else {
      log_warn("Table PNG not created for '", name, "'.")
    }
  }

  invisible(csv_path)
}

#' Save an R object as RDS in outputs/rds/
save_rds <- function(obj, name) {
  path <- out_path("rds", paste0(name, ".rds"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, path)
  log_line("RDS saved   : ", .output_relative_path(path))
  invisible(path)
}

#' Load an RDS from outputs/rds/
load_rds <- function(name) {
  path <- out_path("rds", paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop(
      "RDS not found: ", path,
      "\nRun earlier pipeline steps first."
    )
  }
  readRDS(path)
}

# =============================================================================
# MODULE SELECTOR
# (Checks ANALYSIS_MODULES env var set by the .bat)
# =============================================================================

ANALYSIS_MODULES <- Sys.getenv("ANALYSIS_MODULES", unset = "all")

module_enabled <- function(name) {
  mods <- tolower(trimws(strsplit(ANALYSIS_MODULES, ",")[[1]]))
  "all" %in% mods || name %in% mods
}

# =============================================================================
# JSON SESSION LOG
# Legacy structured-session helper; if called manually, writes under extra/logs/.
# Called by run_all.R after all modules complete.
# .sess is initialised ONCE on first load and preserved across module sourcing.
# =============================================================================

if (!exists(".sess") || !is.environment(.sess)) {
  .sess <- new.env(parent = emptyenv())
  .sess$started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  .sess$study_name <- NULL
  .sess$r_version <- R.version$version.string
  .sess$platform <- R.version$platform
  .sess$os <- paste(Sys.info()[c("sysname", "release")], collapse = " ")
  .sess$modules <- list()
  .sess$warnings_list <- character(0)
  .sess$errors_list <- character(0)
  .sess$flags <- list() # psychometric / data-quality flags
  .sess$output_counts <- list()
  .sess$results_summary <- list() # computed results for audit trail
}

#' Record a completed module step into the session log
session_record_module <- function(module_id, status, elapsed_s) {
  .sess$modules[[module_id]] <<- list(
    status = status,
    elapsed_s = round(elapsed_s, 2)
  )
}

#' Record a warning in the structured session log
session_record_warning <- function(msg) {
  .sess$warnings_list <<- c(
    .sess$warnings_list,
    paste0(format(Sys.time(), "[%H:%M:%S] "), msg)
  )
}

#' Record an error in the structured session log
session_record_error <- function(msg) {
  .sess$errors_list <<- c(
    .sess$errors_list,
    paste0(format(Sys.time(), "[%H:%M:%S] "), msg)
  )
}

#' Record a psychometric/data-quality flag
session_record_flag <- function(item, form, category, detail = NULL) {
  key <- paste0(form, "::", item)
  .sess$flags[[key]] <<- list(
    item = item, form = form,
    category = category, detail = detail %||% "",
    time = format(Sys.time(), "%H:%M:%S")
  )
}

#' Record a key computed result for inclusion in the SUMMARY.txt
#' @param key   short label (e.g. "intervention_effect_diff")
#' @param value character string with the formatted value(s)
session_record_result <- function(key, value) {
  if (!exists("results_summary", envir = .sess, inherits = FALSE)) {
    .sess$results_summary <- list()
  }
  .sess$results_summary[[key]] <- as.character(value)
}

#' Write the structured JSON session summary to logs/<timestamp>_session.json
#' Called once from run_all.R at pipeline end.
write_session_json <- function(n_ok = 0L, n_err = 0L,
                               n_skip = 0L, total_secs = 0) {
  tryCatch(
    {
      .sess$completed_at <<- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      .sess$study_name <<- STUDY_NAME
      .sess$data_dir <<- DATA_DIR
      .sess$config_path <<- .config_path
      .sess$modules_env <<- Sys.getenv("ANALYSIS_MODULES", "all")
      .sess$n_ok <<- n_ok
      .sess$n_errors <<- n_err
      .sess$n_skipped <<- n_skip
      .sess$total_secs <<- round(total_secs, 1)
      .sess$packages <<- tryCatch(sort(names(sessionInfo()$otherPkgs)),
        error = function(e) character(0)
      )
      out_dir <- out_path()
      .sess$output_counts <<- list(
        figures_png = length(list.files(out_path("figures"),
          pattern = "\\.png$", recursive = TRUE
        )),
        tables_csv = length(list.files(out_path("tables"),
          pattern = "\\.csv$", recursive = TRUE
        )),
        tables_png = length(list.files(out_path("tables_png"),
          pattern = "\\.png$", recursive = TRUE
        )),
        rds_objects = length(list.files(out_path("rds"),
          pattern = "\\.rds$"
        ))
      )

      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      jpath <- out_path("logs", paste0(ts, "_session.json"))

      sess_list <- as.list(.sess)

      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::write_json(sess_list, jpath,
          pretty = TRUE, auto_unbox = TRUE,
          null = "null", na = "string"
        )
      } else {
        # Minimal fallback without jsonlite
        lines <- c(
          "{",
          sprintf('  "started_at"   : "%s",', sess_list$started_at),
          sprintf('  "completed_at" : "%s",', sess_list$completed_at),
          sprintf('  "study_name"   : "%s",', sess_list$study_name),
          sprintf('  "r_version"    : "%s",', sess_list$r_version),
          sprintf('  "n_ok"         : %d,', sess_list$n_ok),
          sprintf('  "n_errors"     : %d,', sess_list$n_errors),
          sprintf('  "total_secs"   : %g', sess_list$total_secs),
          "}"
        )
        writeLines(lines, jpath, useBytes = TRUE)
      }

      log_line("Session JSON : logs/", basename(jpath))
      invisible(jpath)
    },
    error = function(e) {
      log_warn("Could not write session JSON: ", conditionMessage(e))
      invisible(NULL)
    }
  )
}

#' Write a human-readable SUMMARY.txt to logs/ (complements JSON)
write_session_summary_txt <- function(n_ok = 0L, n_err = 0L,
                                      n_skip = 0L, total_secs = 0) {
  tryCatch(
    {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      spath <- out_path("logs", paste0(ts, "_SUMMARY.txt"))

      n_fig <- .sess$output_counts$figures_png %||% 0L
      n_csv <- .sess$output_counts$tables_csv %||% 0L
      n_tpng <- .sess$output_counts$tables_png %||% 0L
      cfg_display <- .sess$config_path %||% "(unknown)"

      # -----------------------------------------------------------------------
      # Section prefix -> display header (checked in order; first match wins)
      # -----------------------------------------------------------------------
      .sdefs <- list(
        list(p = "reliability_", h = "  [ Reliability ]"),
        list(p = "n_participants", h = "  [ Sample ]"),
        list(p = "sequence_groups", h = "  [ Sample ]"),
        list(
          p = "intervention_effect_",
          h = "  [ Intervention Effect  (within-person paired t-test, two-sided) ]"
        ),
        list(
          p = "period_effect_",
          h = "  [ Period Effect  (within-person paired t-test, two-sided) ]"
        ),
        list(
          p = "carryover_test__",
          h = "  [ Carryover Test  (Grizzle 1965 -- Welch t on Period-1 scores between sequences) ]"
        ),
        list(
          p = "seq_period_interaction__",
          h = "  [ Sequence x Period Interaction  (Welch t on per-person P2-minus-P1 differences) ]"
        ),
        list(
          p = "period_specific_int__",
          h = "  [ Period-Specific Intervention Effect  (Welch t on Int-minus-Ctl by period administered) ]"
        )
      )

      .get_section <- function(k) {
        for (d in .sdefs) {
          if (k == d$p || startsWith(k, d$p)) {
            return(d$h)
          }
        }
        "  [ Other ]"
      }

      # Convert raw session-result key names to readable labels
      .clean_label <- function(k) {
        exact <- c(
          "n_participants"  = "N",
          "sequence_groups" = "Sequence groups"
        )
        if (k %in% names(exact)) {
          return(exact[[k]])
        }
        k <- sub("^reliability_", "", k)
        k <- sub("^intervention_effect_", "", k)
        k <- sub("^period_effect_", "", k)
        k <- sub("^carryover_test__", "", k)
        k <- sub("^seq_period_interaction__", "", k)
        k <- sub("^period_specific_int__", "", k)
        k <- sub("^full__sample$", "Full scoring -- group means & SDs", k)
        k <- sub("^full__test$", "Full scoring -- t-test result", k)
        k <- sub("^restr__sample$", "Restricted scoring -- group means & SDs", k)
        k <- sub("^restr__test$", "Restricted scoring -- t-test result", k)
        k <- sub("^full$", "Full scoring", k)
        k <- sub("^restr$", "Restricted scoring", k)
        k <- sub("_full$", " (full)", k)
        k <- sub("_restricted$", " (restricted)", k)
        k
      }

      # Analysis settings block -- sourced directly from cfg for transparency
      .settings_block <- function() {
        if (!exists("cfg")) {
          return(character(0))
        }
        alpha_val <- cfg$analysis$alpha %||% 0.05
        ci_val <- cfg$analysis$ci_level %||% 0.95
        x_excl <- cfg$item_exclusions$x %||% cfg$scores$exclude$x %||% character(0)
        y_excl <- cfg$item_exclusions$y %||% cfg$scores$exclude$y %||% character(0)
        x_excl <- unlist(x_excl, use.names = FALSE)
        y_excl <- unlist(y_excl, use.names = FALSE)
        x_str <- if (length(x_excl) > 0) paste(toupper(x_excl), collapse = ", ") else "none"
        y_str <- if (length(y_excl) > 0) paste(toupper(y_excl), collapse = ", ") else "none"
        c(
          "  [ Analysis Settings ]",
          sprintf("    %-42s %s", "Alpha (significance threshold):", alpha_val),
          sprintf("    %-42s %s", "CI level:", paste0(round(ci_val * 100), "%")),
          sprintf("    %-42s %s", "Items excluded from Form X:", x_str),
          sprintf("    %-42s %s", "Items excluded from Form Y:", y_str)
        )
      }

      # Session results grouped by section with labeled headers
      .results_block <- function() {
        rs <- if (exists("results_summary", envir = .sess, inherits = FALSE)) {
          .sess$results_summary
        } else {
          list()
        }
        if (length(rs) == 0) {
          return(character(0))
        }
        out <- character(0)
        cur <- ""
        for (k in names(rs)) {
          sec <- .get_section(k)
          if (sec != cur) {
            out <- c(out, "", sec)
            cur <- sec
          }
          lbl <- .clean_label(k)
          out <- c(out, sprintf("    %-42s %s", paste0(lbl, ":"), rs[[k]]))
        }
        out
      }

      cr_lines <- c(.settings_block(), .results_block())
      computed_block <- if (length(cr_lines) > 0) {
        c("  Computed results:", paste0("  ", strrep("-", 68)), cr_lines)
      } else {
        "  Computed results: (analyses module not run)"
      }

      lines <- c(
        strrep("=", 72),
        "  PIPELINE RUN SUMMARY",
        strrep("=", 72),
        paste0("  Study       : ", STUDY_NAME),
        paste0("  Config      : ", cfg_display),
        paste0("  Data dir    : ", DATA_DIR),
        paste0("  Started     : ", .sess$started_at),
        paste0("  Completed   : ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
        paste0("  Total time  : ", round(total_secs, 1), "s"),
        paste0("  Modules OK  : ", n_ok),
        paste0("  Errors      : ", n_err),
        paste0("  Figures     : ", n_fig, " PNG files"),
        paste0("  Tables (CSV): ", n_csv, " files"),
        paste0("  Tables (PNG): ", n_tpng, " files"),
        "",
        "  Module timing:",
        vapply(names(.sess$modules), function(mid) {
          m <- .sess$modules[[mid]]
          sprintf("    %-20s  %-8s  %.1fs", mid, m$status, m$elapsed_s %||% 0)
        }, character(1)),
        "",
        if (length(.sess$warnings_list) > 0) c("  Warnings:", paste0("    ", .sess$warnings_list)) else "  Warnings: none",
        "",
        if (length(.sess$errors_list) > 0) c("  Errors:", paste0("    ", .sess$errors_list)) else "  Errors: none",
        "",
        if (length(.sess$flags) > 0) {
          flag_lines <- vapply(names(.sess$flags), function(k) {
            f <- .sess$flags[[k]]
            sprintf("    [%s] %s -- %s  %s", f$form, toupper(f$item), f$category, f$detail)
          }, character(1))
          c(paste0("  Psychometric flags (", length(.sess$flags), "):"), flag_lines)
        } else {
          c("  Psychometric flags: none")
        },
        "",
        computed_block,
        "",
        strrep("=", 72)
      )

      writeLines(lines, spath, useBytes = TRUE)
      log_line("Summary TXT  : logs/", basename(spath))
      invisible(spath)
    },
    error = function(e) {
      log_warn("Could not write summary TXT: ", conditionMessage(e))
      invisible(NULL)
    }
  )
}

# =============================================================================
# DATA-FRAME LOG HELPER
# =============================================================================

#' Log a compact summary of a data frame (dimensions + columns + types)
log_df <- function(df, label = "Data frame") {
  nc <- ncol(df)
  nr <- nrow(df)
  cat("[DF]    ", label, " \u2014 ", nr, " rows \u00d7 ", nc, " cols\n", sep = "")
  if (nc > 0) {
    types <- sapply(df, function(x) substr(class(x)[1], 1, 5))
    col_info <- paste0(names(df), "<", types, ">")
    cat("        ", paste(col_info, collapse = ", "), "\n", sep = "")
  }
}

#' Log a psychometric quality flag — always goes to log AND session record
log_flag <- function(item, form, reason, detail = NULL) {
  msg <- sprintf(
    "[FLAG]  %-6s  %-14s  %s%s",
    toupper(item), form, reason,
    if (!is.null(detail)) paste0("  (", detail, ")") else ""
  )
  cat(msg, "\n", sep = "")
  session_record_flag(item, form, reason, detail)
}

# Override log_warn to also capture in session log
.orig_log_warn <- log_warn
log_warn <- function(...) {
  msg <- paste0(...)
  cat("[WARN]  ", msg, "\n", sep = "")
  session_record_warning(msg)
}

# =============================================================================
# DONE
# =============================================================================
log_line("00_setup.R loaded (study=", STUDY_NAME, ", data=", DATA_DIR, ")")
