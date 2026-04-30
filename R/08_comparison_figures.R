## =============================================================================
## R/08_comparison_figures.R
## Side-by-side (or top-and-bottom) comparison panels from multiple runs.
##
## When comparison.common_scale is true (recommended):
##   1. Reads every run's analysis_data.rds to find the global score minimum.
##   2. Re-renders 05_figures.R for each run into figures_comparison/ using
##      that shared lower bound (SCORE_Y_LO_OVERRIDE env var).
##      The per-run figures/ directories are left untouched.
##   3. Stitches the common-scale figures into the comparison output dir.
##
## When common_scale is false:
##   Stitches each run's existing figures/ PNGs directly (scales may differ).
##
## Invoked by run_comparison.R.  Set COMPARISON_CONFIG env var first.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---------------------------------------------------------------------------
# Require magick and yaml
# ---------------------------------------------------------------------------
for (.pkg in c("magick", "yaml")) {
  if (!requireNamespace(.pkg, quietly = TRUE)) {
    stop(
      "Package '", .pkg, "' is required for comparison figures.\n",
      "  Install with: install.packages('", .pkg, "')"
    )
  }
}

# ---------------------------------------------------------------------------
# Resolve project root
# (set by run_comparison.R; fall back to cwd if sourced interactively)
# ---------------------------------------------------------------------------
if (!exists("proj_root")) {
  proj_root <- normalizePath(
    tryCatch({
      args <- commandArgs(trailingOnly = FALSE)
      f    <- sub("^--file=", "", args[startsWith(args, "--file=")])
      if (length(f) && nzchar(f[1])) dirname(dirname(f[1])) else "."
    }, error = function(e) "."),
    winslash = "/", mustWork = FALSE
  )
}
if (!exists("r_dir")) r_dir <- file.path(proj_root, "R")

cat(strrep("=", 72), "\n")
cat("  COMPARISON FIGURES\n")
cat("  Copyright (c) 2026 Aidan Sauls\n")
cat("  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 72), "\n\n")
cat("Project root :", proj_root, "\n")

# ---------------------------------------------------------------------------
# 1. Load comparison config
# ---------------------------------------------------------------------------
comp_cfg_path <- Sys.getenv("COMPARISON_CONFIG", unset = "")
if (!nzchar(comp_cfg_path))
  stop("COMPARISON_CONFIG environment variable not set.\n",
       "  Example:  $env:COMPARISON_CONFIG = 'config\\comparison_pilot.yml'")

if (!file.exists(comp_cfg_path))
  comp_cfg_path <- file.path(proj_root, comp_cfg_path)
if (!file.exists(comp_cfg_path))
  stop("Comparison config not found: ", comp_cfg_path)

yml  <- yaml::read_yaml(comp_cfg_path)
comp <- yml[["comparison"]]
if (is.null(comp)) stop("No 'comparison:' section found in ", comp_cfg_path)

runs         <- comp[["runs"]]
layout       <- comp[["layout"]]        %||% "side_by_side"
out_name     <- comp[["output_name"]]   %||% "comparison"
dpi          <- as.integer(comp[["dpi"]] %||% 300)
label_size   <- as.integer(comp[["label_font_size"]] %||% 28)
fig_groups       <- comp[["figure_groups"]] %||% "all"
common_scale     <- isTRUE(comp[["common_scale"]])
source_data_name <- comp[["source_data_name"]] %||% NULL

# ---------------------------------------------------------------------------
# Classification tiers for figure subfolders
# ---------------------------------------------------------------------------
# All score-based, item-level, psychometric, exploratory, and supplementary
# figures are classified as full comparison panels.  Exclusions affect scores,
# item statistics, psychometric indices, and derived summaries, so these figures
# are conceptually capable of changing across exclusion variants regardless of
# whether they actually differ in a particular dataset.
#
# The distinction between "did not change in this dataset" and "cannot change
# regardless of dataset" is fundamental: auto-collapsing figures that are merely
# similar in this sample would hide real differences in other datasets.
#
# Only figures that are structurally incapable of changing with exclusions
# (e.g. raw timing data) are shown as single-output.
#
# Core comparison: side-by-side panels that directly answer the key question
# "Does excluding Y6 in addition to Y1 materially change the results?"
.TIER_CORE         <- c("primary", "mixed_models")
# Secondary comparison: side-by-side panels for secondary inferential checks
.TIER_SECONDARY    <- c("secondary")
# Item-level and psychometric figures: all shown as full comparison panels.
# These are DIRECTLY affected by which items are included in scoring.
.TIER_ITEM         <- c("item_analysis", "psychometrics")
# Exploratory and supplementary: compare but label clearly as non-primary
.TIER_EXPLORATORY  <- c("exploratory")
.TIER_SUPPLEMENTARY <- c("supplementary")
# Descriptive: mostly score-based so shown as comparison; a per-file list
# (.STRUCTURAL_SINGLE_FILES) identifies the small subset that cannot change.
.TIER_DESCRIPTIVE  <- c("descriptive")
# Suppressed from comparison: sequence-subgroup / between-group content.
# period_effects figures (Fig 4, 15, 21) are sequence-group comparisons that
# do not speak to the cross-exclusion-variant question at all.
.TIER_SUPPRESSED   <- c("period_effects")

# Files that are structurally incapable of changing when scoring exclusions
# change.  These are shown as single-output rather than comparison panels.
# Timing/completion data: collected independently of which items are scored.
.STRUCTURAL_SINGLE_FILES <- c(
  "time_taken_by_form.png"
)

# Files that are descriptively invariant across scoring-exclusion variants.
# These contain raw item-level data (responses, correlations, difficulty) that
# does not depend on which items are included in the total score.
# They are rendered once using the reference run (the run with the broadest
# exclusion set) so that two-level markers (Y1**, Y6*) appear where applicable.
# Side-by-side comparison for these figures is uninformative: both panels would
# show identical data, differing only in which items bear an exclusion marker.
.SINGLE_DESCRIPTIVE_FILES <- c(
  # Item intercorrelation heatmaps: Pearson r between raw item responses.
  # Correlations do not change when the scoring exclusion set changes.
  # The reference-run figure carries two-level axis/cell markers.
  "item_intercorrelation_heatmap_x.png",
  "item_intercorrelation_heatmap_y.png",
  # Raw item response matrices: participant × item binary response grids.
  "item_response_matrix_x.png",
  "item_response_matrix_y.png",
  # Item endorsement rates by sequence group: raw proportion-correct per item.
  "item_endorsement_by_sequence.png",
  # Item difficulty (p-values): proportion correct, independent of scoring.
  "item_difficulty_by_form.png",
  # X-form discrimination: X scores are unaffected when only Y exclusions vary.
  "item_discrimination_boxplot_form_x.png"
)

.classify_subfolder <- function(sf) {
  if (sf %in% .TIER_SUPPRESSED)    return("Suppressed - sequence/period effects")
  if (sf %in% .TIER_CORE)          return("Core comparison")
  if (sf %in% .TIER_SECONDARY)     return("Secondary comparison")
  if (sf %in% .TIER_ITEM)          return("Item/psychometric comparison")
  if (sf %in% .TIER_EXPLORATORY)   return("Exploratory comparison")
  if (sf %in% .TIER_SUPPLEMENTARY) return("Supplementary comparison")
  if (sf %in% .TIER_DESCRIPTIVE)   return("Descriptive comparison")
  "Core comparison"  # unknown subfolders: default to comparing
}

# Standard exclusion context note appended to all comparison outputs.
.EXCL_NOTE <- paste0(
  "Y1 is excluded in both variants. ",
  "Y6 is additionally excluded only in the second variant.")

# Normalize shorthand run labels (from YAML or auto-compare) to canonical form.
.normalize_label <- function(lbl) {
  lbl <- trimws(lbl %||% "")
  lookup <- list(
    "y1"                                     = "Y1 excluded",
    "Y1"                                     = "Y1 excluded",
    "y1 excluded"                            = "Y1 excluded",
    "Y1 Excluded"                            = "Y1 excluded",
    "Y1 excluded"                            = "Y1 excluded",
    "Restricted scoring: Y1 excluded"        = "Y1 excluded",
    "y1,y6"                                  = "Y1 and Y6 excluded",
    "y1+y6"                                  = "Y1 and Y6 excluded",
    "Y1+Y6"                                  = "Y1 and Y6 excluded",
    "y1 & y6 excluded"                       = "Y1 and Y6 excluded",
    "Y1 & Y6 Excluded"                       = "Y1 and Y6 excluded",
    "Y1 & Y6 excluded"                       = "Y1 and Y6 excluded",
    "y1 and y6 excluded"                     = "Y1 and Y6 excluded",
    "Y1 and Y6 excluded"                     = "Y1 and Y6 excluded",
    "Restricted scoring: Y1 and Y6 excluded" = "Y1 and Y6 excluded"
  )
  lookup[[lbl]] %||% lbl
}

# Apply label normalization to all runs immediately after config load,
# so every downstream reference uses the explicit label.
runs <- lapply(runs, function(r) {
  r[["label"]] <- .normalize_label(r[["label"]] %||% r[["name"]] %||% "")
  r
})

# ---------------------------------------------------------------------------
# 2. Validate runs
# ---------------------------------------------------------------------------
if (length(runs) < 2) stop("Need at least 2 entries under comparison.runs.")

outputs_dir <- file.path(proj_root, "outputs")

for (r in runs) {
  d <- file.path(outputs_dir, r[["name"]], "figures")
  if (!dir.exists(d))
    stop("Figures directory not found for run '", r[["name"]], "':\n  ", d,
         "\n  Run the main pipeline for this config first.")
}

cat("Runs:\n")
for (r in runs) cat(sprintf("  %-30s -> label: %s\n", r[["name"]], r[["label"]]))
cat("Layout      :", layout, "\n")
cat("Common scale:", common_scale, "\n")
cat("Output      :", file.path(outputs_dir, out_name), "\n")
cat("Figure groups:",
    if (identical(fig_groups, "all")) "all" else paste(fig_groups, collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 2b. Cross-run exclusion context
# ---------------------------------------------------------------------------
# Load each run's raw_data.rds to determine which items are excluded in which
# variants.  This drives two-level exclusion markers in the comparison heatmaps:
#   ** = item excluded in ALL comparison variants (always excluded)
#   *  = item excluded only in the STRICTER comparison variant
#
# These sets are passed to 05_figures.R via HEATMAP_EXCL_ALWAYS_X / _Y env vars
# during the context-aware re-render (section 3b below).
.rds_excl <- lapply(runs, function(r) {
  rds_p <- file.path(outputs_dir, r[["name"]], "rds", "raw_data.rds")
  rd    <- tryCatch(readRDS(rds_p), error = function(e) NULL)
  list(
    x = if (!is.null(rd)) (rd[["x_excluded"]] %||% character(0)) else character(0),
    y = if (!is.null(rd)) (rd[["y_excluded"]] %||% character(0)) else character(0)
  )
})
names(.rds_excl) <- vapply(runs, `[[`, character(1), "name")

.all_excl_x    <- unique(unlist(lapply(.rds_excl, `[[`, "x")))
.all_excl_y    <- unique(unlist(lapply(.rds_excl, `[[`, "y")))
.always_excl_x <- Reduce(intersect, lapply(.rds_excl, `[[`, "x"))
.always_excl_y <- Reduce(intersect, lapply(.rds_excl, `[[`, "y"))
.strict_excl_x <- setdiff(.all_excl_x, .always_excl_x)
.strict_excl_y <- setdiff(.all_excl_y, .always_excl_y)

cat("Cross-run exclusion context:\n")
cat(sprintf("  X always excluded : %s\n",
            if (length(.always_excl_x)) paste(.always_excl_x, collapse = ", ") else "(none)"))
cat(sprintf("  X strict only     : %s\n",
            if (length(.strict_excl_x)) paste(.strict_excl_x, collapse = ", ") else "(none)"))
cat(sprintf("  Y always excluded : %s\n",
            if (length(.always_excl_y)) paste(.always_excl_y, collapse = ", ") else "(none)"))
cat(sprintf("  Y strict only     : %s\n",
            if (length(.strict_excl_y)) paste(.strict_excl_y, collapse = ", ") else "(none)"))
cat("\n")

# Reference run for single descriptive figures: the run whose re-rendered
# figures carry the most complete two-level exclusion context (most items
# excluded, so Y1** AND Y6* markers both appear in heatmaps etc.).
.ref_run_idx  <- which.max(vapply(runs, function(r) {
  length(.rds_excl[[r[["name"]]]][["x"]]) + length(.rds_excl[[r[["name"]]]][["y"]])
}, numeric(1)))
.ref_run_name <- runs[[.ref_run_idx]][["name"]]
cat("Reference run for single descriptive figures:", .ref_run_name, "\n\n")

# Two-level markers are active when at least one form has items in both sets.
.two_level_active <- (length(.always_excl_x) > 0 && length(.strict_excl_x) > 0) ||
                     (length(.always_excl_y) > 0 && length(.strict_excl_y) > 0)

# ---------------------------------------------------------------------------
# 3. Re-render step
# ---------------------------------------------------------------------------
# A re-render is needed when EITHER:
#   a) common_scale = true (shared y-axis lower bound across runs), OR
#   b) two-level exclusion marker context must be injected into figure code
#      (HEATMAP_EXCL_ALWAYS_X / _Y env vars).
#
# Both cases use the same mechanism: re-run 05_figures.R for each run into
# figures_comparison/ with the relevant env vars set.
#
# .figs_src is the subdirectory name used inside each run's outputs/ folder.
.figs_src <- "figures"

# A re-render is required when common_scale=true (shared y-axis) OR when
# two-level exclusion markers must be injected (HEATMAP_EXCL_ALWAYS_* env vars).
.needs_rerender <- common_scale || .two_level_active ||
                   length(.all_excl_x) > 0 || length(.all_excl_y) > 0

if (.needs_rerender) {
  # 3a. Compute global score_y_lo when common_scale is requested.
  global_y_lo <- NULL
  if (common_scale) {
    all_score_vals <- c()
    for (r in runs) {
      rds_p <- file.path(outputs_dir, r[["name"]], "rds", "analysis_data.rds")
      if (!file.exists(rds_p)) {
        warning("analysis_data.rds not found for '", r[["name"]], "' — skipping from global min")
        next
      }
      dat_r <- tryCatch(readRDS(rds_p), error = function(e) NULL)
      if (is.null(dat_r)) next
      sc <- grep("_score_(full|restricted)$", names(dat_r), value = TRUE)
      for (col in sc) all_score_vals <- c(all_score_vals, dat_r[[col]])
    }
    if (length(all_score_vals) == 0)
      stop("Could not read any analysis_data.rds — cannot compute common scale.")
    global_y_lo <- floor(min(all_score_vals, na.rm = TRUE))
    cat(sprintf("Common y_lo : %d  (floor of global min %.3f)\n\n",
                global_y_lo, min(all_score_vals, na.rm = TRUE)))
  }

  # 3b. Re-render each run's figures into figures_comparison/ with:
  #     - SCORE_Y_LO_OVERRIDE (when common_scale=true)
  #     - HEATMAP_EXCL_ALWAYS_X / _Y (when exclusion context available)
  #     The original figures/ dirs are NOT touched.
  rscript_bin    <- file.path(R.home("bin"),
                               if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  run_all_script <- file.path(r_dir, "run_all.R")

  # Build HEATMAP_EXCL_ALWAYS env var values (comma-separated item names).
  .hmap_always_x_val <- paste(.always_excl_x, collapse = ",")
  .hmap_always_y_val <- paste(.always_excl_y, collapse = ",")

  for (r in runs) {
    cfg_rel <- r[["config"]]
    # cfg_rel may be NULL if the comparison YAML has no per-run config entry.
    # Fall back to the global study_config.yml when absent.
    if (is.null(cfg_rel)) {
      cfg_rel <- file.path("config", "study_config.yml")
      message("No 'config:' entry for run '", r[["name"]], "' in comparison YAML.",
              "\n  Falling back to: ", cfg_rel,
              "\n  Item exclusions will be sourced from the run's raw_data.rds via ITEM_EXCLUSIONS.")
    }
    cfg_abs <- if (file.exists(cfg_rel)) cfg_rel else file.path(proj_root, cfg_rel)
    if (!file.exists(cfg_abs)) {
      warning("Config not found: ", cfg_abs, " — skipping re-render for '", r[["name"]], "'")
      next
    }

    # Derive per-run item exclusions from the previously loaded rds data.
    .run_excl_x <- .rds_excl[[r[["name"]]]][["x"]]
    .run_excl_y <- .rds_excl[[r[["name"]]]][["y"]]
    .run_excl_all <- c(.run_excl_x, .run_excl_y)
    .item_excl_val <- if (length(.run_excl_all) > 0) paste(.run_excl_all, collapse = ",") else "NONE"

    cat("Re-rendering :", r[["name"]],
        "  (ITEM_EXCLUSIONS=", .item_excl_val, ")\n", sep = "")

    # Save env vars we will temporarily override.
    .env_keys  <- c("PIPELINE_CONFIG", "STUDY_NAME", "ANALYSIS_MODULES",
                    "REUSE_DATA", "FIGURES_ROOT_SUFFIX", "SCORE_Y_LO_OVERRIDE",
                    "HEATMAP_EXCL_ALWAYS_X", "HEATMAP_EXCL_ALWAYS_Y",
                    "ITEM_EXCLUSIONS")
    .saved_env <- Sys.getenv(.env_keys, names = TRUE)

    .new_env <- list(
      PIPELINE_CONFIG       = normalizePath(cfg_abs, winslash = "/"),
      STUDY_NAME            = r[["name"]],
      ANALYSIS_MODULES      = "tables,figures",
      REUSE_DATA            = "1",
      FIGURES_ROOT_SUFFIX   = "figures_comparison",
      HEATMAP_EXCL_ALWAYS_X = .hmap_always_x_val,
      HEATMAP_EXCL_ALWAYS_Y = .hmap_always_y_val,
      ITEM_EXCLUSIONS       = .item_excl_val
    )
    if (!is.null(global_y_lo))
      .new_env[["SCORE_Y_LO_OVERRIDE"]] <- as.character(global_y_lo)

    do.call(Sys.setenv, .new_env)

    exit_code <- system2(rscript_bin, args = normalizePath(run_all_script, winslash = "/"),
                         stdout = "", stderr = "")

    # Restore env vars.
    for (k in names(.saved_env)) {
      if (nzchar(.saved_env[[k]])) {
        do.call(Sys.setenv, setNames(list(.saved_env[[k]]), k))
      } else {
        Sys.unsetenv(k)
      }
    }

    if (exit_code != 0)
      warning("Re-render failed for '", r[["name"]], "' (exit code ", exit_code, ")")
    else
      cat("[OK]  Re-render complete:", r[["name"]], "\n")
  }

  .figs_src <- "figures_comparison"
  cat("\n")
}

# ---------------------------------------------------------------------------
# 3c. Base data render for single descriptive figures
#     When source_data_name is specified in the comparison YAML, perform a
#     fresh import + figures render from the canonical source dataset.
#     Single descriptive figures are sourced from this render so they reflect
#     the actual study data rather than an exclusion-run RDS that may have been
#     built from a different source directory.
# ---------------------------------------------------------------------------
.base_render_name <- NULL
if (!is.null(source_data_name)) {
  .sdn_data_path <- file.path(proj_root, "study_data", source_data_name)
  if (!dir.exists(.sdn_data_path)) {
    warning("source_data_name '", source_data_name, "' not found in study_data/ — ",
            "single descriptive figures will fall back to reference comparison run.")
  } else {
    cat("Base data render for single descriptive figures:", source_data_name, "\n")
    .rscript_base  <- file.path(R.home("bin"),
                                if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    .run_all_base  <- file.path(r_dir, "run_all.R")
    .bexcl_all     <- c(.all_excl_x, .all_excl_y)
    .bexcl_str     <- if (length(.bexcl_all) > 0) paste(.bexcl_all, collapse = ",") else "NONE"

    .benv_keys  <- c("PIPELINE_CONFIG", "STUDY_NAME", "STUDY_DATA_PATH",
                     "ANALYSIS_MODULES", "REUSE_DATA", "FIGURES_ROOT_SUFFIX",
                     "HEATMAP_EXCL_ALWAYS_X", "HEATMAP_EXCL_ALWAYS_Y",
                     "ITEM_EXCLUSIONS")
    .benv_saved <- Sys.getenv(.benv_keys, names = TRUE)

    do.call(Sys.setenv, list(
      PIPELINE_CONFIG       = normalizePath(
                                file.path(proj_root, "config", "study_config.yml"),
                                winslash = "/"),
      STUDY_NAME            = source_data_name,
      STUDY_DATA_PATH       = normalizePath(.sdn_data_path, winslash = "/"),
      ANALYSIS_MODULES      = "psychometrics,analyses,figures",
      REUSE_DATA            = "0",
      FIGURES_ROOT_SUFFIX   = "figures_comparison",
      HEATMAP_EXCL_ALWAYS_X = paste(.always_excl_x, collapse = ","),
      HEATMAP_EXCL_ALWAYS_Y = paste(.always_excl_y, collapse = ","),
      ITEM_EXCLUSIONS       = .bexcl_str
    ))

    .base_exit <- system2(.rscript_base,
                          args = normalizePath(.run_all_base, winslash = "/"),
                          stdout = "", stderr = "")

    for (k in names(.benv_saved)) {
      if (nzchar(.benv_saved[[k]])) {
        do.call(Sys.setenv, setNames(list(.benv_saved[[k]]), k))
      } else {
        Sys.unsetenv(k)
      }
    }

    if (.base_exit != 0)
      warning("Base render failed for '", source_data_name, "' (exit ", .base_exit, ")")
    else {
      .base_render_name <- source_data_name
      cat("[OK]  Base render complete:", source_data_name, "\n")
    }
  }
}

# ---------------------------------------------------------------------------
# 4. Discover common figure subfolders / PNGs
# ---------------------------------------------------------------------------
first_fig_dir  <- file.path(outputs_dir, runs[[1]][["name"]], .figs_src)
all_subfolders <- sort(list.dirs(first_fig_dir, full.names = FALSE, recursive = FALSE))
all_subfolders <- all_subfolders[nzchar(all_subfolders)]

if (!identical(fig_groups, "all") && is.character(fig_groups) && length(fig_groups) > 0)
  all_subfolders <- intersect(all_subfolders, fig_groups)

if (length(all_subfolders) == 0)
  stop("No figure subfolders found under: ", first_fig_dir)

cat("Subfolders and comparison tiers:\n")
for (.sf in all_subfolders)
  cat(sprintf("  %-24s -> %s\n", .sf, .classify_subfolder(.sf)))
cat("\n")

# Pixel fingerprint helper: scale to 32x32 grey thumbnail, return mean pixel
# value (0–255).  Used to detect effectively unchanged figures across runs.
.img_fingerprint <- function(img) {
  thumb <- magick::image_convert(
    magick::image_scale(img, "32x32!"), colorspace = "gray")
  mean(as.numeric(magick::image_data(thumb, channels = "gray")))
}

.imgs_are_redundant <- function(fp_list, tol = 3.0) {
  # tol: max permitted mean-grey difference across all run thumbnails (0–255).
  # 3.0/255 ≈ 1.2% — catches identical or near-identical renders.
  fps <- unlist(fp_list)
  fps <- fps[!is.na(fps)]
  length(fps) >= 2 && (max(fps) - min(fps)) < tol
}

# Accumulated per-figure audit records (populated during section 6 loop).
.comp_audit <- list()

# ---------------------------------------------------------------------------
# 5. Create comparison output directory
# ---------------------------------------------------------------------------
out_root <- file.path(outputs_dir, out_name, "figures")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Clean stale output directories for suppressed subfolders.
# The loop in section 6 fires `next` for suppressed tiers, which prevents
# new files from being created but does NOT delete previously-generated files
# from earlier runs.  Explicitly remove them here so they cannot re-appear.
for (.sf_clean in all_subfolders) {
  if (.classify_subfolder(.sf_clean) == "Suppressed - sequence/period effects") {
    .stale_sf_out <- file.path(out_root, .sf_clean)
    if (dir.exists(.stale_sf_out)) {
      cat("[CLEAN] Removing stale suppressed output subfolder: ",
          .sf_clean, "\n", sep = "")
      unlink(.stale_sf_out, recursive = TRUE)
    }
  }
}

# ---------------------------------------------------------------------------
# 6. Build comparison panels figure by figure
# ---------------------------------------------------------------------------
# Internal helper: build a labelled header block to prepend above an image.
.make_header <- function(img, label_text, bg_color = "white") {
  w         <- as.integer(magick::image_info(img)[["width"]])
  px_per_pt <- dpi / 72.0
  h_hdr     <- max(200L, as.integer(label_size * px_per_pt * 2.5))
  hdr       <- magick::image_blank(w, h_hdr, color = bg_color)
  hdr       <- magick::image_annotate(hdr, text = label_text,
    gravity = "Center", size = label_size,
    weight  = 700L, color = "black", font = "sans")
  sep       <- magick::image_blank(w, 2L, color = "grey80")
  magick::image_append(c(hdr, sep), stack = TRUE)
}

n_created        <- 0L
n_single         <- 0L
n_descriptive    <- 0L
n_skipped        <- 0L
n_suppressed_sf  <- 0L

for (subfolder in all_subfolders) {

  .tier <- .classify_subfolder(subfolder)

  # Skip subfolders suppressed from comparison output.
  # period_effects figures are sequence-subgroup / between-group plots that
  # do not speak to the cross-exclusion-variant comparison question.
  if (.tier == "Suppressed - sequence/period effects") {
    cat("[SUPPRESSED] ", subfolder,
        " — sequence/between-group figures omitted from comparison package\n",
        sep = "")
    n_suppressed_sf <- n_suppressed_sf + 1L
    .comp_audit[[length(.comp_audit) + 1L]] <- list(
      Subfolder      = subfolder,
      File           = "(all)",
      Classification = "Suppressed - sequence/period effects",
      Outcome        = "Entire subfolder suppressed",
      Note           = paste0(
        "Sequence-subgroup / between-group content suppressed from comparison package. ",
        "Exists in per-run outputs. The comparison question (does Y6 exclusion change ",
        "within-subject conclusions?) is not answered by sequence-subgroup figures."))
    next
  }

  file_sets <- lapply(runs, function(r) {
    d <- file.path(outputs_dir, r[["name"]], .figs_src, subfolder)
    if (!dir.exists(d)) return(character(0))
    basename(list.files(d, pattern = "\\.png$", full.names = FALSE))
  })

  common_files <- Reduce(intersect, file_sets)
  if (length(common_files) == 0) {
    cat("[SKIP] ", subfolder, " — no common files across all runs\n", sep = "")
    next
  }

  out_sub <- file.path(out_root, subfolder)
  dir.create(out_sub, recursive = TRUE, showWarnings = FALSE)

  for (fig_file in common_files) {

    # --- Read raw source images (no header yet) ---
    raw_imgs <- lapply(runs, function(r) {
      path <- file.path(outputs_dir, r[["name"]], .figs_src, subfolder, fig_file)
      tryCatch(magick::image_read(path), error = function(e) NULL)
    })

    valid_idx <- which(!vapply(raw_imgs, is.null, logical(1)))
    if (length(valid_idx) == 0) {
      cat("[SKIP] ", subfolder, "/", fig_file, " — all source files missing\n", sep = "")
      n_skipped <- n_skipped + 1L
      .comp_audit[[length(.comp_audit) + 1L]] <- list(
        Subfolder      = subfolder, File = fig_file,
        Classification = .tier,    Outcome = "Skipped",
        Note = "Source files missing in all runs")
      next
    }

    out_path <- file.path(out_sub, fig_file)

    # --------------------------------------------------------------------- #
    # A) Structural single-output
    #    Shown as single-output because the data is collected independently
    #    of item scoring.  Examples: raw timing/completion figures.
    # --------------------------------------------------------------------- #
    if (fig_file %in% .STRUCTURAL_SINGLE_FILES) {
      base_img   <- raw_imgs[[valid_idx[1]]]
      .hdr_label <- paste0("Structural single-output\n",
                           "(timing/completion data — unaffected by scoring exclusions)")
      hdr      <- .make_header(base_img, .hdr_label)
      combined <- magick::image_append(c(hdr, base_img), stack = TRUE)
      magick::image_write(combined, path = out_path, density = dpi)
      cat("[SINGLE]    ", subfolder, "/", fig_file,
          " — structural invariant (timing data)\n", sep = "")
      n_single <- n_single + 1L
      .comp_audit[[length(.comp_audit) + 1L]] <- list(
        Subfolder      = subfolder, File = fig_file,
        Classification = "Structural single-output",
        Outcome        = "Single-output (structural invariant)",
        Note           = paste0(
          "Timing/completion data is collected independently of item scoring. ",
          "Genuinely cannot change when scoring exclusions change."))
      next
    }

    # --------------------------------------------------------------------- #
    # A2) Descriptive single-output
    #     Raw item-level data (correlations, responses, difficulty) that does
    #     not depend on the scoring exclusion set.  Showing these side-by-side
    #     is uninformative: both panels would be identical save for which items
    #     bear an exclusion marker.  Instead, use the reference run's re-render
    #     (the run with the broadest exclusion set) so that two-level axis/cell
    #     markers (Y1**, Y6*) appear in a single coherent figure.
    # --------------------------------------------------------------------- #
    if (fig_file %in% .SINGLE_DESCRIPTIVE_FILES) {
      # Prefer the base render (canonical source data) over the reference
      # comparison run so single descriptive figures reflect the actual study
      # data rather than an exclusion-run RDS built from a different source.
      .desc_src <- .base_render_name %||% .ref_run_name
      ref_path  <- file.path(outputs_dir, .desc_src, .figs_src, subfolder, fig_file)
      if (!file.exists(ref_path))
        ref_path <- file.path(outputs_dir, .desc_src, "figures", subfolder, fig_file)
      # If base render didn't produce this file, fall back to reference comparison run.
      if (!file.exists(ref_path) && !is.null(.base_render_name)) {
        ref_path <- file.path(outputs_dir, .ref_run_name, .figs_src, subfolder, fig_file)
        if (!file.exists(ref_path))
          ref_path <- file.path(outputs_dir, .ref_run_name, "figures", subfolder, fig_file)
      }

      if (file.exists(ref_path)) {
        ref_img <- tryCatch(magick::image_read(ref_path), error = function(e) NULL)
        if (!is.null(ref_img)) {
          magick::image_write(ref_img, path = out_path, density = dpi)
          cat("[DESCRIPTIVE] ", subfolder, "/", fig_file,
              " — single descriptive (raw item data)\n", sep = "")
          n_descriptive <- n_descriptive + 1L
          .comp_audit[[length(.comp_audit) + 1L]] <- list(
            Subfolder      = subfolder, File = fig_file,
            Classification = "Single descriptive",
            Outcome        = "Single descriptive figure (canonical source data)",
            Note           = paste0(
              "Raw item-level data unchanged by scoring exclusion choice. ",
              "Source: ", .desc_src, ". ",
              "Two-level exclusion markers (** / *) embedded in figure."))
          next
        }
      }
      # Reference file not found — fall through to comparison panel.
      cat("[WARN] Descriptive reference missing for ", fig_file,
          " in run '", .desc_src, "' — falling back to comparison panel\n", sep = "")
    }

    # --------------------------------------------------------------------- #
    # B) Full side-by-side (or stacked) comparison panel
    #    All remaining figures are shown as full comparison panels.
    #    No auto-collapse based on pixel similarity: figures that are
    #    conceptually capable of changing (scores, items, psychometrics,
    #    model outputs) are always shown side-by-side so the reader can
    #    judge the difference directly.
    # --------------------------------------------------------------------- #
    panel_imgs <- lapply(seq_along(runs), function(i) {
      img <- raw_imgs[[i]]
      if (is.null(img)) return(NULL)
      hdr <- .make_header(img, runs[[i]][["label"]])
      magick::image_append(c(hdr, img), stack = TRUE)
    })
    panel_imgs <- Filter(Negate(is.null), panel_imgs)
    if (length(panel_imgs) == 0) { n_skipped <- n_skipped + 1L; next }

    infos <- lapply(panel_imgs, magick::image_info)

    if (layout == "side_by_side") {
      min_h      <- min(vapply(infos, function(i) as.integer(i[["height"]]), integer(1)))
      panel_imgs <- lapply(panel_imgs, function(img)
        magick::image_scale(img, paste0("x", min_h)))
      combined   <- magick::image_append(do.call(c, panel_imgs), stack = FALSE)
    } else {
      min_w      <- min(vapply(infos, function(i) as.integer(i[["width"]]),  integer(1)))
      panel_imgs <- lapply(panel_imgs, function(img)
        magick::image_scale(img, paste0(min_w, "x")))
      combined   <- magick::image_append(do.call(c, panel_imgs), stack = TRUE)
    }

    magick::image_write(combined, path = out_path, density = dpi)
    cat("[OK]        ", subfolder, "/", fig_file, "\n", sep = "")
    n_created <- n_created + 1L
    .comp_audit[[length(.comp_audit) + 1L]] <- list(
      Subfolder      = subfolder, File = fig_file,
      Classification = .tier, Outcome = "Comparison panel produced",
      Note           = .EXCL_NOTE)
  }
}

# ---------------------------------------------------------------------------
# 7. Summary + per-figure classification audit CSV
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 72), "\n", sep = "")
cat(sprintf(
  "  Figure output:\n    %d comparison panels\n    %d single descriptive figures\n    %d single-output (structural invariant)\n    %d suppressed subfolders (sequence/between-group)\n    %d skipped\n  Output: %s\n",
  n_created, n_descriptive, n_single, n_suppressed_sf, n_skipped, out_root))
if (!is.null(global_y_lo))
  cat(sprintf("  Common y-axis lower bound: %d\n", global_y_lo))
cat(strrep("=", 72), "\n")

if (length(.comp_audit) > 0) {
  .audit_df  <- do.call(rbind, lapply(.comp_audit, as.data.frame,
                                      stringsAsFactors = FALSE))
  .audit_csv <- file.path(outputs_dir, out_name, "FIGURE_CLASSIFICATION.csv")
  write.csv(.audit_df, .audit_csv, row.names = FALSE, quote = TRUE)
  cat("\nFigure classification audit: ", .audit_csv, "\n", sep = "")
}

# ---------------------------------------------------------------------------
# 8. Comparison tables — CSV + PNG
# ---------------------------------------------------------------------------
# Local helper: write one comparison table as CSV and (optionally) PNG.
# Writes to outputs/<out_name>/tables/<name>.csv
#             outputs/<out_name>/tables_png/<name>.png
.comp_tbl_dir     <- file.path(outputs_dir, out_name, "tables")
.comp_tbl_png_dir <- file.path(outputs_dir, out_name, "tables_png")
dir.create(.comp_tbl_dir,     recursive = TRUE, showWarnings = FALSE)
dir.create(.comp_tbl_png_dir, recursive = TRUE, showWarnings = FALSE)

# Clean stale table files from previous runs before writing new ones.
# This prevents old outputs (from superseded code paths) from persisting
# alongside current outputs and causing confusion.
.stale_csv <- list.files(.comp_tbl_dir,     pattern = "\\.csv$", full.names = TRUE)
.stale_png <- list.files(.comp_tbl_png_dir, pattern = "\\.png$", full.names = TRUE)
if (length(.stale_csv) > 0 || length(.stale_png) > 0) {
  cat(sprintf("Cleaning %d stale table file(s) from previous run...\n",
              length(.stale_csv) + length(.stale_png)))
  file.remove(c(.stale_csv, .stale_png))
}

save_comp_table <- function(df, name, notes = NULL, png_note = NULL, sidecar = NULL) {
  csv_path <- file.path(.comp_tbl_dir, paste0(name, ".csv"))
  write.csv(df, csv_path, row.names = FALSE, quote = TRUE)
  if (!is.null(notes))
    cat(c("", "# --- Notes ---", paste0("# ", notes)),
        file = csv_path, sep = "\n", append = TRUE)
  cat("[CSV] ", basename(csv_path), "\n", sep = "")

  if (requireNamespace("gt", quietly = TRUE)) {
    png_path <- file.path(.comp_tbl_png_dir, paste0(name, ".png"))
    gt_tbl <- gt::gt(df) |>
      gt::tab_options(
        table.font.size                   = 11,
        column_labels.font.weight         = "bold",
        table.border.top.color            = "grey30",
        table.border.bottom.color         = "grey30",
        column_labels.border.bottom.color = "grey50",
        data_row.padding                  = gt::px(4)
      ) |>
      gt::opt_table_lines("none") |>
      gt::opt_row_striping()
    if (!is.null(png_note))
      gt_tbl <- gt_tbl |> gt::tab_source_note(gt::md(png_note))
    tryCatch({
      gt::gtsave(gt_tbl, png_path)
      cat("[PNG] ", basename(png_path), "\n", sep = "")
    }, error = function(e)
      message("[WARN] gt PNG failed for '", name, "': ", conditionMessage(e)))
  }
  if (!is.null(sidecar)) {
    sidecar_path <- file.path(.comp_tbl_png_dir, paste0(name, ".notes.md"))
    writeLines(sidecar, sidecar_path)
    cat("[NOTES] ", basename(sidecar_path), "\n", sep = "")
  }
  invisible(csv_path)
}

# Helper: if all comparison rows are identical at displayed precision across
# exclusion variants, returns a note string; otherwise returns NULL.
.identical_at_precision_note <- function(df, variant_col = "Exclusion Variant") {
  if (!variant_col %in% names(df)) return(NULL)
  variants <- unique(df[[variant_col]])
  if (length(variants) < 2L) return(NULL)
  value_cols <- setdiff(names(df), variant_col)
  rows_by_v  <- lapply(variants, function(v)
    df[df[[variant_col]] == v, value_cols, drop = FALSE])
  n_rows <- vapply(rows_by_v, nrow, integer(1L))
  if (length(unique(n_rows)) > 1L) return(NULL)
  ref      <- rows_by_v[[1L]]
  all_same <- all(vapply(rows_by_v[-1L], function(other) {
    all(mapply(function(a, b) identical(as.character(a), as.character(b)), ref, other))
  }, logical(1L)))
  if (all_same)
    paste0("NOTE: Values are identical at displayed precision across all exclusion variants. ",
           "Differences may exist at finer precision but are smaller than the rounding shown.")
  else NULL
}

# ---------------------------------------------------------------------------
# Pre-table staleness guard
# ---------------------------------------------------------------------------
# Verify that the key per-run source table used in all period/condition
# comparisons is not older than the corresponding analysis_data.rds.
# When .needs_rerender is TRUE (default for this config) tables are already
# regenerated in section 3b (ANALYSIS_MODULES=tables,figures); this guard
# catches the case where that step failed or .needs_rerender was FALSE.
{
  .stale_runs <- character(0)
  for (.sr in runs) {
    .rds_p <- file.path(outputs_dir, .sr[["name"]], "rds", "analysis_data.rds")
    .csv_p <- file.path(outputs_dir, .sr[["name"]], "tables", "descriptive",
                        "09_period_condition_cell_means.csv")
    if (!file.exists(.csv_p)) {
      .stale_runs <- c(.stale_runs, .sr[["name"]])
      cat(sprintf("[STALE] %s: 09_period_condition_cell_means.csv is missing\n",
                  .sr[["name"]]))
    } else if (file.exists(.rds_p)) {
      .rds_mtime <- file.info(.rds_p)$mtime
      .csv_mtime <- file.info(.csv_p)$mtime
      if (.csv_mtime < .rds_mtime) {
        .stale_runs <- c(.stale_runs, .sr[["name"]])
        cat(sprintf("[STALE] %s: CSV (%s) older than analysis_data.rds (%s)\n",
                    .sr[["name"]], format(.csv_mtime), format(.rds_mtime)))
      }
    }
  }
  if (length(.stale_runs) > 0) {
    stop(
      "Per-run source table(s) are stale or missing for: ",
      paste(.stale_runs, collapse = ", "), "\n",
      "Regenerate with (PowerShell):\n",
      paste(vapply(.stale_runs, function(rn)
        sprintf("  $env:STUDY_NAME='%s'; $env:REUSE_DATA='1'; $env:ANALYSIS_MODULES='tables'; Rscript --vanilla R/run_all.R", rn),
        character(1)), collapse = "\n"),
      "\nThen re-run the comparison pipeline."
    )
  }
  rm(.stale_runs)
}

cat("\nBuilding comparison tables...\n")

# ---------------------------------------------------------------------------
# §8 local helpers: paired-contrast computation for within-subject subgroup rows
# ---------------------------------------------------------------------------
.fmt_ms_sub <- function(m, s) sprintf("%.2f (%.2f)", m, s)
.fmt_ci_sub <- function(lo, hi) sprintf("[%.2f, %.2f]", lo, hi)
.fmt_p_sub  <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}
# Normalise bare decimals that lack a leading zero: ".266" -> "0.266",
# "< .001" -> "< 0.001", "-.093" -> "-0.093". No-op on already-correct strings.
.add_leading_zero <- function(s) {
  gsub("(?<![0-9])(-)?\\.([0-9])", "\\10.\\2", s, perl = TRUE)
}
# Compute within-subject paired contrast: a - b.
# Returns list(n, m_a, sd_a, m_b, sd_b, m_diff, dz, ci_lo, ci_hi, p).
.paired_contrast <- function(a, b, ci_level = 0.95) {
  valid <- !is.na(a) & !is.na(b)
  a <- a[valid]; b <- b[valid]
  n <- length(a)
  if (n < 2L) return(NULL)
  diffs <- a - b
  tt    <- tryCatch(t.test(a, b, paired = TRUE, conf.level = ci_level),
                    error = function(e) NULL)
  list(
    n      = n,
    m_a    = mean(a),  sd_a = sd(a),
    m_b    = mean(b),  sd_b = sd(b),
    m_diff = mean(diffs),
    dz     = if (sd(diffs) > 0) mean(diffs) / sd(diffs) else NA_real_,
    ci_lo  = if (!is.null(tt)) tt$conf.int[1L] else NA_real_,
    ci_hi  = if (!is.null(tt)) tt$conf.int[2L] else NA_real_,
    p      = if (!is.null(tt)) tt$p.value else NA_real_
  )
}

# ---------------------------------------------------------------------------
# 8a. Overall results — both contrasts, all exclusion variants
#     Primary sensitivity table: mirrors structure of 00_overall_results.csv.
#     Rows: AI vs Control (restricted) + Period 2 vs Period 1 (restricted),
#     one row per exclusion variant, sorted by Effect then Exclusion Variant.
# ---------------------------------------------------------------------------
.rows_8a <- lapply(runs, function(r) {
  run_name  <- r[["name"]]
  run_label <- r[["label"]] %||% run_name

  contrasts_f <- file.path(outputs_dir, run_name,
                            "tables", "primary", "03_primary_contrasts.csv")
  if (!file.exists(contrasts_f)) {
    message("[WARN] Primary contrasts table not found for '", run_name, "'"); return(NULL)
  }
  contrasts <- tryCatch(
    read.csv(contrasts_f, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) { message("[WARN] ", e$message); NULL }
  )
  if (is.null(contrasts)) return(NULL)

  int_row <- contrasts[grepl("Intervention vs Control", contrasts$Contrast, fixed = TRUE) &
                         grepl("restricted", contrasts$Contrast, fixed = TRUE), , drop = FALSE]
  per_row <- contrasts[grepl("Period 2 vs Period 1",    contrasts$Contrast, fixed = TRUE) &
                         grepl("restricted", contrasts$Contrast, fixed = TRUE), , drop = FALSE]

  mm_f <- file.path(outputs_dir, run_name,
                    "tables", "mixed_models", "07_mixed_model_results.csv")
  cond_est <- cond_p <- per_est <- per_p <- NA_character_
  if (file.exists(mm_f)) {
    mm <- tryCatch(read.csv(mm_f, stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) NULL)
    if (!is.null(mm)) {
      mm_c <- mm[tolower(mm[["Scoring"]]) == "restricted" &
                   grepl("condition_fac", mm[["Term"]], fixed = TRUE), , drop = FALSE]
      if (nrow(mm_c) > 0) {
        cond_est <- .add_leading_zero(paste0(
          sub("^=\\s*", "", as.character(round(as.numeric(mm_c[["Estimate"]][1]), 3))),
          " (", sub("^=\\s*", "", as.character(round(as.numeric(mm_c[["SE"]][1]),       3))), ")"
        ))
        cond_p <- .add_leading_zero(sub("^=\\s*", "", as.character(mm_c[["p"]][1])))
      }
      mm_p <- mm[tolower(mm[["Scoring"]]) == "restricted" &
                   grepl("period_fac", mm[["Term"]], fixed = TRUE), , drop = FALSE]
      if (nrow(mm_p) > 0) {
        per_est <- .add_leading_zero(paste0(
          sub("^=\\s*", "", as.character(round(as.numeric(mm_p[["Estimate"]][1]), 3))),
          " (", sub("^=\\s*", "", as.character(round(as.numeric(mm_p[["SE"]][1]),      3))), ")"
        ))
        per_p <- .add_leading_zero(sub("^=\\s*", "", as.character(mm_p[["p"]][1])))
      }
    }
  }

  rows <- list()
  if (nrow(int_row) > 0) {
    pr <- int_row[1L, ]
    rows[[1L]] <- data.frame(
      "Effect"            = "AI vs. Control",
      "Exclusion Variant" = run_label,
      "N"                 = pr[["N"]],
      "Mean A (SD)"       = pr[["Mean A (SD)"]],
      "Mean B (SD)"       = pr[["Mean B (SD)"]],
      "Mean Diff"         = pr[["Mean Diff"]],
      "95% CI"            = pr[["95% CI"]],
      "Cohen dz"          = pr[["Cohen dz"]],
      "p"                 = .add_leading_zero(sub("^=\\s*", "", as.character(pr[["p"]]))),
      "MM Est (SE)"       = cond_est,
      "MM p"              = cond_p,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  if (nrow(per_row) > 0) {
    pr <- per_row[1L, ]
    rows[[2L]] <- data.frame(
      "Effect"            = "Period 2 vs. Period 1",
      "Exclusion Variant" = run_label,
      "N"                 = pr[["N"]],
      "Mean A (SD)"       = pr[["Mean A (SD)"]],
      "Mean B (SD)"       = pr[["Mean B (SD)"]],
      "Mean Diff"         = pr[["Mean Diff"]],
      "95% CI"            = pr[["95% CI"]],
      "Cohen dz"          = pr[["Cohen dz"]],
      "p"                 = .add_leading_zero(sub("^=\\s*", "", as.character(pr[["p"]]))),
      "MM Est (SE)"       = per_est,
      "MM p"              = per_p,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
})
.rows_8a <- Filter(Negate(is.null), .rows_8a)
if (length(.rows_8a) > 0) {
  .df_8a <- do.call(rbind, .rows_8a)
  # Sort: AI vs Control first, then Period 2 vs Period 1; within each, by variant
  .df_8a <- .df_8a[order(.df_8a[["Effect"]], .df_8a[["Exclusion Variant"]]), ]
  .notes_8a <- Filter(Negate(is.null), c(
    "Sensitivity check: both primary contrasts (AI vs. Control and Period 2 vs. Period 1) under restricted scoring, compared across exclusion variants.",
    "Mean A (SD) and Mean B (SD): A = first-named group in Effect column (AI; Period 2). B = second-named group (Control; Period 1). Mean Diff = A minus B (paired).",
    "MM Est (SE) = linear mixed-model fixed-effect estimate (SE) for each contrast.",
    "Table structure mirrors 00_overall_results.csv in each run's primary/ folder.",
    .EXCL_NOTE,
    .identical_at_precision_note(.df_8a)
  ))
  .sidecar_8a <- c(
    "# Variant Comparison: Overall Results (Sensitivity Table)",
    "",
    "**Display title:** Sensitivity Analysis \u2014 Overall Results by Exclusion Variant (Restricted Scoring)",
    "",
    "**Suggested manuscript title:** Primary and Period Effects by Exclusion Variant: Sensitivity Analysis (Restricted Scoring)",
    "",
    "**Suggested caption:** Sensitivity analysis: AI vs.\u00a0Control and Period\u00a02 vs.\u00a0Period\u00a01 contrasts",
    "across two item-exclusion variants (restricted scoring).",
    "Mean A = first-named group in Effect column (AI; Period\u00a02). Mean B = second-named group (Control; Period\u00a01).",
    "Mean Diff = A minus B (paired). MM\u00a0Est (SE) = linear mixed-model fixed-effect estimate (SE).",
    "",
    "## What it shows",
    "",
    "Both primary contrasts (AI vs.\u00a0Control and Period\u00a02 vs.\u00a0Period\u00a01) under restricted",
    "scoring, side by side for each exclusion variant. Allows a direct sensitivity check:",
    "do the overall results change when Y6 is additionally excluded?",
    "",
    "## How to interpret it",
    "",
    "Each pair of rows (one per exclusion variant) represents the same contrast computed",
    "with a different item set. Similar effect sizes and significance levels across variants",
    "indicate robustness to the Y6 exclusion decision.",
    "",
    "## What it does not show / Limitations",
    "",
    "- Not a statistical test of whether the two variants differ from each other.",
    "- Does not address the substantive justification for excluding Y6.",
    "- MM Est (SE) is a corroborating estimate, not an independent hypothesis test.",
    "",
    "## Important notes",
    "",
    "- Y1 excluded in both variants; only Y6 exclusion varies.",
    "- Restricted scoring: scores use only items retained under each variant\u2019s exclusion rule.",
    "- MM Est (SE) is read from 07_mixed_model_results.csv (SE column required).",
    "",
    "## Classification",
    "",
    "- **Type:** Sensitivity",
    "- **Source file(s):** tables/primary/03_primary_contrasts.csv,",
    "  tables/mixed_models/07_mixed_model_results.csv (per run)"
  )
  save_comp_table(.df_8a, "variant_comparison_overall_results",
                  notes = .notes_8a, sidecar = .sidecar_8a)
} else {
  message("[WARN] variant_comparison_overall_results: no rows — table not written.")
}

# ---------------------------------------------------------------------------
# 8b. Full vs restricted sensitivity check [SENSITIVITY — Table D]
# ---------------------------------------------------------------------------
# Draws full and restricted Intervention vs Control contrasts from
# 03_primary_contrasts.csv (same source as 8a).  Replaces the earlier lookup
# of 13b_full_vs_restricted_effect_sizes.csv which was in the wrong subfolder.
.rows_8b <- lapply(runs, function(r) {
  run_name  <- r[["name"]]
  run_label <- r[["label"]] %||% run_name

  f <- file.path(outputs_dir, run_name, "tables", "primary", "03_primary_contrasts.csv")
  if (!file.exists(f)) {
    message("[WARN] 03_primary_contrasts.csv not found (8b) for '", run_name, "'")
    return(NULL)
  }
  contrasts <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                        error = function(e) { message("[WARN] ", e$message); NULL })
  if (is.null(contrasts)) return(NULL)

  full_row <- contrasts[grepl("Intervention vs Control", contrasts$Contrast, fixed = TRUE) &
                          grepl("full", contrasts$Contrast, fixed = TRUE), , drop = FALSE]
  rest_row <- contrasts[grepl("Intervention vs Control", contrasts$Contrast, fixed = TRUE) &
                          grepl("restricted", contrasts$Contrast, fixed = TRUE), , drop = FALSE]

  rows <- list()
  for (.info in list(list(row = full_row, lbl = "Full"),
                     list(row = rest_row, lbl = "Restricted"))) {
    if (nrow(.info$row) == 0) next
    pr <- .info$row[1, ]
    rows[[length(rows) + 1L]] <- data.frame(
      "Exclusion Variant" = run_label,
      "Scoring"           = .info$lbl,
      "N"                 = pr[["N"]],
      "AI Mean (SD)"      = pr[["Mean A (SD)"]],
      "Control Mean (SD)" = pr[["Mean B (SD)"]],
      "Mean Diff"         = pr[["Mean Diff"]],
      "95% CI"            = pr[["95% CI"]],
      "Cohen dz"          = pr[["Cohen dz"]],
      "p"                 = .add_leading_zero(sub("^=\\s*", "", as.character(pr[["p"]]))),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
})
.rows_8b <- Filter(Negate(is.null), .rows_8b)

# NOTE: full-vs-restricted is intentionally excluded from the default comparison
# package.  The comparison question here is specifically whether adding Y6
# exclusion (on top of Y1) changes results — not whether full vs restricted
# scoring differs.  The table is written to InternalUse/ only for reference.
if (length(.rows_8b) > 0) {
  .internal_csv <- file.path(outputs_dir, out_name, "InternalUse",
                             "variant_comparison_full_vs_restricted_internal.csv")
  .internal_csv <- normalizePath(.internal_csv, mustWork = FALSE)
  dir.create(dirname(.internal_csv), showWarnings = FALSE, recursive = TRUE)
  .tmp <- do.call(rbind, .rows_8b)
  write.csv(.tmp, .internal_csv, row.names = FALSE, quote = TRUE)
  cat("[INTERNAL] variant_comparison_full_vs_restricted_internal.csv (InternalUse only — not in default package)\n")
}

# ---------------------------------------------------------------------------
# 8c. [SUPPRESSED] Period effect — merged into variant_comparison_overall_results
# ---------------------------------------------------------------------------
# The within-subject period/practice effect (P2 vs P1, restricted scoring) is
# now the second row-block in variant_comparison_overall_results (§8a), which
# mirrors the structure of the main pipeline's 00_overall_results table.
# A separate period_effect table is redundant.
cat("[SUPPRESSED] §8c variant_comparison_period_effect: merged into variant_comparison_overall_results\n")

# ---------------------------------------------------------------------------
# 8d. [SUPPRESSED] Period / sequence cell means
# ---------------------------------------------------------------------------
# Overlaps with §8g (variant_comparison_subgroup_period) which provides the
# same subgroup period breakdown with 95% CI, Cohen dz, and paired p-values.
cat("[SUPPRESSED] \u00a78d variant_comparison_period_cell_means: overlaps with \u00a78g variant_comparison_subgroup_period\n")

# ---------------------------------------------------------------------------
# 8e. [SUPPRESSED] Condition-by-sequence cell means (descriptive)
# ---------------------------------------------------------------------------
# Subsumed by §8f (variant_comparison_subgroup_condition) which provides the
# same subgroup AI vs Control breakdown with 95% CI, Cohen dz, and p-values.
cat("[SUPPRESSED] \u00a78e variant_comparison_condition_by_sequence: subsumed by \u00a78f variant_comparison_subgroup_condition\n")

# NOTE: The sequence-subgroup / period-specific intervention effect table
# (Table 15, 15_period_specific_intervention_effect.csv) is intentionally
# excluded from the comparison package.  It addresses a different question
# (does the intervention effect differ by which period it fell in?) using
# between-group framing that does not belong in the core exclusion-variant
# comparison.  It exists in each run's own outputs/period_effects/ directory.

# ---------------------------------------------------------------------------
# 8f. Condition (AI vs Control) by sequence — within-subject inferential
#     [DEFAULT PACKAGE — within-subject paired contrasts]
# ---------------------------------------------------------------------------
# Adds 95% CI, Cohen dz, and paired p to the condition-by-sequence summary.
# Every row is a within-subject paired contrast.  The subgroup rows are
# computed fresh from analysis_data.rds by subsetting on intervention_period,
# then running a within-group paired t-test.  This is NOT a between-group
# comparison and NOT a sequence-moderation or interaction test.
#
# Rows:
#   Overall              — from 03_primary_contrasts.csv (already computed)
#   AI in Period 1 group — intervention_period == 1 → within-group paired
#   AI in Period 2 group — intervention_period == 2 → within-group paired
# ---------------------------------------------------------------------------
.rows_8f <- lapply(runs, function(r) {
  run_name  <- r[["name"]]
  run_label <- r[["label"]] %||% run_name

  rows <- list()

  # — Overall row (from pre-computed contrasts CSV) —
  contrasts_f <- file.path(outputs_dir, run_name, "tables", "primary",
                            "03_primary_contrasts.csv")
  if (file.exists(contrasts_f)) {
    contrasts <- tryCatch(read.csv(contrasts_f, stringsAsFactors = FALSE,
                                   check.names = FALSE),
                          error = function(e) NULL)
    if (!is.null(contrasts)) {
      int_row <- contrasts[
        grepl("Intervention vs Control", contrasts$Contrast, fixed = TRUE) &
        grepl("restricted",              contrasts$Contrast, fixed = TRUE),
        , drop = FALSE]
      if (nrow(int_row) > 0) {
        pr <- int_row[1L, ]
        rows[[1L]] <- data.frame(
          "Exclusion Variant" = run_label,
          "Group"             = "Overall",
          "n"                 = pr[["N"]],
          "AI Mean (SD)"      = pr[["Mean A (SD)"]],
          "Control Mean (SD)" = pr[["Mean B (SD)"]],
          "AI \u2212 Control" = pr[["Mean Diff"]],
          "95% CI"            = pr[["95% CI"]],
          "Cohen dz"          = pr[["Cohen dz"]],
          "paired p"          = .add_leading_zero(sub("^=\\s*", "", as.character(pr[["p"]]))),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
  }

  # — Subgroup rows (from analysis_data.rds, within-group paired contrasts) —
  rds_f <- file.path(outputs_dir, run_name, "rds", "analysis_data.rds")
  if (file.exists(rds_f)) {
    dat_r <- tryCatch(readRDS(rds_f), error = function(e) NULL)
    if (!is.null(dat_r)) {
      for (.grp in list(
        list(period = 1L, label = "AI in Period 1 (Intervention-first)"),
        list(period = 2L, label = "AI in Period 2 (Control-first)")
      )) {
        sub_dat <- dat_r[dat_r[["intervention_period"]] == .grp$period, ,
                         drop = FALSE]
        int_col <- if ("intervention_score_restricted" %in% names(sub_dat))
          "intervention_score_restricted" else "intervention_score_full"
        ctl_col <- if ("control_score_restricted" %in% names(sub_dat))
          "control_score_restricted" else "control_score_full"
        if (!int_col %in% names(sub_dat) || !ctl_col %in% names(sub_dat)) next
        st <- .paired_contrast(sub_dat[[int_col]], sub_dat[[ctl_col]])
        if (is.null(st)) next
        rows[[length(rows) + 1L]] <- data.frame(
          "Exclusion Variant" = run_label,
          "Group"             = .grp$label,
          "n"                 = st$n,
          "AI Mean (SD)"      = .fmt_ms_sub(st$m_a, st$sd_a),
          "Control Mean (SD)" = .fmt_ms_sub(st$m_b, st$sd_b),
          "AI \u2212 Control" = sprintf("%.3f", st$m_diff),
          "95% CI"            = .fmt_ci_sub(st$ci_lo, st$ci_hi),
          "Cohen dz"          = sprintf("%.3f", st$dz),
          "paired p"          = .fmt_p_sub(st$p),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
})
.rows_8f <- Filter(Negate(is.null), .rows_8f)
if (length(.rows_8f) > 0) {
  .df_8f    <- do.call(rbind, .rows_8f)
  .notes_8f <- Filter(Negate(is.null), c(
    "Supporting table. Secondary to variant_comparison_overall_results (primary sensitivity analysis).",
    "Within-subject paired contrasts (AI vs. Control) labelled by randomised sequence group.",
    "Restricted scoring used throughout.",
    "Overall row: from primary contrasts table (03_primary_contrasts.csv).",
    "Subgroup rows: computed from analysis_data.rds — within-group paired t-test only.",
    "Subgroup rows are NOT a between-group test, NOT a moderation test, NOT a sequence interaction.",
    "AI in Period 1 = Intervention-first participants (received AI in Period 1).",
    "AI in Period 2 = Control-first participants (received AI in Period 2).",
    .EXCL_NOTE,
    .identical_at_precision_note(.df_8f)
  ))
  .sidecar_8f <- c(
    "# Variant Comparison: AI vs. Control Paired Contrasts by Sequence Group (Inferential)",
    "",
    "**Display title:** Exclusion Variant Comparison \u2014 AI vs.\u00a0Control Effect by Sequence Group (Inferential)",
    "",
    "**Suggested manuscript title:** Within-Subject AI vs.\u00a0Control Paired Contrast by Sequence Group and Exclusion Variant",
    "",
    "**Suggested caption:** Within-subject paired contrasts (AI vs.\u00a0Control) for the overall",
    "sample and within each sequence group, shown for each item-exclusion variant. Restricted",
    "scoring throughout. The Overall row is from the pre-computed primary contrasts table;",
    "subgroup rows are paired t-tests computed within each sequence group independently.",
    "These are not between-group comparisons.",
    "",
    "## What it shows",
    "",
    "Paired contrasts (mean difference, 95%\u00a0CI, Cohen\u00a0dz, p-value) for AI vs.\u00a0Control within",
    "each sequence group (Overall / AI in Period\u00a01 / AI in Period\u00a02), replicated for each",
    "exclusion variant. Allows you to see whether the AI vs.\u00a0Control effect is similar across",
    "sequence groups and robust to the Y6 exclusion decision.",
    "",
    "## How to interpret it",
    "",
    "The Overall row is the primary result. The subgroup rows are confirmatory/contextual: if",
    "both sequence groups show a similar direction and magnitude, the result is not driven by",
    "one group. Compare across exclusion variants to assess robustness. Subgroup n values are",
    "approximately half the total, so CIs will be wider.",
    "",
    "## What it does not show / Limitations",
    "",
    "- Subgroup rows are NOT a between-group comparison (not testing whether the two sequence",
    "  groups differ from each other).",
    "- Subgroup rows are NOT a sequence moderation or interaction test.",
    "- Subgroup sample sizes are approximately half the overall N; interpret wide CIs accordingly.",
    "- A sequence \u00d7 condition interaction requires a formal test not provided here.",
    "",
    "## Important notes",
    "",
    "- Overall row: sourced from 03_primary_contrasts.csv (pre-computed primary analysis).",
    "- Subgroup rows: computed from analysis_data.rds \u2014 within-group paired t-test only.",
    "- \u201cAI in Period\u00a01\u201d = Intervention-first participants (AI in Period\u00a01, Control in Period\u00a02).",
    "- \u201cAI in Period\u00a02\u201d = Control-first participants (Control in Period\u00a01, AI in Period\u00a02).",
    "- Restricted scoring throughout; Y1 excluded in both variants, Y6 additionally in second.",
    "- If subgroup n < ~15, interpret effect sizes and p-values with caution.",
    "",
    "## Classification",
    "",
    "- **Type:** Supporting / Supplementary (secondary to variant_comparison_overall_results)",
    "- **Source file(s):** tables/primary/03_primary_contrasts.csv (Overall row),",
    "  rds/analysis_data.rds (subgroup rows) \u2014 per run"
  )
  save_comp_table(.df_8f,
                  "variant_comparison_subgroup_condition",
                  notes = .notes_8f, sidecar = .sidecar_8f)
} else {
  message("[WARN] variant_comparison_subgroup_condition: no rows — table not written.")
}

# ---------------------------------------------------------------------------
# 8g. Period effect by sequence — within-subject inferential
#     [DEFAULT PACKAGE — within-subject paired contrasts]
# ---------------------------------------------------------------------------
# Paired (P2 − P1) contrast within each sequence subgroup.
# Every row is a within-subject paired contrast (not a between-group test).
# Quantifies the period/practice effect separately for participants who
# received AI in Period 1 vs those who received it in Period 2.
#
# Rows:
#   Overall              — from 03_primary_contrasts.csv (already computed)
#   AI in Period 1 group — intervention_period == 1 → P2 − P1 paired
#   AI in Period 2 group — intervention_period == 2 → P2 − P1 paired
# ---------------------------------------------------------------------------
.rows_8g <- lapply(runs, function(r) {
  run_name  <- r[["name"]]
  run_label <- r[["label"]] %||% run_name

  rows <- list()

  # — Overall row (from pre-computed contrasts CSV) —
  contrasts_f <- file.path(outputs_dir, run_name, "tables", "primary",
                            "03_primary_contrasts.csv")
  if (file.exists(contrasts_f)) {
    contrasts <- tryCatch(read.csv(contrasts_f, stringsAsFactors = FALSE,
                                   check.names = FALSE),
                          error = function(e) NULL)
    if (!is.null(contrasts)) {
      per_row <- contrasts[
        grepl("Period 2 vs Period 1", contrasts$Contrast, fixed = TRUE) &
        grepl("restricted",           contrasts$Contrast, fixed = TRUE),
        , drop = FALSE]
      if (nrow(per_row) > 0) {
        pr <- per_row[1L, ]
        rows[[1L]] <- data.frame(
          "Exclusion Variant" = run_label,
          "Group"             = "Overall",
          "n"                 = pr[["N"]],
          "P2 Mean (SD)"      = pr[["Mean A (SD)"]],
          "P1 Mean (SD)"      = pr[["Mean B (SD)"]],
          "P2 \u2212 P1"      = pr[["Mean Diff"]],
          "95% CI"            = pr[["95% CI"]],
          "Cohen dz"          = pr[["Cohen dz"]],
          "paired p"          = .add_leading_zero(sub("^=\\s*", "", as.character(pr[["p"]]))),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
  }

  # — Subgroup rows (from analysis_data.rds, within-group paired contrasts) —
  rds_f <- file.path(outputs_dir, run_name, "rds", "analysis_data.rds")
  if (file.exists(rds_f)) {
    dat_r <- tryCatch(readRDS(rds_f), error = function(e) NULL)
    if (!is.null(dat_r)) {
      for (.grp in list(
        list(period = 1L, label = "AI in Period 1 (Intervention-first)"),
        list(period = 2L, label = "AI in Period 2 (Control-first)")
      )) {
        sub_dat <- dat_r[dat_r[["intervention_period"]] == .grp$period, ,
                         drop = FALSE]
        p2_col <- if ("period2_score_restricted" %in% names(sub_dat))
          "period2_score_restricted" else "period2_score_full"
        p1_col <- if ("period1_score_restricted" %in% names(sub_dat))
          "period1_score_restricted" else "period1_score_full"
        if (!p2_col %in% names(sub_dat) || !p1_col %in% names(sub_dat)) next
        st <- .paired_contrast(sub_dat[[p2_col]], sub_dat[[p1_col]])  # P2 - P1
        if (is.null(st)) next
        rows[[length(rows) + 1L]] <- data.frame(
          "Exclusion Variant" = run_label,
          "Group"             = .grp$label,
          "n"                 = st$n,
          "P2 Mean (SD)"      = .fmt_ms_sub(st$m_a, st$sd_a),
          "P1 Mean (SD)"      = .fmt_ms_sub(st$m_b, st$sd_b),
          "P2 \u2212 P1"      = sprintf("%.3f", st$m_diff),
          "95% CI"            = .fmt_ci_sub(st$ci_lo, st$ci_hi),
          "Cohen dz"          = sprintf("%.3f", st$dz),
          "paired p"          = .fmt_p_sub(st$p),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
})
.rows_8g <- Filter(Negate(is.null), .rows_8g)
if (length(.rows_8g) > 0) {
  .df_8g    <- do.call(rbind, .rows_8g)
  .notes_8g <- Filter(Negate(is.null), c(
    "Supporting table. Secondary to variant_comparison_overall_results (primary sensitivity analysis).",
    "Within-subject paired period effect (P2 \u2212 P1) labelled by randomised sequence group.",
    "Restricted scoring used throughout.",
    "Overall row: from primary contrasts table (03_primary_contrasts.csv).",
    "Subgroup rows: computed from analysis_data.rds — within-group paired t-test only.",
    "Subgroup rows are NOT a between-group test, NOT a moderation test, NOT a sequence interaction.",
    "A positive P2 \u2212 P1 value indicates higher scores in Period 2 (consistent with practice effect).",
    "AI in Period 1 = Intervention-first participants (received AI in Period 1).",
    "AI in Period 2 = Control-first participants (received AI in Period 2).",
    .EXCL_NOTE,
    .identical_at_precision_note(.df_8g)
  ))
  .sidecar_8g <- c(
    "# Variant Comparison: Period Effect (P2 \u2212 P1) by Sequence Group (Inferential)",
    "",
    "**Display title:** Exclusion Variant Comparison \u2014 Period Effect by Sequence Group (Inferential)",
    "",
    "**Suggested manuscript title:** Within-Subject Period Effect by Sequence Group and Exclusion Variant",
    "",
    "**Suggested caption:** Within-subject Period\u00a02 minus Period\u00a01 paired contrasts for the overall",
    "sample and within each sequence group, shown for each item-exclusion variant. Restricted",
    "scoring throughout. A positive P2\u00a0\u2212\u00a0P1 value indicates higher scores in Period\u00a02 (consistent",
    "with practice or ordering effects). These are not between-group comparisons.",
    "",
    "## What it shows",
    "",
    "Paired (P2\u00a0\u2212\u00a0P1) contrasts (mean difference, 95%\u00a0CI, Cohen\u00a0dz, p-value) for the overall",
    "sample and within each sequence group, replicated for each exclusion variant. Allows you",
    "to see whether the period/practice effect differs across sequence groups and whether it",
    "is robust to the Y6 exclusion decision.",
    "",
    "## How to interpret it",
    "",
    "The Overall row summarises the period effect for the full sample. The subgroup rows show",
    "whether the period effect is similar in size and direction for participants who experienced",
    "AI vs.\u00a0Control in Period\u00a01. This is informative because participants in different sequence",
    "groups carry different condition assignments into Period\u00a02, which may differentially",
    "contribute to practice or carryover effects.",
    "",
    "## What it does not show / Limitations",
    "",
    "- Subgroup rows are NOT a between-group comparison (not testing whether the two sequence",
    "  groups differ in their period effects).",
    "- Subgroup rows are NOT a period \u00d7 sequence interaction test.",
    "- Subgroup sample sizes are approximately half the overall N; CIs will be wider.",
    "- Does not distinguish practice effects from carryover or fatigue effects.",
    "",
    "## Important notes",
    "",
    "- Overall row: sourced from 03_primary_contrasts.csv (pre-computed primary analysis).",
    "- Subgroup rows: computed from analysis_data.rds \u2014 within-group paired t-test only.",
    "- \u201cAI in Period\u00a01\u201d = Intervention-first participants (AI in Period\u00a01, Control in Period\u00a02).",
    "- \u201cAI in Period\u00a02\u201d = Control-first participants (Control in Period\u00a01, AI in Period\u00a02).",
    "- A positive P2\u00a0\u2212\u00a0P1 value: participants scored higher overall in Period\u00a02 (regardless of",
    "  which condition was assigned to Period\u00a02 in each group).",
    "- Restricted scoring throughout; Y1 excluded in both variants, Y6 additionally in second.",
    "",
    "## Classification",
    "",
    "- **Type:** Supporting / Supplementary (secondary to variant_comparison_overall_results)",
    "- **Source file(s):** tables/primary/03_primary_contrasts.csv (Overall row),",
    "  rds/analysis_data.rds (subgroup rows) \u2014 per run"
  )
  save_comp_table(.df_8g,
                  "variant_comparison_subgroup_period",
                  notes = .notes_8g, sidecar = .sidecar_8g)
} else {
  message("[WARN] variant_comparison_subgroup_period: no rows — table not written.")
}

# ---------------------------------------------------------------------------
# §8h. Reliability comparison across exclusion variants [SUPPORTING]
# Reads psychometrics/reliability_summary_full.csv from each run and stacks
# with an "Exclusion Variant" column prepended.  Columns: Exclusion Variant,
# Form, N, Items, KR-20, Cronbach alpha, McDonald omega (total), Mean inter-item r,
# Split-half (S-B).  Useful for checking whether item exclusions materially
# change internal consistency.
# ---------------------------------------------------------------------------
{
  .rows_8h <- lapply(runs, function(r) {
    run_name  <- r[["name"]]
    run_label <- r[["label"]] %||% run_name

    f <- file.path(outputs_dir, run_name, "tables", "psychometrics",
                   "reliability_summary_full.csv")
    if (!file.exists(f)) {
      message("[WARN] reliability_summary_full.csv not found (8h) for '", run_name, "'")
      return(NULL)
    }
    df <- tryCatch(
      read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) { message("[WARN] ", e$message); NULL }
    )
    if (is.null(df) || nrow(df) == 0L) return(NULL)

    # Select the columns we want, tolerating missing optional ones
    .want <- c("Form", "N", "Items", "KR-20", "Cronbach alpha",
               "McDonald omega (total)", "Mean inter-item r", "Split-half (S-B)")
    .have <- intersect(.want, names(df))
    df    <- df[, .have, drop = FALSE]

    # Prepend variant label
    cbind(`Exclusion Variant` = run_label, df, stringsAsFactors = FALSE)
  })

  .df_8h <- do.call(rbind, Filter(Negate(is.null), .rows_8h))

  if (!is.null(.df_8h) && nrow(.df_8h) > 0L) {
    # Replace NA omega values with NA† to signal non-estimability explicitly
    .omega_col <- "McDonald omega (total)"
    if (.omega_col %in% names(.df_8h)) {
      .df_8h[[.omega_col]] <- ifelse(
        is.na(.df_8h[[.omega_col]]),
        "NA\u2020",
        as.character(.df_8h[[.omega_col]])
      )
    }
    .sidecar_8h <- c(
      "# Reliability Comparison Across Exclusion Variants",
      "",
      "## Display title",
      "",
      "Table 8h: Internal Consistency by Exclusion Variant",
      "",
      "## Question answered",
      "",
      "Do different item exclusion sets produce materially different internal consistency",
      "estimates for Form X and Form Y?",
      "",
      "## What it shows",
      "",
      "KR-20, Cronbach alpha, McDonald omega (total), mean inter-item r, and Spearman-Brown",
      "split-half reliability side by side for each exclusion variant. Both full-scoring and",
      "restricted-scoring rows are included where applicable.",
      "",
      "## How to interpret it",
      "",
      "If KR-20 / alpha values are similar across variants, the exclusion decision has little",
      "impact on scale reliability. A notable increase in the restricted variant suggests the",
      "excluded item(s) were reducing scale coherence, providing psychometric justification",
      "for the exclusion.",
      "",
      "## What it does not show / Limitations",
      "",
      "- Does not test whether the reliability difference between variants is statistically",
      "  significant.",
      "- \u2020 McDonald omega could not be estimated; the single-factor model did not converge",
      "  under this sample size and item-response structure. KR-20 is the recommended reliability estimate.",
      "",
      "## Classification",
      "",
      "- **Type:** Supplementary / psychometric context",
      "- **Source file(s):** tables/psychometrics/reliability_summary_full.csv (per run)"
    )
    save_comp_table(.df_8h, "variant_comparison_reliability",
                    notes = .sidecar_8h, sidecar = .sidecar_8h)
  } else {
    message("[WARN] variant_comparison_reliability: no rows — table not written.")
  }
}

cat("\nComparison tables written to: ", .comp_tbl_dir, "\n\n", sep = "")

# --- Output audit for the comparison directory ---
{
  .audit_path <- file.path(r_dir, "09_audit.R")
  if (file.exists(.audit_path))
    tryCatch(source(.audit_path, echo = FALSE),
             error = function(e)
               cat("[WARN] Audit script failed:", conditionMessage(e), "\n"))
}

