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
fig_groups   <- comp[["figure_groups"]] %||% "all"
common_scale <- isTRUE(comp[["common_scale"]])

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
# 3. Common-scale re-render (optional)
# ---------------------------------------------------------------------------
# .figs_src is the subdirectory name used inside each run's outputs/ folder.
.figs_src <- "figures"

if (common_scale) {
  # 3a. Compute global score_y_lo across all runs' data
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

  # 3b. Re-render each run's figures into figures_comparison/ with shared scale.
  #     The original figures/ dirs are NOT touched.
  rscript_bin    <- file.path(R.home("bin"),
                               if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  run_all_script <- file.path(r_dir, "run_all.R")

  for (r in runs) {
    cfg_rel <- r[["config"]]
    if (is.null(cfg_rel)) {
      warning("No 'config:' entry for run '", r[["name"]], "' — skipping re-render.\n",
              "  Add 'config: config/<your_file>.yml' to the comparison YAML.")
      next
    }
    cfg_abs <- if (file.exists(cfg_rel)) cfg_rel else file.path(proj_root, cfg_rel)
    if (!file.exists(cfg_abs)) {
      warning("Config not found: ", cfg_abs, " — skipping re-render for '", r[["name"]], "'")
      next
    }

    cat("Re-rendering :", r[["name"]], "...\n")

    # Save env vars we will temporarily override
    .saved_env <- Sys.getenv(c("PIPELINE_CONFIG", "STUDY_NAME", "ANALYSIS_MODULES",
                                "REUSE_DATA", "FIGURES_ROOT_SUFFIX", "SCORE_Y_LO_OVERRIDE"),
                              names = TRUE)

    Sys.setenv(
      PIPELINE_CONFIG     = normalizePath(cfg_abs, winslash = "/"),
      STUDY_NAME          = r[["name"]],
      ANALYSIS_MODULES    = "figures",
      REUSE_DATA          = "1",
      FIGURES_ROOT_SUFFIX = "figures_comparison",
      SCORE_Y_LO_OVERRIDE = as.character(global_y_lo)
    )

    exit_code <- system2(rscript_bin, args = normalizePath(run_all_script, winslash = "/"),
                         stdout = "", stderr = "")

    # Restore env vars (Sys.setenv requires named args, so use do.call)
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
# 4. Discover common figure subfolders / PNGs
# ---------------------------------------------------------------------------
first_fig_dir  <- file.path(outputs_dir, runs[[1]][["name"]], .figs_src)
all_subfolders <- sort(list.dirs(first_fig_dir, full.names = FALSE, recursive = FALSE))
all_subfolders <- all_subfolders[nzchar(all_subfolders)]

if (!identical(fig_groups, "all") && is.character(fig_groups) && length(fig_groups) > 0)
  all_subfolders <- intersect(all_subfolders, fig_groups)

if (length(all_subfolders) == 0)
  stop("No figure subfolders found under: ", first_fig_dir)

cat("Subfolders :", paste(all_subfolders, collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 5. Create comparison output directory
# ---------------------------------------------------------------------------
out_root <- file.path(outputs_dir, out_name, "figures")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 6. Build comparison panels figure by figure
# ---------------------------------------------------------------------------
n_created <- 0L
n_skipped <- 0L

for (subfolder in all_subfolders) {

  file_sets <- lapply(runs, function(r) {
    d <- file.path(outputs_dir, r[["name"]], .figs_src, subfolder)
    if (!dir.exists(d)) return(character(0))
    basename(list.files(d, pattern = "\\.png$", full.names = FALSE))
  })

  common_files <- Reduce(intersect, file_sets)
  if (length(common_files) == 0) {
    cat("[SKIP]", subfolder, "— no common files across all runs\n")
    next
  }

  out_sub <- file.path(out_root, subfolder)
  dir.create(out_sub, recursive = TRUE, showWarnings = FALSE)

  for (fig_file in common_files) {

    panel_imgs <- lapply(runs, function(r) {
      path <- file.path(outputs_dir, r[["name"]], .figs_src, subfolder, fig_file)
      img  <- tryCatch(magick::image_read(path), error = function(e) NULL)
      if (is.null(img)) return(NULL)

      info  <- magick::image_info(img)
      w     <- as.integer(info[["width"]])
      h_hdr <- max(50L, as.integer(label_size * 2L))

      header <- magick::image_blank(w, h_hdr, color = "white")
      header <- magick::image_annotate(header, text = r[["label"]],
                                        gravity = "Center", size = label_size,
                                        color = "black", font = "sans")
      magick::image_append(c(header, img), stack = TRUE)
    })

    panel_imgs <- Filter(Negate(is.null), panel_imgs)
    if (length(panel_imgs) == 0) {
      cat("[SKIP]", subfolder, "/", fig_file, "— all files missing\n")
      n_skipped <- n_skipped + 1L
      next
    }

    infos <- lapply(panel_imgs, magick::image_info)

    if (layout == "side_by_side") {
      min_h      <- min(vapply(infos, function(i) as.integer(i[["height"]]), integer(1)))
      panel_imgs <- lapply(panel_imgs, function(img) magick::image_scale(img, paste0("x", min_h)))
      combined   <- magick::image_append(do.call(c, panel_imgs), stack = FALSE)
    } else {
      min_w      <- min(vapply(infos, function(i) as.integer(i[["width"]]),  integer(1)))
      panel_imgs <- lapply(panel_imgs, function(img) magick::image_scale(img, paste0(min_w, "x")))
      combined   <- magick::image_append(do.call(c, panel_imgs), stack = TRUE)
    }

    magick::image_write(combined, path = file.path(out_sub, fig_file), density = dpi)
    cat("[OK] ", subfolder, "/", fig_file, "\n", sep = "")
    n_created <- n_created + 1L
  }
}

# ---------------------------------------------------------------------------
# 7. Summary
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 72), "\n", sep = "")
cat(sprintf("  Done: %d comparison figures created, %d skipped\n  Output: %s\n",
            n_created, n_skipped, out_root))
if (common_scale)
  cat(sprintf("  Common y-axis lower bound: %d\n", global_y_lo))
cat(strrep("=", 72), "\n")

# ---------------------------------------------------------------------------
# 8. Cross-run restricted-score summary table
# ---------------------------------------------------------------------------
# Reads standard pipeline tables from each run and assembles one row per run:
#   outputs/<output_name>/tables/variant_comparison_restricted_results.csv
.tbl_rows <- lapply(runs, function(r) {
  run_name  <- r[["name"]]
  run_label <- r[["label"]] %||% run_name

  # -- Primary contrasts (03_primary_contrasts.csv) ---------------------------
  contrasts_f <- file.path(outputs_dir, run_name,
                            "tables", "primary", "03_primary_contrasts.csv")
  if (!file.exists(contrasts_f)) {
    message("[WARN] Primary contrasts table not found for '", run_name, "' — skipping row")
    return(NULL)
  }
  contrasts <- tryCatch(
    read.csv(contrasts_f, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) { message("[WARN] ", e$message); NULL }
  )
  if (is.null(contrasts)) return(NULL)

  int_row <- contrasts[grepl("Intervention vs Control", contrasts$Contrast, fixed = TRUE) &
                         grepl("restricted", contrasts$Contrast, fixed = TRUE), , drop = FALSE]
  if (nrow(int_row) == 0) {
    message("[WARN] No restricted 'Intervention vs Control' row in '", run_name, "'")
    return(NULL)
  }
  int_row <- int_row[1, ]

  # -- Mixed model results (07_mixed_model_results.csv) -----------------------
  mm_f <- file.path(outputs_dir, run_name,
                    "tables", "mixed_models", "07_mixed_model_results.csv")
  cond_est <- NA_real_
  cond_p   <- NA_character_
  if (file.exists(mm_f)) {
    mm <- tryCatch(
      read.csv(mm_f, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(mm)) {
      mm_row <- mm[tolower(mm$Scoring) == "restricted" &
                     grepl("condition_fac", mm$Term, fixed = TRUE), , drop = FALSE]
      if (nrow(mm_row) > 0) {
        cond_est <- mm_row$Estimate[1]
        cond_p   <- mm_row$p[1]
      }
    }
  } else {
    message("[WARN] Mixed model table not found for '", run_name, "'")
  }

  data.frame(
    "Run"                    = run_label,
    "N"                      = int_row[["N"]],
    "Intervention Mean (SD)" = int_row[["Mean A (SD)"]],
    "Control Mean (SD)"      = int_row[["Mean B (SD)"]],
    "Mean Diff"              = int_row[["Mean Diff"]],
    "95% CI"                 = int_row[["95% CI"]],
    "Cohen dz"               = int_row[["Cohen dz"]],
    "p"                      = int_row[["p"]],
    "MM Condition Estimate"  = cond_est,
    "MM Condition p"         = cond_p,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
})

.tbl_rows <- Filter(Negate(is.null), .tbl_rows)

if (length(.tbl_rows) > 0) {
  tbl_out_dir <- file.path(outputs_dir, out_name, "tables")
  dir.create(tbl_out_dir, recursive = TRUE, showWarnings = FALSE)
  tbl_path <- file.path(tbl_out_dir, "variant_comparison_restricted_results.csv")
  write.csv(do.call(rbind, .tbl_rows), tbl_path, row.names = FALSE, quote = TRUE)
  cat("\nCross-run restricted-score summary table:\n  ", tbl_path, "\n\n", sep = "")
} else {
  message("[WARN] No data rows collected — variant_comparison_restricted_results.csv not written.")
}

# --- Output audit for the comparison directory ---
{
  .audit_path <- file.path(r_dir, "09_audit.R")
  if (file.exists(.audit_path))
    tryCatch(source(.audit_path, echo = FALSE),
             error = function(e)
               cat("[WARN] Audit script failed:", conditionMessage(e), "\n"))
}

