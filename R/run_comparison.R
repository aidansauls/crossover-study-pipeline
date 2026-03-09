## =============================================================================
## R/run_comparison.R
## Standalone orchestrator for comparison panel figures.
##
## Usage (PowerShell):
##   $env:COMPARISON_CONFIG = "config\comparison_pilot.yml"
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" R\run_comparison.R
##
## Or use RunPipeline.bat option [9] for the interactive launcher.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Locate project root from the --file= argument
# ---------------------------------------------------------------------------
.args      <- commandArgs(trailingOnly = FALSE)
.this_file <- sub("^--file=", "", .args[startsWith(.args, "--file=")])

if (length(.this_file) > 0 && nzchar(.this_file[1])) {
  r_dir     <- dirname(normalizePath(.this_file[1], winslash = "/"))
  proj_root <- dirname(r_dir)
  if (basename(r_dir) != "R") {
    # Script is inside R/; proj_root was already set correctly above
    # but handle the edge case of running from elsewhere
    proj_root <- r_dir
    r_dir     <- file.path(r_dir, "R")
  }
} else {
  # Interactive sourcing — assume cwd is the project root
  r_dir     <- normalizePath(file.path(".", "R"), winslash = "/")
  proj_root <- normalizePath(".", winslash = "/")
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

source(file.path(r_dir, "08_comparison_figures.R"))
