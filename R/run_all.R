## =============================================================================
## R/run_all.R
## Master pipeline runner. Sources each analysis script in order.
## Module selection is controlled by the ANALYSIS_MODULES environment variable
## (set by RunPipeline.bat or manually).
##
## Usage:
##   Rscript R/run_all.R
##   ANALYSIS_MODULES=psychometrics,primary Rscript R/run_all.R
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# Handle special invocations before anything else
.modules_raw <- Sys.getenv("ANALYSIS_MODULES", unset = "all")
if (trimws(tolower(.modules_raw)) == "list") {
  cat("Available pipeline modules:\n")
  cat("  import          01_data_import.R          (always runs unless --reuse-data)\n")
  cat("  scores          02_score_calculation.R    (always runs unless --reuse-data)\n")
  cat("  psychometrics   03_psychometrics.R        (opt-in)\n")
  cat("  analyses        04_analyses.R             (opt-in)\n")
  cat("  figures         05_figures.R              (opt-in)\n")
  cat("  tables          06_tables.R               (opt-in)\n")
  cat("  demographics    07_demographics.R         (opt-in; also needs demographics.generate: true in config)\n")
  cat("\nSet ANALYSIS_MODULES=all to run everything, or comma-separate IDs, e.g.:\n")
  cat("  ANALYSIS_MODULES=psychometrics,analyses,figures,tables\n")
  cat("\nSet REUSE_DATA=1 to skip import+scores when RDS files already exist.\n")
  quit(save = "no", status = 0)
}

# --- Locate R directory ---
this_file <- tryCatch(
  normalizePath(commandArgs(trailingOnly = FALSE)[
    startsWith(commandArgs(trailingOnly = FALSE), "--file=")
  ] |> sub("^--file=", "", x = _),
  winslash = "/", mustWork = FALSE),
  error = function(e) NA_character_
)

if (is.na(this_file) || !nzchar(this_file)) {
  r_dir <- suppressWarnings(
    tryCatch(
      dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE)),
      error = function(e) file.path(getwd(), "R")
    )
  )
} else {
  r_dir <- dirname(this_file)
}

# Ensure r_dir points to the R/ folder inside the project root
if (basename(r_dir) != "R") r_dir <- file.path(r_dir, "R")
proj_root <- dirname(r_dir)

R_script <- function(name) file.path(r_dir, name)

# --- Banner ---
cat(strrep("=", 72), "\n")
cat("  CROSSOVER STUDY ANALYSIS PIPELINE\n")
cat("  Copyright (c) 2026 Aidan Sauls\n")
cat("  Free to use with attribution in published work -- see LICENSE\n")
cat("  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 72), "\n\n")
cat("Project root : ", proj_root, "\n")
cat("R scripts    : ", r_dir,    "\n")
cat("Study        : ", Sys.getenv("STUDY_NAME",       unset = "(from config)"), "\n")
cat("Data dir     : ", Sys.getenv("STUDY_DATA_PATH",  unset = "(from config)"), "\n")
cat("Config       : ", Sys.getenv("PIPELINE_CONFIG",  unset = "(default)"),     "\n")
cat("Modules      : ", Sys.getenv("ANALYSIS_MODULES", unset = "all"),            "\n\n")

# --- Reuse-data logic ---
# Set env var REUSE_DATA=1 to skip import+scores when RDS files already exist.
# Useful when re-running only downstream modules (figures, tables, etc.)
.reuse_data  <- Sys.getenv("REUSE_DATA", unset = "0") == "1"
# Study name: prefer explicit env var; fall back to reading config YAML directly
# so the RDS path check works even when STUDY_NAME is not set in the shell.
.study_name  <- Sys.getenv("STUDY_NAME", unset = "")
if (!nzchar(.study_name)) {
  .cfg_path_early <- Sys.getenv("PIPELINE_CONFIG", unset = "config/study_config.yml")
  if (!file.exists(.cfg_path_early))
    .cfg_path_early <- file.path(proj_root, .cfg_path_early)
  .study_name <- tryCatch({
    .sn <- yaml::read_yaml(.cfg_path_early)[["study"]][["name"]]
    if (is.null(.sn) || !nzchar(.sn)) "my_study" else .sn
  }, error = function(e) "my_study")
}
.out_dir     <- file.path(proj_root, "outputs", .study_name)
.rds_exists  <- file.exists(file.path(.out_dir, "rds", "analysis_data.rds")) &&
                file.exists(file.path(.out_dir, "rds", "raw_data.rds"))
.skip_source <- .reuse_data && .rds_exists

if (.reuse_data && .rds_exists) {
  cat("[REUSE] REUSE_DATA=1 and RDS files found — skipping import + scores.\n\n")
} else if (.reuse_data && !.rds_exists) {
  cat("[REUSE] REUSE_DATA=1 but RDS files not found — running import + scores anyway.\n\n")
}

# --- Module definitions ---
# These are run in order; each depends on the previous steps' RDS outputs.
PIPELINE_STEPS <- list(
  list(id = "import",        script = "01_data_import.R",      always = !.skip_source),
  list(id = "scores",        script = "02_score_calculation.R", always = !.skip_source),
  list(id = "psychometrics", script = "03_psychometrics.R",   always = FALSE),
  list(id = "analyses",      script = "04_analyses.R",        always = FALSE),
  list(id = "figures",       script = "05_figures.R",         always = FALSE),
  list(id = "tables",        script = "06_tables.R",          always = FALSE),
  list(id = "demographics",  script = "07_demographics.R",    always = FALSE)
)

# Parse ANALYSIS_MODULES env var
modules_env <- tolower(trimws(
  strsplit(Sys.getenv("ANALYSIS_MODULES", unset = "all"), ",")[[1]]
))
run_all_modules <- "all" %in% modules_env

# --- Determine which steps to run ---
steps_to_run <- Filter(function(s) {
  if (s$always) return(TRUE)
  run_all_modules || s$id %in% modules_env
}, PIPELINE_STEPS)

cat("Steps to run: ",
    paste(vapply(steps_to_run, `[[`, character(1), "id"), collapse = ", "), "\n\n")

# --- Run ---
.pipeline_results  <- list()
start_all <- Sys.time()

# Expose the R/ directory path to sub-scripts via env var.
# Avoids sys.frames()[[1]]$ofile being NULL when scripts are sourced rather than
# executed directly.
Sys.setenv(R_SCRIPTS_DIR = r_dir)

for (step in steps_to_run) {
  path <- R_script(step$script)
  cat(strrep("-", 72), "\n")
  cat("RUNNING: ", step$script, "\n")
  cat(strrep("-", 72), "\n")
  
  if (!file.exists(path)) {
    cat("  [SKIP] Script not found: ", path, "\n\n")
    .pipeline_results[[step$id]] <- "SKIPPED"
    if (exists("session_record_module", envir = .GlobalEnv))
      session_record_module(step$id, "SKIPPED", 0)
    next
  }
  
  t0  <- Sys.time()
  err <- NULL
  
  tryCatch(
    withCallingHandlers(
      source(path, echo = FALSE),
      warning = function(w) {
        cat("[WARN] ", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        cat(conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      err <<- e
    }
  )
  
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  
  if (!is.null(err)) {
    cat("\n[ERROR] ", step$script, " failed:\n  ", conditionMessage(err), "\n\n")
    .pipeline_results[[step$id]] <- paste0("ERROR: ", conditionMessage(err))
    if (exists("session_record_module",  envir = .GlobalEnv))
      session_record_module(step$id, "ERROR", elapsed)
    if (exists("session_record_error", envir = .GlobalEnv))
      session_record_error(paste0(step$script, ": ", conditionMessage(err)))
  } else {
    cat("\n[OK]    ", step$script, " completed in ", elapsed, "s\n\n")
    .pipeline_results[[step$id]] <- paste0("OK (", elapsed, "s)")
    if (exists("session_record_module", envir = .GlobalEnv))
      session_record_module(step$id, "OK", elapsed)
  }
}

# --- Summary ---
total_secs <- round(as.numeric(difftime(Sys.time(), start_all, units = "secs")), 1)

cat(strrep("=", 72), "\n")
cat("  PIPELINE SUMMARY\n")
cat(strrep("=", 72), "\n\n")

n_ok    <- sum(startsWith(unlist(.pipeline_results), "OK"))
n_err   <- sum(startsWith(unlist(.pipeline_results), "ERROR"))
n_skip  <- sum(unlist(.pipeline_results) == "SKIPPED")

for (nm in names(.pipeline_results)) {
  status <- .pipeline_results[[nm]]
  icon   <- if (startsWith(status, "OK")) "OK  " else
            if (startsWith(status, "ERROR")) "ERR " else "SKIP"
  cat(sprintf("  [%s]  %-22s  %s\n", icon, nm, status))
}

cat("\n")
study_name <- Sys.getenv("STUDY_NAME", unset = "study")
out_root   <- file.path(proj_root, "outputs", study_name)

if (dir.exists(out_root)) {
  n_fig <- length(list.files(file.path(out_root, "figures"),
                              pattern = "\\.png$", recursive = TRUE))
  n_csv <- length(list.files(file.path(out_root, "tables"),
                              pattern = "\\.csv$", recursive = TRUE))
  n_png <- length(list.files(file.path(out_root, "tables_png"),
                              pattern = "\\.png$", recursive = TRUE))
  cat(sprintf("  Output: %s\n", out_root))
  cat(sprintf("  Figures (PNG):  %d\n", n_fig))
  cat(sprintf("  Tables (CSV):   %d\n", n_csv))
  cat(sprintf("  Tables (PNG):   %d\n", n_png))
  cat("\n")
}

cat(sprintf("  Completed: %d OK, %d errors, %d skipped — %.1fs total\n\n",
            n_ok, n_err, n_skip, total_secs))
cat(strrep("=", 72), "\n")

# Write structured session logs (JSON + summary TXT)
if (exists("write_session_json", envir = .GlobalEnv)) {
  write_session_json(n_ok = n_ok, n_err = n_err,
                     n_skip = n_skip, total_secs = total_secs)
}
if (exists("write_session_summary_txt", envir = .GlobalEnv)) {
  write_session_summary_txt(n_ok = n_ok, n_err = n_err,
                             n_skip = n_skip, total_secs = total_secs)
}

# --- Output audit ---
{
  .audit_path <- R_script("09_audit.R")
  if (file.exists(.audit_path))
    tryCatch(source(.audit_path, echo = FALSE),
             error = function(e)
               cat("[WARN] Audit script failed:", conditionMessage(e), "\n"))
}

# Exit with error code if any step failed
if (n_err > 0) quit(status = 1, save = "no")
