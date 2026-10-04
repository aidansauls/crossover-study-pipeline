## =============================================================================
## R/run_all.R
## Master pipeline runner.
##
## Standard output layout:
##   outputs/<study>/manuscript_selected/
##   outputs/<study>/extra/
##
## Reviewer mode (PIPELINE_MODE=reviewer) runs only the analysis needed to
## reproduce manuscript_selected and removes non-manuscript generated artifacts
## after successful assembly, leaving the run transcript under extra/logs/.
## =============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

.modules_raw <- Sys.getenv("ANALYSIS_MODULES", unset = "all")
if (trimws(tolower(.modules_raw)) == "list") {
  cat("Available pipeline modules:\n")
  cat("  import          01_data_import.R\n")
  cat("  scores          02_score_calculation.R\n")
  cat("  psychometrics   03_psychometrics.R\n")
  cat("  analyses        04_analyses.R\n")
  cat("  figures         05_figures.R\n")
  cat("  tables          06_tables.R\n")
  cat("  demographics    07_demographics.R\n")
  cat("  manuscript      10_manuscript_selected.R\n")
  cat("\nStandard full run: ANALYSIS_MODULES=all\n")
  cat("Reviewer run: set PIPELINE_MODE=reviewer\n")
  quit(save = "no", status = 0)
}

# =============================================================================
# PROJECT ROOT
# =============================================================================

this_file <- tryCatch(
  normalizePath(
    sub(
      "^--file=", "",
      commandArgs(trailingOnly = FALSE)[
        startsWith(commandArgs(trailingOnly = FALSE), "--file=")
      ][1]
    ),
    winslash = "/",
    mustWork = FALSE
  ),
  error = function(e) NA_character_
)

if (is.na(this_file) || !nzchar(this_file)) {
  r_dir <- file.path(getwd(), "R")
} else {
  r_dir <- dirname(this_file)
}
if (basename(r_dir) != "R") r_dir <- file.path(r_dir, "R")

.project_root_override <- Sys.getenv("PIPELINE_PROJECT_ROOT", unset = "")
proj_root <- normalizePath(
  if (nzchar(.project_root_override)) .project_root_override else dirname(r_dir),
  winslash = "/",
  mustWork = FALSE
)

R_script <- function(name) file.path(r_dir, name)
Sys.setenv(R_SCRIPTS_DIR = r_dir)

# Load helpers before doing anything else.
source(R_script("00_setup.R"), echo = FALSE)

.pipeline_mode <- tolower(trimws(Sys.getenv("PIPELINE_MODE", unset = "standard")))
.reviewer_mode <- identical(.pipeline_mode, "reviewer")
.reuse_data <- identical(Sys.getenv("REUSE_DATA", unset = "0"), "1")

# A normal new run starts from a clean study output folder. A REUSE_DATA run must
# preserve analysis_objects, so it does not clean the root first.
initialize_output_tree(clean = !.reuse_data)

# Start one transcript that captures essentially the same text shown in terminal.
log_start()

cat(strrep("=", 72), "\n")
cat("  CROSSOVER STUDY ANALYSIS PIPELINE\n")
cat("  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat(strrep("=", 72), "\n\n")
cat("Project root : ", proj_root, "\n", sep = "")
cat("Study        : ", STUDY_NAME, "\n", sep = "")
cat("Mode         : ", .pipeline_mode, "\n", sep = "")
cat("Modules      : ", .modules_raw, "\n\n", sep = "")

# =============================================================================
# MODULE PLAN
# =============================================================================

.rds_ready <- file.exists(out_path("rds", "analysis_data.rds")) &&
  file.exists(out_path("rds", "raw_data.rds"))
.skip_source <- .reuse_data && .rds_ready

if (.reuse_data && .rds_ready) {
  cat("[REUSE] Existing analysis objects found; import + scores will be skipped.\n\n")
} else if (.reuse_data && !.rds_ready) {
  cat("[REUSE] Requested, but required analysis objects are missing; import + scores will run.\n\n")
}

PIPELINE_STEPS <- list(
  list(id = "import", script = "01_data_import.R", source_stage = TRUE),
  list(id = "scores", script = "02_score_calculation.R", source_stage = TRUE),
  list(id = "psychometrics", script = "03_psychometrics.R", source_stage = FALSE),
  list(id = "analyses", script = "04_analyses.R", source_stage = FALSE),
  list(id = "figures", script = "05_figures.R", source_stage = FALSE),
  list(id = "tables", script = "06_tables.R", source_stage = FALSE),
  list(id = "demographics", script = "07_demographics.R", source_stage = FALSE)
)

.modules <- tolower(trimws(strsplit(.modules_raw, ",")[[1]]))
.run_all_modules <- "all" %in% .modules

if (.reviewer_mode) {
  # 05_figures produces the four source figures that are promoted into the final
  # manuscript-selected set. Non-selected outputs are deleted after assembly.
  .requested <- c("import", "scores", "psychometrics", "analyses", "figures")
} else if (.run_all_modules) {
  .requested <- vapply(PIPELINE_STEPS, `[[`, character(1), "id")
} else {
  .requested <- unique(c("import", "scores", .modules))
}

if (.skip_source) {
  .requested <- setdiff(.requested, c("import", "scores"))
}

steps_to_run <- Filter(
  function(s) s$id %in% .requested,
  PIPELINE_STEPS
)

cat(
  "Steps to run: ",
  if (length(steps_to_run)) {
    paste(vapply(steps_to_run, `[[`, character(1), "id"), collapse = ", ")
  } else {
    "none"
  },
  "\n\n",
  sep = ""
)

# =============================================================================
# RUN MODULES — FAIL FAST
# =============================================================================

.pipeline_results <- list()
.start_all <- Sys.time()

.run_one <- function(step) {
  path <- R_script(step$script)
  cat(strrep("-", 72), "\n")
  cat("RUNNING: ", step$script, "\n", sep = "")
  cat(strrep("-", 72), "\n")

  if (!file.exists(path)) {
    stop("Required script not found: ", path)
  }

  t0 <- Sys.time()
  tryCatch(
    withCallingHandlers(
      source(path, echo = FALSE),
      warning = function(w) {
        cat("[WARN] ", conditionMessage(w), "\n", sep = "")
        if (exists("session_record_warning", envir = .GlobalEnv)) {
          session_record_warning(conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        cat(conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
      .pipeline_results[[step$id]] <<- paste0("ERROR (", elapsed, "s): ", conditionMessage(e))
      if (exists("session_record_module", envir = .GlobalEnv)) {
        session_record_module(step$id, "ERROR", elapsed)
      }
      if (exists("session_record_error", envir = .GlobalEnv)) {
        session_record_error(paste0(step$script, ": ", conditionMessage(e)))
      }
      cat("\n[ERROR] ", step$script, " failed:\n  ", conditionMessage(e), "\n\n", sep = "")
      stop(e)
    }
  )

  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  .pipeline_results[[step$id]] <<- paste0("OK (", elapsed, "s)")
  if (exists("session_record_module", envir = .GlobalEnv)) {
    session_record_module(step$id, "OK", elapsed)
  }
  cat("\n[OK] ", step$script, " completed in ", elapsed, "s\n\n", sep = "")
  invisible(TRUE)
}

.run_failed <- FALSE
.run_error <- NULL

tryCatch(
  {
    for (step in steps_to_run) .run_one(step)
  },
  error = function(e) {
    .run_failed <<- TRUE
    .run_error <<- e
  }
)

if (.run_failed) {
  cat(strrep("=", 72), "\n")
  cat("  PIPELINE FAILED\n")
  cat(strrep("=", 72), "\n")
  cat("The run stopped at the first failed module.\n")
  cat("Error: ", conditionMessage(.run_error), "\n", sep = "")
  cat("Log:   ", out_path("logs", "latest_run.log"), "\n", sep = "")
  log_stop()
  quit(save = "no", status = 1)
}

# =============================================================================
# MANUSCRIPT-SELECTED ASSEMBLY
# =============================================================================
# A full run always builds the manuscript-selected set. Custom module runs do so
# when explicitly requested. Reviewer mode always does so.

.run_manuscript <- .reviewer_mode || .run_all_modules || "manuscript" %in% .modules

if (.run_manuscript) {
  manuscript_path <- R_script("10_manuscript_selected.R")
  cat(strrep("-", 72), "\n")
  cat("RUNNING: 10_manuscript_selected.R\n")
  cat(strrep("-", 72), "\n")

  .manuscript_error <- NULL
  tryCatch(
    source(manuscript_path, echo = FALSE),
    error = function(e) {
      .manuscript_error <<- e
      if (exists("session_record_error", envir = .GlobalEnv)) {
        session_record_error(paste0("10_manuscript_selected.R: ", conditionMessage(e)))
      }
    }
  )

  if (!is.null(.manuscript_error)) {
    cat("\n[ERROR] Manuscript-selected assembly failed:\n  ",
      conditionMessage(.manuscript_error), "\n",
      sep = ""
    )
    log_stop()
    quit(save = "no", status = 1)
  }
}

# =============================================================================
# FINAL RUN RECORD
# =============================================================================

if (exists("write_effective_config", envir = .GlobalEnv)) {
  try(write_effective_config(), silent = TRUE)
}

try(
  writeLines(
    capture.output(utils::sessionInfo()),
    out_path("logs", "session_info.txt"),
    useBytes = TRUE
  ),
  silent = TRUE
)

# Reviewer output should contain only manuscript_selected plus a compact extra/
# log route. Analysis objects and non-selected plots were temporary build inputs.
if (.reviewer_mode) {
  for (.rel in c("figures", "tables", "analysis_objects", "comparison_figures")) {
    .path <- file.path(out_path(), "extra", .rel)
    if (dir.exists(.path)) unlink(.path, recursive = TRUE, force = TRUE)
  }
}

.total_secs <- round(as.numeric(difftime(Sys.time(), .start_all, units = "secs")), 1)

.main_count <- if (dir.exists(out_path("manuscript_selected"))) {
  length(list.files(out_path("manuscript_selected"), recursive = TRUE, full.names = TRUE))
} else {
  0L
}
.extra_fig_count <- if (dir.exists(file.path(out_path(), "extra", "figures"))) {
  length(list.files(file.path(out_path(), "extra", "figures"), pattern = "\\.png$", recursive = TRUE))
} else {
  0L
}
.extra_table_count <- if (dir.exists(file.path(out_path(), "extra", "tables"))) {
  length(list.files(file.path(out_path(), "extra", "tables"), pattern = "\\.csv$", recursive = TRUE))
} else {
  0L
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("  PIPELINE COMPLETE\n")
cat(strrep("=", 72), "\n")
cat("  Study              : ", STUDY_NAME, "\n", sep = "")
cat("  Mode               : ", .pipeline_mode, "\n", sep = "")
cat("  Manuscript files   : ", .main_count, "\n", sep = "")
cat("  Extra figures      : ", .extra_fig_count, "\n", sep = "")
cat("  Extra table CSVs   : ", .extra_table_count, "\n", sep = "")
cat("  Total time         : ", .total_secs, "s\n", sep = "")
cat("\n  Output routes:\n")
cat("    ", out_path("manuscript_selected"), "\n", sep = "")
cat("    ", file.path(out_path(), "extra"), "\n", sep = "")
cat("\n")

log_stop()
quit(save = "no", status = 0)
