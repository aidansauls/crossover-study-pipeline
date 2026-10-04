## =============================================================================
## R/09_audit.R
## Compatibility shim for the retired standalone audit-output stage.
##
## Run logging is now handled centrally by R/run_all.R + R/00_setup.R:
##   outputs/<study>/extra/logs/<timestamp>_run.log
##   outputs/<study>/extra/logs/latest_run.log
##   outputs/<study>/extra/logs/effective_config.yml
##   outputs/<study>/extra/logs/session_info.txt
##
## This file intentionally creates no AUDIT.csv, AUDIT.md, run_audit/, or
## run_provenance/ trees. It remains in the repository only so older commands
## that source R/09_audit.R fail gracefully rather than recreating legacy output.
## =============================================================================

.script_dir <- local({
  d <- Sys.getenv("R_SCRIPTS_DIR", unset = "")
  if (nzchar(d)) {
    return(d)
  }
  for (i in rev(seq_along(sys.frames()))) {
    f <- sys.frames()[[i]]$ofile
    if (!is.null(f) && nzchar(f)) {
      return(dirname(normalizePath(f, winslash = "/")))
    }
  }
  normalizePath("R", winslash = "/", mustWork = FALSE)
})

source(file.path(.script_dir, "00_setup.R"), echo = FALSE)

log_h1("09  AUDIT OUTPUT — RETIRED")
log_line("No standalone audit tree is generated.")
log_line("Use extra/logs/latest_run.log for the terminal-style run transcript.")
log_line("Resolved settings are stored in extra/logs/effective_config.yml.")
log_line("R session details are stored in extra/logs/session_info.txt.")

invisible(TRUE)
