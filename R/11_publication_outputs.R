## =============================================================================
## R/11_publication_outputs.R
## Compatibility stub.
##
## Publication-role organization now lives entirely in manuscript_selected/.
## The canonical non-selected output library lives in extra/.
##
## This script intentionally creates no third publication_outputs/ tree.
## It is retained only so older commands that source this file fail gracefully.
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

source(file.path(.script_dir, "00_setup.R"))

log_h1("11  PUBLICATION OUTPUT COMPATIBILITY")
log_line("No separate publication_outputs directory is created.")
log_line("Final numbered manuscript material: manuscript_selected/")
log_line("All non-selected outputs: extra/")

invisible(TRUE)
