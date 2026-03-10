## =============================================================================
## R/01_data_import.R
## Load CSV files, validate structure, standardize column names, save raw data.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

# Locate 00_setup.R — works both when run directly (Rscript 01_*.R) and when
# sourced from run_all.R.  R_SCRIPTS_DIR is set by run_all.R; the frame search
# is the fallback for direct execution.
.script_dir <- local({
  d <- Sys.getenv("R_SCRIPTS_DIR", unset = "")
  if (nzchar(d)) return(d)
  for (i in rev(seq_along(sys.frames()))) {
    f <- sys.frames()[[i]]$ofile
    if (!is.null(f) && nzchar(f))
      return(dirname(normalizePath(f, winslash = "/")))
  }
  normalizePath("R", winslash = "/")  # last resort: cwd must be project root
})
source(file.path(.script_dir, "00_setup.R"))
log_h1("01  DATA IMPORT")

cfg <- read_config()

# =============================================================================
# LOCATE CSV FILES
# =============================================================================
# Paths can be overridden individually via environment variables or config.
# Fall back to DATA_DIR for the standard file names.

.locate <- function(env_var, filename) {
  path <- Sys.getenv(env_var, unset = "")
  if (nzchar(path) && file.exists(path)) return(path)
  data_file(filename)
}

assignment_path <- .locate("CSV_ASSIGNMENT",   "assignment.csv")
x_path          <- .locate("CSV_POSTTEST_X",   "posttest_x_items.csv")
y_path          <- .locate("CSV_POSTTEST_Y",   "posttest_y_items.csv")

log_line("assignment.csv  -> ", assignment_path)
log_line("posttest_x      -> ", x_path)
log_line("posttest_y      -> ", y_path)

for (.req in c(assignment_path, x_path, y_path)) {
  if (!file.exists(.req)) stop("Required file not found: ", .req)
}

# =============================================================================
# READ & CLEAN ASSIGNMENT
# =============================================================================

assignment_raw <- readr::read_csv(assignment_path, show_col_types = FALSE,
                                   name_repair = "minimal") |>
  janitor::clean_names()

log_check("assignment columns: ", paste(names(assignment_raw), collapse = ", "))

# Remove email column if present (PII — must not flow into analysis objects)
# Save a participant→email map to InternalUse/ first so it is preserved.
if ("email" %in% names(assignment_raw)) {
  .email_map <- dplyr::select(assignment_raw,
    participant = dplyr::any_of(c("participant_id", "participant")),
    email)
  .email_map_path <- out_path("InternalUse", "participant_email_map.csv")
  tryCatch({
    readr::write_csv(.email_map, .email_map_path)
    log_line("Email map saved -> ", .email_map_path)
  }, error = function(e) {
    log_warn("Could not save email map: ", conditionMessage(e))
  })
  log_warn("Email column found in assignment.csv — removed from analysis; map saved to InternalUse/.")
  assignment_raw <- dplyr::select(assignment_raw, -email)
}

# Standardize participant ID -> "participant"
assignment <- standardize_participant_id(assignment_raw, "assignment.csv")

# Standardize order column names -> intervention_order, form_x_order, form_y_order
assignment <- standardize_assignment_cols(assignment)

# Warn about any columns that aren't part of the expected assignment schema
.expected_assign_cols <- c("participant", "intervention_order", "control_order",
                            "form_x_order", "form_y_order")
.extra_assign_cols <- setdiff(names(assignment), .expected_assign_cols)
if (length(.extra_assign_cols) > 0) {
  log_warn("Unexpected column(s) in assignment.csv (not used by pipeline): ",
           paste(.extra_assign_cols, collapse = ", "),
           " -- Check README for expected column names.")
}

# Parse order strings to integer periods
assignment <- assignment |>
  dplyr::mutate(
    intervention_period = order_to_period(.data$intervention_order),
    control_period      = order_to_period(.data$control_order),
    form_x_period       = order_to_period(.data$form_x_order),
    form_y_period       = order_to_period(.data$form_y_order)
  )

bad_rows <- dplyr::filter(assignment,
  is.na(.data$intervention_period) |
  is.na(.data$control_period) |
  is.na(.data$form_x_period) |
  is.na(.data$form_y_period))

if (nrow(bad_rows) > 0) {
  stop("Could not parse order values for participant(s): ",
       paste(bad_rows$participant, collapse = ", "),
       "\nExpected '1st' or '2nd' in order columns.")
}

log_check("assignment rows=", nrow(assignment),
          " | intervention_period: ",
          paste(assignment$intervention_period, collapse = " "))

# =============================================================================
# READ & CLEAN X ITEMS
# =============================================================================

x_raw <- readr::read_csv(x_path, show_col_types = FALSE,
                          name_repair = "minimal") |>
  janitor::clean_names()

log_check("X items columns: ", paste(names(x_raw), collapse = ", "))

x_items <- standardize_participant_id(x_raw, "posttest_x_items.csv")

# Detect and parse time column
time_col_cfg <- tolower(cfg$columns$time_taken %||% "time_taken")
if (time_col_cfg %in% names(x_items)) {
  x_items <- x_items |>
    dplyr::mutate(time_taken_x_sec = parse_time_taken(.data[[time_col_cfg]])) |>
    dplyr::select(-dplyr::all_of(time_col_cfg))
  log_check("X time parsed: range ", min(x_items$time_taken_x_sec, na.rm = TRUE),
            "–", max(x_items$time_taken_x_sec, na.rm = TRUE), " seconds")
}

x_cols <- get_question_cols_ordered(x_items, "x")
if (length(x_cols) == 0) stop("No X item columns found (expected x1, x2, ...) in: ", x_path)
log_check("X item columns (", length(x_cols), "): ", paste(head(x_cols, 5), collapse = ", "),
          if (length(x_cols) > 5) "..." else "")

# Warn about unexpected columns (not participant, time, or x-item columns)
.ok_x_cols <- c("participant", "time_taken_x_sec", x_cols)
.extra_x   <- setdiff(names(x_items), .ok_x_cols)
if (length(.extra_x) > 0) {
  log_warn("Unexpected column(s) in posttest_x_items.csv (not used by pipeline): ",
           paste(.extra_x, collapse = ", "),
           " -- Check README for expected column names.")
}

# Validate binary
.non_binary_x <- sapply(x_cols, function(col) {
  vals <- na.omit(x_items[[col]])
  !all(vals %in% c(0, 1))
})
if (any(.non_binary_x)) {
  log_warn("Non-binary values in X items: ", paste(x_cols[.non_binary_x], collapse = ", "))
}

# =============================================================================
# READ & CLEAN Y ITEMS
# =============================================================================

y_raw <- readr::read_csv(y_path, show_col_types = FALSE,
                          name_repair = "minimal") |>
  janitor::clean_names()

log_check("Y items columns: ", paste(names(y_raw), collapse = ", "))

y_items <- standardize_participant_id(y_raw, "posttest_y_items.csv")

if (time_col_cfg %in% names(y_items)) {
  y_items <- y_items |>
    dplyr::mutate(time_taken_y_sec = parse_time_taken(.data[[time_col_cfg]])) |>
    dplyr::select(-dplyr::all_of(time_col_cfg))
  log_check("Y time parsed: range ", min(y_items$time_taken_y_sec, na.rm = TRUE),
            "–", max(y_items$time_taken_y_sec, na.rm = TRUE), " seconds")
}

y_cols <- get_question_cols_ordered(y_items, "y")
if (length(y_cols) == 0) stop("No Y item columns found (expected y1, y2, ...) in: ", y_path)
log_check("Y item columns (", length(y_cols), "): ", paste(head(y_cols, 5), collapse = ", "),
          if (length(y_cols) > 5) "..." else "")

# Warn about unexpected columns
.ok_y_cols <- c("participant", "time_taken_y_sec", y_cols)
.extra_y   <- setdiff(names(y_items), .ok_y_cols)
if (length(.extra_y) > 0) {
  log_warn("Unexpected column(s) in posttest_y_items.csv (not used by pipeline): ",
           paste(.extra_y, collapse = ", "),
           " -- Check README for expected column names.")
}

.non_binary_y <- sapply(y_cols, function(col) {
  vals <- na.omit(y_items[[col]])
  !all(vals %in% c(0, 1))
})
if (any(.non_binary_y)) {
  log_warn("Non-binary values in Y items: ", paste(y_cols[.non_binary_y], collapse = ", "))
}

# =============================================================================
# CHECK PARTICIPANT ALIGNMENT
# =============================================================================

ids_assign <- sort(unique(assignment$participant))
ids_x      <- sort(unique(x_items$participant))
ids_y      <- sort(unique(y_items$participant))

if (!identical(ids_assign, ids_x) || !identical(ids_assign, ids_y)) {
  log_warn("Participant ID mismatch across files:")
  log_warn("  assignment.csv: n=", length(ids_assign),
           " | in_x: n=", length(ids_x),
           " | in_y: n=", length(ids_y))
  only_assign <- setdiff(ids_assign, union(ids_x, ids_y))
  only_x      <- setdiff(ids_x, ids_assign)
  only_y      <- setdiff(ids_y, ids_assign)
  if (length(only_assign)) log_warn("  Only in assignment: ", paste(only_assign, collapse = ", "))
  if (length(only_x))      log_warn("  Only in X items:    ", paste(only_x, collapse = ", "))
  if (length(only_y))      log_warn("  Only in Y items:    ", paste(only_y, collapse = ", "))
  # Proceed on intersection
  common <- Reduce(intersect, list(ids_assign, ids_x, ids_y))
  assignment <- dplyr::filter(assignment, .data$participant %in% common)
  x_items    <- dplyr::filter(x_items,   .data$participant %in% common)
  y_items    <- dplyr::filter(y_items,   .data$participant %in% common)
  log_warn("  Continuing on ", length(common), " common participants.")
}

log_check("Final N = ", nrow(assignment))

# =============================================================================
# APPLY ITEM EXCLUSIONS
# =============================================================================
# Exclusions defined in config create a "restricted" item set (e.g., for
# problematic/invalid items). Full and restricted scores are both computed
# downstream; this just stores which items to exclude.

x_excluded <- tolower(as.character(cfg$item_exclusions$x %||% character(0)))
y_excluded <- tolower(as.character(cfg$item_exclusions$y %||% character(0)))

if (length(x_excluded) > 0) {
  log_line("X item exclusions (from config): ", paste(x_excluded, collapse = ", "))
  invalid <- setdiff(x_excluded, x_cols)
  if (length(invalid)) log_warn("Excluded X items not found in data: ", paste(invalid, collapse = ", "))
}
if (length(y_excluded) > 0) {
  log_line("Y item exclusions (from config): ", paste(y_excluded, collapse = ", "))
  invalid <- setdiff(y_excluded, y_cols)
  if (length(invalid)) log_warn("Excluded Y items not found in data: ", paste(invalid, collapse = ", "))
}

x_cols_restricted <- setdiff(x_cols, x_excluded)
y_cols_restricted <- setdiff(y_cols, y_excluded)

# =============================================================================
# SAVE
# =============================================================================

raw_data <- list(
  assignment         = assignment,
  x_items            = x_items,
  y_items            = y_items,
  x_cols_full        = x_cols,
  x_cols_restricted  = x_cols_restricted,
  y_cols_full        = y_cols,
  y_cols_restricted  = y_cols_restricted,
  x_excluded         = x_excluded,
  y_excluded         = y_excluded
)

save_rds(raw_data, "raw_data")

log_h2("IMPORT COMPLETE")
log_line("  N participants   : ", nrow(assignment))
log_line("  X items (full)   : ", length(x_cols), " | restricted: ", length(x_cols_restricted))
log_line("  Y items (full)   : ", length(y_cols), " | restricted: ", length(y_cols_restricted))
