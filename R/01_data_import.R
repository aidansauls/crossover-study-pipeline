## =============================================================================
## R/01_data_import.R
## Load CSV files, detect/remove direct identifiers, validate structure,
## standardize column names, and save deidentified raw analysis objects.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
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
  normalizePath("R", winslash = "/")
})
source(file.path(.script_dir, "00_setup.R"))
log_h1("01  DATA IMPORT")

cfg <- read_config()

# =============================================================================
# LOCATE CSV FILES
# =============================================================================

.locate <- function(env_var, filename) {
  path <- Sys.getenv(env_var, unset = "")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  data_file(filename)
}

assignment_path <- .locate("CSV_ASSIGNMENT", "assignment.csv")
x_path <- .locate("CSV_POSTTEST_X", "posttest_x_items.csv")
y_path <- .locate("CSV_POSTTEST_Y", "posttest_y_items.csv")

log_line("assignment.csv  -> ", assignment_path)
log_line("posttest_x      -> ", x_path)
log_line("posttest_y      -> ", y_path)

for (.req in c(assignment_path, x_path, y_path)) {
  if (!file.exists(.req)) stop("Required file not found: ", .req)
}

# =============================================================================
# READ SOURCE FILES
# =============================================================================
# clean_names() is applied before identifier detection so the detector does not
# depend on capitalization, spaces, or punctuation in the original headers.

assignment_source <- readr::read_csv(
  assignment_path,
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  janitor::clean_names()

x_source <- readr::read_csv(
  x_path,
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  janitor::clean_names()

y_source <- readr::read_csv(
  y_path,
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  janitor::clean_names()

# =============================================================================
# DIRECT-IDENTIFIER DETECTION / DEIDENTIFICATION
# =============================================================================
# The analysis objects must never retain obvious direct identifiers.  If direct
# identifiers are detected, the output gains:
#
#   extra/data/identified/      exact source CSVs + optional ID mapping
#   extra/data/deidentified/    analysis-safe CSVs
#   extra/data/deidentification/manifest.csv
#
# If no direct identifiers are detected, extra/data is not created at all.
# A generic participant_id is treated as a study code, not PII.  Explicit names
# such as student_id, email, name, DOB, MRN, etc. are treated as direct IDs.

.direct_identifier_names <- unique(c(
  "email", "email_address", "e_mail",
  "name", "full_name", "first_name", "middle_name", "last_name",
  "student_id", "studentid", "student_number", "student_number_id",
  "osu_id", "university_id", "school_id", "employee_id",
  "medical_record_number", "medical_record_no", "mrn",
  "date_of_birth", "dob", "birth_date",
  "phone", "phone_number", "mobile", "mobile_number",
  "address", "street_address", "home_address",
  janitor::make_clean_names(as.character(unlist(
    cfg$privacy$additional_direct_identifier_columns %||% character(),
    use.names = FALSE
  )))
))

.is_direct_identifier <- function(nm) {
  nm <- tolower(as.character(nm))
  nm %in% .direct_identifier_names ||
    grepl("(^|_)(email|phone|mobile|mrn|medical_record)(_|$)", nm) ||
    grepl("(^|_)(first|last|full)_?name($|_)", nm) ||
    grepl("(^|_)(student|university|school)_?id($|_)", nm) ||
    grepl("(^|_)(date_of_birth|birth_date|dob)($|_)", nm)
}

.detect_ids <- function(df) {
  names(df)[vapply(names(df), .is_direct_identifier, logical(1))]
}

.id_cols <- list(
  assignment = .detect_ids(assignment_source),
  posttest_x = .detect_ids(x_source),
  posttest_y = .detect_ids(y_source)
)

.any_direct_ids <- any(lengths(.id_cols) > 0L)

# Determine which source column functions as the participant key. If that key
# itself is a direct identifier (e.g., student_id), pseudonymize it rather than
# dropping it so joins remain possible.
.configured_id <- janitor::make_clean_names(
  cfg$columns$participant_id %||% "participant_id"
)
.participant_key_candidates <- unique(c(
  .configured_id, "participant_id", "participant", "id", "subject_id", "subject"
))

.find_participant_key <- function(df) {
  hit <- intersect(.participant_key_candidates, names(df))
  if (length(hit)) hit[[1L]] else NA_character_
}

.assignment_key <- .find_participant_key(assignment_source)
.x_key <- .find_participant_key(x_source)
.y_key <- .find_participant_key(y_source)

.key_is_direct <- !is.na(.assignment_key) && .is_direct_identifier(.assignment_key)

.pseudonym_map <- NULL
if (.key_is_direct) {
  source_ids <- as.character(assignment_source[[.assignment_key]])
  unique_ids <- unique(source_ids)
  .pseudonym_map <- data.frame(
    original_id = unique_ids,
    participant_code = seq_along(unique_ids),
    stringsAsFactors = FALSE
  )
}

.deidentify_frame <- function(df, participant_key = NA_character_) {
  out <- df
  id_cols <- .detect_ids(out)

  if (!is.na(participant_key) && participant_key %in% names(out) &&
    .is_direct_identifier(participant_key) && !is.null(.pseudonym_map)) {
    idx <- match(as.character(out[[participant_key]]), .pseudonym_map$original_id)
    if (any(is.na(idx))) {
      stop("A direct participant identifier could not be mapped during deidentification.")
    }
    out[[participant_key]] <- .pseudonym_map$participant_code[idx]
    id_cols <- setdiff(id_cols, participant_key)
  }

  if (length(id_cols)) {
    out <- dplyr::select(out, -dplyr::all_of(id_cols))
  }
  out
}

if (.any_direct_ids) {
  id_root <- file.path(out_path(), "extra", "data")
  identified_dir <- file.path(id_root, "identified")
  deidentified_dir <- file.path(id_root, "deidentified")
  deid_meta_dir <- file.path(id_root, "deidentification")

  dir.create(identified_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(deidentified_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(deid_meta_dir, recursive = TRUE, showWarnings = FALSE)

  # Preserve the exact supplied source files only because direct identifiers were
  # detected. These files stay under extra/data/identified and are never used by
  # downstream analysis after this point.
  file.copy(assignment_path, file.path(identified_dir, "assignment.csv"), overwrite = TRUE)
  file.copy(x_path, file.path(identified_dir, "posttest_x_items.csv"), overwrite = TRUE)
  file.copy(y_path, file.path(identified_dir, "posttest_y_items.csv"), overwrite = TRUE)

  if (!is.null(.pseudonym_map)) {
    readr::write_csv(.pseudonym_map, file.path(identified_dir, "participant_id_mapping.csv"))
  }

  assignment_source <- .deidentify_frame(assignment_source, .assignment_key)
  x_source <- .deidentify_frame(x_source, .x_key)
  y_source <- .deidentify_frame(y_source, .y_key)

  readr::write_csv(assignment_source, file.path(deidentified_dir, "assignment.csv"))
  readr::write_csv(x_source, file.path(deidentified_dir, "posttest_x_items.csv"))
  readr::write_csv(y_source, file.path(deidentified_dir, "posttest_y_items.csv"))

  .deid_manifest <- data.frame(
    source_file = c("assignment.csv", "posttest_x_items.csv", "posttest_y_items.csv"),
    detected_direct_identifier_columns = c(
      paste(.id_cols$assignment, collapse = "; "),
      paste(.id_cols$posttest_x, collapse = "; "),
      paste(.id_cols$posttest_y, collapse = "; ")
    ),
    participant_key_pseudonymized = c(
      .key_is_direct,
      !is.na(.x_key) && .is_direct_identifier(.x_key),
      !is.na(.y_key) && .is_direct_identifier(.y_key)
    ),
    stringsAsFactors = FALSE
  )
  readr::write_csv(
    .deid_manifest,
    file.path(deid_meta_dir, "manifest.csv")
  )

  log_warn(
    "Direct identifiers detected. Identified source copies and deidentified analysis copies were written under extra/data/."
  )
  for (.nm in names(.id_cols)) {
    if (length(.id_cols[[.nm]])) {
      log_line("  ", .nm, " direct identifiers: ", paste(.id_cols[[.nm]], collapse = ", "))
    }
  }
} else {
  log_check("No obvious direct-identifier columns detected; extra/data was not created.")
}

# From this point forward, *_source objects are deidentified when needed.
assignment_raw <- assignment_source
x_raw <- x_source
y_raw <- y_source

# =============================================================================
# CLEAN ASSIGNMENT
# =============================================================================

log_check("assignment columns: ", paste(names(assignment_raw), collapse = ", "))

assignment <- standardize_participant_id(assignment_raw, "assignment.csv")
assignment <- standardize_assignment_cols(assignment)

.expected_assign_cols <- c(
  "participant", "intervention_order", "control_order",
  "form_x_order", "form_y_order"
)
.extra_assign_cols <- setdiff(names(assignment), .expected_assign_cols)
if (length(.extra_assign_cols) > 0) {
  log_warn(
    "Unexpected column(s) in assignment.csv (not used by pipeline): ",
    paste(.extra_assign_cols, collapse = ", "),
    " -- Check README for expected column names."
  )
}

assignment <- assignment |>
  dplyr::mutate(
    intervention_period = order_to_period(.data$intervention_order),
    control_period = order_to_period(.data$control_order),
    form_x_period = order_to_period(.data$form_x_order),
    form_y_period = order_to_period(.data$form_y_order)
  )

bad_rows <- dplyr::filter(
  assignment,
  is.na(.data$intervention_period) |
    is.na(.data$control_period) |
    is.na(.data$form_x_period) |
    is.na(.data$form_y_period)
)

if (nrow(bad_rows) > 0) {
  stop(
    "Could not parse order values for participant(s): ",
    paste(bad_rows$participant, collapse = ", "),
    "\nExpected '1st' or '2nd' in order columns."
  )
}

log_check(
  "assignment rows=", nrow(assignment),
  " | intervention_period: ",
  paste(assignment$intervention_period, collapse = " ")
)

# =============================================================================
# CLEAN X ITEMS
# =============================================================================

log_check("X items columns: ", paste(names(x_raw), collapse = ", "))
x_items <- standardize_participant_id(x_raw, "posttest_x_items.csv")

time_col_cfg <- tolower(cfg$columns$time_taken %||% "time_taken")
if (time_col_cfg %in% names(x_items)) {
  x_items <- x_items |>
    dplyr::mutate(time_taken_x_sec = parse_time_taken(.data[[time_col_cfg]])) |>
    dplyr::select(-dplyr::all_of(time_col_cfg))
  log_check(
    "X time parsed: range ",
    min(x_items$time_taken_x_sec, na.rm = TRUE), "–",
    max(x_items$time_taken_x_sec, na.rm = TRUE), " seconds"
  )
}

x_cols <- get_question_cols_ordered(x_items, "x")
if (length(x_cols) == 0) {
  stop("No X item columns found (expected x1, x2, ...) in: ", x_path)
}
log_check(
  "X item columns (", length(x_cols), "): ",
  paste(head(x_cols, 5), collapse = ", "),
  if (length(x_cols) > 5) "..." else ""
)

.ok_x_cols <- c("participant", "time_taken_x_sec", x_cols)
.extra_x <- setdiff(names(x_items), .ok_x_cols)
if (length(.extra_x) > 0) {
  log_warn(
    "Unexpected column(s) in posttest_x_items.csv (not used by pipeline): ",
    paste(.extra_x, collapse = ", "),
    " -- Check README for expected column names."
  )
}

.non_binary_x <- sapply(x_cols, function(col) {
  vals <- na.omit(x_items[[col]])
  !all(vals %in% c(0, 1))
})
if (any(.non_binary_x)) {
  log_warn("Non-binary values in X items: ", paste(x_cols[.non_binary_x], collapse = ", "))
}

# =============================================================================
# CLEAN Y ITEMS
# =============================================================================

log_check("Y items columns: ", paste(names(y_raw), collapse = ", "))
y_items <- standardize_participant_id(y_raw, "posttest_y_items.csv")

if (time_col_cfg %in% names(y_items)) {
  y_items <- y_items |>
    dplyr::mutate(time_taken_y_sec = parse_time_taken(.data[[time_col_cfg]])) |>
    dplyr::select(-dplyr::all_of(time_col_cfg))
  log_check(
    "Y time parsed: range ",
    min(y_items$time_taken_y_sec, na.rm = TRUE), "–",
    max(y_items$time_taken_y_sec, na.rm = TRUE), " seconds"
  )
}

y_cols <- get_question_cols_ordered(y_items, "y")
if (length(y_cols) == 0) {
  stop("No Y item columns found (expected y1, y2, ...) in: ", y_path)
}
log_check(
  "Y item columns (", length(y_cols), "): ",
  paste(head(y_cols, 5), collapse = ", "),
  if (length(y_cols) > 5) "..." else ""
)

.ok_y_cols <- c("participant", "time_taken_y_sec", y_cols)
.extra_y <- setdiff(names(y_items), .ok_y_cols)
if (length(.extra_y) > 0) {
  log_warn(
    "Unexpected column(s) in posttest_y_items.csv (not used by pipeline): ",
    paste(.extra_y, collapse = ", "),
    " -- Check README for expected column names."
  )
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
ids_x <- sort(unique(x_items$participant))
ids_y <- sort(unique(y_items$participant))

if (!identical(ids_assign, ids_x) || !identical(ids_assign, ids_y)) {
  log_warn("Participant ID mismatch across files:")
  log_warn(
    "  assignment.csv: n=", length(ids_assign),
    " | in_x: n=", length(ids_x),
    " | in_y: n=", length(ids_y)
  )
  only_assign <- setdiff(ids_assign, union(ids_x, ids_y))
  only_x <- setdiff(ids_x, ids_assign)
  only_y <- setdiff(ids_y, ids_assign)
  if (length(only_assign)) log_warn("  Only in assignment: ", paste(only_assign, collapse = ", "))
  if (length(only_x)) log_warn("  Only in X items:    ", paste(only_x, collapse = ", "))
  if (length(only_y)) log_warn("  Only in Y items:    ", paste(only_y, collapse = ", "))

  common <- Reduce(intersect, list(ids_assign, ids_x, ids_y))
  assignment <- dplyr::filter(assignment, .data$participant %in% common)
  x_items <- dplyr::filter(x_items, .data$participant %in% common)
  y_items <- dplyr::filter(y_items, .data$participant %in% common)
  log_warn("  Continuing on ", length(common), " common participants.")
}

log_check("Final N = ", nrow(assignment))

# =============================================================================
# APPLY ITEM EXCLUSIONS
# =============================================================================

x_excluded <- tolower(as.character(cfg$item_exclusions$x %||% character(0)))
y_excluded <- tolower(as.character(cfg$item_exclusions$y %||% character(0)))

if (length(x_excluded) > 0) {
  log_line("X item exclusions (from config): ", paste(x_excluded, collapse = ", "))
  invalid <- setdiff(x_excluded, x_cols)
  if (length(invalid)) {
    log_warn("Excluded X items not found in data: ", paste(invalid, collapse = ", "))
  }
}
if (length(y_excluded) > 0) {
  log_line("Y item exclusions (from config): ", paste(y_excluded, collapse = ", "))
  invalid <- setdiff(y_excluded, y_cols)
  if (length(invalid)) {
    log_warn("Excluded Y items not found in data: ", paste(invalid, collapse = ", "))
  }
}

x_cols_restricted <- setdiff(x_cols, x_excluded)
y_cols_restricted <- setdiff(y_cols, y_excluded)

# =============================================================================
# SAVE DEIDENTIFIED ANALYSIS INPUT OBJECT
# =============================================================================

raw_data <- list(
  assignment = assignment,
  x_items = x_items,
  y_items = y_items,
  x_cols_full = x_cols,
  x_cols_restricted = x_cols_restricted,
  y_cols_full = y_cols,
  y_cols_restricted = y_cols_restricted,
  x_excluded = x_excluded,
  y_excluded = y_excluded,
  direct_identifiers_detected = .any_direct_ids,
  direct_identifier_columns_detected = .id_cols,
  privacy_schema_version = 1L
)

save_rds(raw_data, "raw_data")

log_h2("IMPORT COMPLETE")
log_line("  N participants   : ", nrow(assignment))
log_line("  X items (full)   : ", length(x_cols), " | restricted: ", length(x_cols_restricted))
log_line("  Y items (full)   : ", length(y_cols), " | restricted: ", length(y_cols_restricted))
