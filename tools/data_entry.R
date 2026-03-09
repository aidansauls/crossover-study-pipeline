## =============================================================================
## tools/data_entry.R
## Interactive data entry wizard.
## Collects participant data from the console and writes properly formatted CSVs.
##
## Usage:
##   Rscript tools/data_entry.R
##   (called automatically from RunPipeline.bat mode 4)
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
## =============================================================================

options(stringsAsFactors = FALSE)

cat("\n", strrep("=", 60), "\n")
cat("  DATA ENTRY WIZARD\n")
cat("  Crossover Within-Subject Study\n")
cat(strrep("=", 60), "\n\n")

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

ask <- function(prompt, valid = NULL, default = NULL, allow_empty = FALSE) {
  repeat {
    if (!is.null(default)) {
      cat(prompt, " [default: ", default, "]: ", sep = "")
    } else {
      cat(prompt, ": ", sep = "")
    }
    raw <- readLines(con = "stdin", n = 1L)
    ans <- trimws(raw)
    if (nchar(ans) == 0 && !is.null(default)) return(default)
    if (nchar(ans) == 0 && allow_empty) return("")
    if (!is.null(valid)) {
      if (tolower(ans) %in% tolower(valid)) {
        return(ans)
      } else {
        cat("  Please enter one of: ", paste(valid, collapse = ", "), "\n")
        next
      }
    }
    if (nchar(ans) > 0) return(ans)
    cat("  Value required.\n")
  }
}

ask_int <- function(prompt, min_val = 1, max_val = Inf, default = NULL) {
  repeat {
    raw <- ask(prompt, default = as.character(default))
    n   <- suppressWarnings(as.integer(raw))
    if (!is.na(n) && n >= min_val && n <= max_val) return(n)
    cat("  Please enter an integer between ", min_val, " and ",
        min(max_val, 9999), "\n")
  }
}

ask_yn <- function(prompt, default = "y") {
  tolower(ask(prompt, valid = c("y", "n", "yes", "no"), default = default)) %in%
    c("y", "yes")
}

cat_section <- function(title) {
  cat("\n", strrep("-", 50), "\n", title, "\n", strrep("-", 50), "\n\n", sep = "")
}

# --------------------------------------------------------------------------
# STEP 1: Study metadata
# --------------------------------------------------------------------------
cat_section("STEP 1: Study Settings")

intervention_name <- ask("  Intervention label (e.g., 'AI', 'Training', 'Treatment')",
                         default = "Intervention")
control_name      <- ask("  Control label",
                         default = "Control")
form_x_name       <- ask("  Name for Form/Test X (e.g., 'Form X', 'Pre-test', 'Quiz A')",
                         default = "Form X")
form_y_name       <- ask("  Name for Form/Test Y (e.g., 'Form Y', 'Post-test', 'Quiz B')",
                         default = "Form Y")
n_questions_x     <- ask_int("  Number of questions on Form X (leave 0 for score-only entry)", 0, 200, 0)
n_questions_y     <- ask_int("  Number of questions on Form Y (leave 0 for score-only entry)", 0, 200, 0)
detailed_entry    <- (n_questions_x > 0) || (n_questions_y > 0)

include_time      <- ask_yn("  Include time-taken column? (y/n)", default = "y")

# Item exclusions (only if detailed entry)
x_excluded <- character(0)
y_excluded <- character(0)

if (detailed_entry) {
  cat("\n  Item exclusions (e.g., Y1, Y6 or X2, Y1, Y6).\n")
  cat("  Enter as comma-separated list, or press Enter to skip: ")
  raw_excl <- trimws(readLines("stdin", n = 1L))
  if (nchar(raw_excl) > 0) {
    all_excl <- tolower(trimws(strsplit(raw_excl, "[,;\\s]+", perl = TRUE)[[1]]))
    all_excl <- all_excl[nchar(all_excl) > 0]
    x_excluded <- all_excl[startsWith(all_excl, "x")]
    y_excluded <- all_excl[startsWith(all_excl, "y")]
    if (length(x_excluded)) cat("  X excluded: ", paste(x_excluded, collapse=", "), "\n")
    if (length(y_excluded)) cat("  Y excluded: ", paste(y_excluded, collapse=", "), "\n")
  }
}

# --------------------------------------------------------------------------
# STEP 2: Number of participants
# --------------------------------------------------------------------------
cat_section("STEP 2: Participant Count")
n_participants <- ask_int("  How many participants?", 1, 500)

# --------------------------------------------------------------------------
# Containers
# --------------------------------------------------------------------------
assignment_rows <- list()
x_rows          <- list()
y_rows          <- list()

# --------------------------------------------------------------------------
# STEP 3: Per-participant data
# --------------------------------------------------------------------------
cat_section("STEP 3: Per-Participant Data Entry")
cat("  For each participant you will enter:\n")
cat("    - Sequence: did they receive ", intervention_name, " or ",
    control_name, " first?\n", sep = "")
cat("    - Form order: did they take ", form_x_name, " or ", form_y_name, " first?\n", sep = "")
if (detailed_entry) {
  cat("    - Responses: 1 (correct) or 0 (incorrect) for each question\n")
} else {
  cat("    - Scores: one number per form\n")
}

for (i in seq_len(n_participants)) {
  cat("\n", strrep("·", 40), "\n")
  cat("  Participant", i, "\n")
  cat(strrep("·", 40), "\n\n")
  
  # --- Sequence ---
  cat("  Did this participant receive ", intervention_name, " FIRST or ",
      control_name, " FIRST?\n", sep = "")
  int_order_raw <- ask(
    paste0("  Enter '", intervention_name, "' or '", control_name, "'"),
    valid = c(intervention_name, control_name,
              tolower(intervention_name), tolower(control_name),
              "1", "2")
  )
  int_order <- if (tolower(int_order_raw) %in%
                     c(tolower(intervention_name), "1")) "1st" else "2nd"
  
  # --- Form order ---
  cat("  Did this participant take ", form_x_name, " FIRST or ", form_y_name, " FIRST?\n", sep = "")
  x_order_raw <- ask(
    paste0("  Enter '", form_x_name, "' or '", form_y_name, "'"),
    valid = c(form_x_name, form_y_name,
              tolower(form_x_name), tolower(form_y_name),
              "x", "y", "1", "2")
  )
  x_order <- if (tolower(x_order_raw) %in%
                   c(tolower(form_x_name), "x", "1")) "1st" else "2nd"
  y_order <- if (x_order == "1st") "2nd" else "1st"
  
  assignment_rows[[i]] <- list(
    Participant_ID       = i,
    Intervention_Order   = int_order,
    X_Order              = x_order,
    Y_Order              = y_order
  )
  
  # --- Form X score(s) ---
  cat("\n  Form X (", form_x_name, "):\n", sep = "")
  x_time <- NA_character_
  if (include_time) {
    x_time <- ask("    Time taken (e.g., '12 min 30 sec', or press Enter to skip)",
                  allow_empty = TRUE)
    if (!nchar(x_time)) x_time <- NA_character_
  }
  
  x_resp <- list(Participant = i)
  if (!is.na(x_time)) x_resp[["Time_taken"]] <- x_time
  
  if (detailed_entry && n_questions_x > 0) {
    for (q in seq_len(n_questions_x)) {
      qname <- paste0("X", q)
      qval  <- ask_int(paste0("    ", qname, " (0=incorrect, 1=correct)"), 0, 1)
      x_resp[[qname]] <- qval
    }
  } else {
    x_raw  <- ask(paste0("    Total score (0 to ", n_questions_x %||% scale_to, ")"),
                  default = NULL)
    x_resp[["X_score"]] <- suppressWarnings(as.numeric(x_raw))
    cat("    Note: pipeline expects item-level columns (X1, X2, ...) for full analysis.\n")
    cat("    If using score-only entry, some psychometric analyses will not run.\n")
  }
  x_rows[[i]] <- x_resp
  
  # --- Form Y score(s) ---
  cat("\n  Form Y (", form_y_name, "):\n", sep = "")
  y_time <- NA_character_
  if (include_time) {
    y_time <- ask("    Time taken (e.g., '12 min 30 sec', or press Enter to skip)",
                  allow_empty = TRUE)
    if (!nchar(y_time)) y_time <- NA_character_
  }
  
  y_resp <- list(Participant = i)
  if (!is.na(y_time)) y_resp[["Time_taken"]] <- y_time
  
  if (detailed_entry && n_questions_y > 0) {
    for (q in seq_len(n_questions_y)) {
      qname <- paste0("Y", q)
      qval  <- ask_int(paste0("    ", qname, " (0=incorrect, 1=correct)"), 0, 1)
      y_resp[[qname]] <- qval
    }
  } else {
    y_raw  <- ask(paste0("    Total score (0 to ", n_questions_y %||% 10, ")"),
                  default = NULL)
    y_resp[["Y_score"]] <- suppressWarnings(as.numeric(y_raw))
  }
  y_rows[[i]] <- y_resp
}

# --------------------------------------------------------------------------
# STEP 4: Output location
# --------------------------------------------------------------------------
cat_section("STEP 4: Output Location")
cat("  CSVs will be saved so the pipeline can analyse them immediately.\n\n")

default_out <- file.path(getwd(), "study_data", "entered_data")
out_dir_raw <- ask(paste0("  Output folder"), default = default_out)
out_dir     <- normalizePath(out_dir_raw, mustWork = FALSE, winslash = "/")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------------------------------
# STEP 5: Build and write CSVs
# --------------------------------------------------------------------------
cat_section("STEP 5: Writing CSVs")

# assignment.csv
df_assign <- do.call(rbind, lapply(assignment_rows, as.data.frame, stringsAsFactors = FALSE))
write.csv(df_assign, file.path(out_dir, "assignment.csv"), row.names = FALSE)
cat("  Written: assignment.csv\n")

# posttest_x_items.csv
df_x <- do.call(rbind, lapply(x_rows, function(r) {
  as.data.frame(r, stringsAsFactors = FALSE)
}))
write.csv(df_x, file.path(out_dir, "posttest_x_items.csv"), row.names = FALSE)
cat("  Written: posttest_x_items.csv\n")

# posttest_y_items.csv
df_y <- do.call(rbind, lapply(y_rows, function(r) {
  as.data.frame(r, stringsAsFactors = FALSE)
}))
write.csv(df_y, file.path(out_dir, "posttest_y_items.csv"), row.names = FALSE)
cat("  Written: posttest_y_items.csv\n")

# Write a stub config snipped with the study labels
config_stub <- paste0(
  "# Auto-generated stub — paste these values into config/study_config.yml\n\n",
  "study:\n",
  "  name: \"entered_data\"\n",
  "  intervention_label: \"", intervention_name, "\"\n",
  "  control_label: \"", control_name, "\"\n",
  "  form_x_label: \"", form_x_name, "\"\n",
  "  form_y_label: \"", form_y_name, "\"\n",
  "\nitem_exclusions:\n",
  "  x: [", paste0('"', x_excluded, '"', collapse = ", "), "]\n",
  "  y: [", paste0('"', y_excluded, '"', collapse = ", "), "]\n"
)
writeLines(config_stub, file.path(out_dir, "config_stub.txt"))
cat("  Written: config_stub.txt (copy values to config/study_config.yml)\n")

# --------------------------------------------------------------------------
# STEP 6: Offer to run pipeline
# --------------------------------------------------------------------------
cat_section("STEP 6: Run Analysis?")
cat("  Data saved to: ", out_dir, "\n\n")

run_now <- ask_yn("  Run the analysis pipeline now on this data? (y/n)", default = "y")

if (run_now) {
  # Find Rscript and run_all.R
  r_dir  <- file.path(dirname(normalizePath(sys.frames()[[1]]$ofile %||% getwd(),
                                            winslash = "/")), "..", "R")
  run_script <- normalizePath(file.path(r_dir, "run_all.R"), mustWork = FALSE)
  
  if (file.exists(run_script)) {
    Sys.setenv(
      STUDY_DATA_PATH  = out_dir,
      STUDY_NAME       = "entered_data",
      ANALYSIS_MODULES = "all"
    )
    source(run_script)
  } else {
    cat("\n  Could not find R/run_all.R.\n")
    cat("  To run manually:\n\n")
    cat("    set STUDY_DATA_PATH=", out_dir, "\n")
    cat("    set STUDY_NAME=entered_data\n")
    cat("    Rscript R/run_all.R\n\n")
  }
} else {
  cat("\n  To run later:\n\n")
  cat("    In RunPipeline.bat, choose Mode 2 (specify folder)\n")
  cat("    and enter: ", out_dir, "\n\n")
}

cat(strrep("=", 60), "\n")
cat("  DATA ENTRY COMPLETE\n")
cat(strrep("=", 60), "\n\n")
