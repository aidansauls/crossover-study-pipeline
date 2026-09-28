## =============================================================================
## R/11_publication_outputs.R
## Assemble an easy-to-navigate, configurable publication output set.
## Canonical outputs remain untouched in figures/, tables/, and tables_png/.
## =============================================================================

.script_dir <- local({
  d <- Sys.getenv("R_SCRIPTS_DIR", unset = "")
  if (nzchar(d)) return(d)
  for (i in rev(seq_along(sys.frames()))) {
    f <- sys.frames()[[i]]$ofile
    if (!is.null(f) && nzchar(f))
      return(dirname(normalizePath(f, winslash = "/")))
  }
  normalizePath("R", winslash = "/")
})
source(file.path(.script_dir, "00_setup.R"))
log_h1("11  PUBLICATION OUTPUTS")

cfg <- read_config()
.pub_cfg <- cfg$publication_outputs %||% list()
.pub_enabled <- isTRUE(.pub_cfg$enabled %||% TRUE)

if (!.pub_enabled) {
  log_line("Publication output assembly skipped: publication_outputs.enabled = false")
  if (exists("session_record_module", envir = .GlobalEnv)) {
    session_record_module("publication_outputs", "SKIPPED", 0)
  }
} else {
  .out_root <- out_path()
  .folder_name <- as.character(
    .pub_cfg$folder_name %||% "publication_outputs"
  )[1]
  if (!nzchar(.folder_name) || .folder_name %in% c(".", "..") ||
      grepl("[\\\\/]", .folder_name)) {
    stop("publication_outputs.folder_name must be one safe folder name.")
  }
  .pub_root <- file.path(.out_root, .folder_name)

  .out_norm <- normalizePath(.out_root, winslash = "/", mustWork = FALSE)
  .pub_norm <- normalizePath(.pub_root, winslash = "/", mustWork = FALSE)
  if (!startsWith(.pub_norm, paste0(.out_norm, "/")) ||
      identical(.pub_norm, .out_norm)) {
    stop("Refusing to reset publication output path outside the study output folder.")
  }
  if (dir.exists(.pub_root)) unlink(.pub_root, recursive = TRUE, force = TRUE)
  dir.create(.pub_root, recursive = TRUE, showWarnings = FALSE)

  .records <- list()
  .slash <- function(x) gsub("\\\\", "/", x)
  .record <- function(classification, id, source_rel, target_rel, copied) {
    .records[[length(.records) + 1L]] <<- data.frame(
      classification = classification,
      id = id,
      source = .slash(source_rel),
      output = .slash(target_rel),
      copied = isTRUE(copied),
      stringsAsFactors = FALSE
    )
  }
  .copy_one <- function(source_rel, target_rel, classification, id,
                        required = TRUE) {
    source <- file.path(.out_root, source_rel)
    target <- file.path(.pub_root, target_rel)
    ok <- file.exists(source)
    if (ok) {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      ok <- isTRUE(file.copy(source, target, overwrite = TRUE))
    }
    .record(classification, id, source_rel, target_rel, ok)
    if (!ok && required) {
      stop("Required publication output source is missing: ", source)
    } else if (!ok) {
      log_warn("Optional publication output source is missing: ", source)
    }
    invisible(ok)
  }

  .figure_catalog <- data.frame(
    id = c("paired_score_plot", "power_curve", "item_endorsement_by_sequence"),
    source = c(
      "manuscript_selected/main_body/figures/figure1_paired_score_plot.png",
      "manuscript_selected/main_body/figures/figure2_power_curve.png",
      "manuscript_selected/main_body/figures/figure3_item_endorsement_by_sequence.png"
    ),
    vector_source = c(
      NA_character_,
      "figures/supplementary/post_hoc_power_curve.pdf",
      NA_character_
    ),
    canonical_source = c(
      "figures/primary/ai_assisted_vs_noai_paired.png",
      "figures/supplementary/post_hoc_power_curve.png",
      "figures/exploratory/item_endorsement_by_sequence.png"
    ),
    filename = c(
      "figure_01_paired_score_plot.png",
      "figure_02_power_curve.png",
      "figure_03_item_endorsement_by_sequence.png"
    ),
    vector_filename = c(
      NA_character_,
      "figure_02_power_curve.pdf",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  .table_catalog <- data.frame(
    id = c(
      "score_descriptive_summary", "primary_paired_contrast",
      "supporting_analysis_summary"
    ),
    source_base = c(
      "manuscript_selected/main_body/tables/table1a_score_descriptive_summary",
      "manuscript_selected/main_body/tables/table1b_primary_paired_contrast",
      "manuscript_selected/main_body/tables/table2_supporting_analysis_summary"
    ),
    canonical_base = c(
      "tables/descriptive/02b_condition_descriptives_restricted",
      NA_character_, NA_character_
    ),
    filename_base = c(
      "table_01a_score_descriptive_summary",
      "table_01b_primary_paired_contrast",
      "table_02_supporting_analysis_summary"
    ),
    stringsAsFactors = FALSE
  )

  .as_selection <- function(x, default) {
    if (is.null(x)) return(default)
    as.character(unlist(x, use.names = FALSE))
  }
  .main_figure_ids <- .as_selection(
    .pub_cfg$main_figures, .figure_catalog$id
  )
  .main_table_ids <- .as_selection(
    .pub_cfg$main_tables, .table_catalog$id
  )

  .selected_figure_canonical <- character()
  for (i in seq_along(.main_figure_ids)) {
    item <- .main_figure_ids[i]
    idx <- match(item, .figure_catalog$id)
    if (!is.na(idx)) {
      source_rel <- .figure_catalog$source[idx]
      filename <- .figure_catalog$filename[idx]
      .selected_figure_canonical <- c(
        .selected_figure_canonical, .figure_catalog$canonical_source[idx]
      )
    } else {
      source_rel <- .slash(item)
      if (!grepl("[.]png$", source_rel, ignore.case = TRUE)) {
        stop("Custom main figure must be an output-relative PNG path: ", item)
      }
      clean <- gsub("[^A-Za-z0-9_.-]+", "_", basename(source_rel))
      filename <- sprintf("figure_%02d_%s", i, clean)
    }
    .copy_one(
      source_rel, file.path("main_figures", filename),
      "Main figure", item, required = TRUE
    )
    if (!is.na(idx) &&
        isTRUE(cfg$figures$power_curve$export_pdf %||% TRUE) &&
        !is.na(.figure_catalog$vector_source[idx])) {
      .copy_one(
        .figure_catalog$vector_source[idx],
        file.path("main_figures", .figure_catalog$vector_filename[idx]),
        "Main figure", item, required = TRUE
      )
    }
  }

  .selected_table_canonical <- character()
  for (i in seq_along(.main_table_ids)) {
    item <- .main_table_ids[i]
    idx <- match(item, .table_catalog$id)
    if (!is.na(idx)) {
      source_base <- .table_catalog$source_base[idx]
      filename_base <- .table_catalog$filename_base[idx]
      if (!is.na(.table_catalog$canonical_base[idx])) {
        .selected_table_canonical <- c(
          .selected_table_canonical, .table_catalog$canonical_base[idx]
        )
      }
    } else {
      source_base <- sub("[.](csv|png)$", "", .slash(item), ignore.case = TRUE)
      clean <- gsub("[^A-Za-z0-9_.-]+", "_", basename(source_base))
      filename_base <- sprintf("table_%02d_%s", i, clean)
    }
    source_csv <- paste0(source_base, ".csv")
    source_png <- paste0(source_base, ".png")
    if (startsWith(source_base, "tables/")) {
      source_png <- paste0(sub("^tables/", "tables_png/", source_base), ".png")
    } else if (startsWith(source_base, "tables_png/")) {
      source_csv <- paste0(sub("^tables_png/", "tables/", source_base), ".csv")
    }
    source_by_ext <- c(csv = source_csv, png = source_png)
    for (ext in names(source_by_ext)) {
      .copy_one(
        source_by_ext[[ext]],
        file.path("main_tables", paste0(filename_base, ".", ext)),
        "Main table", item, required = TRUE
      )
    }
  }

  .copy_tree <- function(source_dir_rel, target_dir_rel, pattern,
                         classification, exclude_rel = character(),
                         skip_first_dirs = character(),
                         exclude_hashes = character()) {
    source_dir <- file.path(.out_root, source_dir_rel)
    if (!dir.exists(source_dir)) return(invisible(0L))
    files <- list.files(
      source_dir, pattern = pattern, recursive = TRUE,
      full.names = TRUE, ignore.case = TRUE
    )
    copied <- 0L
    for (source in files) {
      rel_inside <- .slash(substring(source, nchar(source_dir) + 2L))
      first_dir <- strsplit(rel_inside, "/", fixed = TRUE)[[1]][1]
      source_rel <- .slash(file.path(source_dir_rel, rel_inside))
      if (first_dir %in% skip_first_dirs || source_rel %in% exclude_rel) next
      if (length(exclude_hashes) > 0) {
        source_hash <- unname(tools::md5sum(source))
        if (!is.na(source_hash) && source_hash %in% exclude_hashes) next
      }
      target_rel <- file.path(target_dir_rel, rel_inside)
      .copy_one(source_rel, target_rel, classification, rel_inside,
                required = FALSE)
      copied <- copied + 1L
    }
    invisible(copied)
  }

  if (isTRUE(.pub_cfg$include_supplementary_figures %||% TRUE)) {
    .main_figure_paths <- list.files(
      file.path(.pub_root, "main_figures"), pattern = "[.]png$",
      full.names = TRUE
    )
    .main_figure_hashes <- if (length(.main_figure_paths)) {
      unname(tools::md5sum(.main_figure_paths))
    } else {
      character()
    }
    .copy_tree(
      "figures", "supplementary_figures", "[.]png$",
      "Supplementary figure", exclude_rel = .selected_figure_canonical,
      skip_first_dirs = "main", exclude_hashes = .main_figure_hashes
    )
    .copy_tree(
      "manuscript_selected/supplement/figures",
      "supplementary_figures/manuscript_selected", "[.]png$",
      "Supplementary figure"
    )
  }

  if (isTRUE(.pub_cfg$include_supplementary_tables %||% TRUE)) {
    .exclude_csv <- paste0(.selected_table_canonical, ".csv")
    .exclude_png <- sub(
      "^tables/", "tables_png/",
      paste0(.selected_table_canonical, ".png")
    )
    .copy_tree(
      "tables", "supplementary_tables", "[.]csv$",
      "Supplementary table", exclude_rel = .exclude_csv
    )
    .copy_tree(
      "tables_png", "supplementary_tables", "[.]png$",
      "Supplementary table", exclude_rel = .exclude_png
    )
    .copy_tree(
      "manuscript_selected/supplement/tables",
      "supplementary_tables/manuscript_selected", "[.](csv|png)$",
      "Supplementary table"
    )
  }

  .manifest <- if (length(.records)) {
    do.call(rbind, .records)
  } else {
    data.frame(
      classification = character(), id = character(), source = character(),
      output = character(), copied = logical(), stringsAsFactors = FALSE
    )
  }
  utils::write.csv(
    .manifest, file.path(.pub_root, "OUTPUT_MANIFEST.csv"), row.names = FALSE
  )

  .count_class <- function(label) sum(.manifest$classification == label & .manifest$copied)
  .count_unique <- function(label) length(unique(
    .manifest$id[.manifest$classification == label & .manifest$copied]
  ))
  .readme <- c(
    "# Publication Outputs",
    "",
    "This folder is generated from `publication_outputs` in the study config.",
    "Canonical pipeline outputs remain unchanged in the study's `figures/`, `tables/`, and `tables_png/` folders.",
    "",
    "## Main outputs",
    "",
    paste0("- `main_figures/`: ", .count_unique("Main figure"), " selected figure(s); the power figure also has a vector PDF when enabled"),
    paste0("- `main_tables/`: ", .count_unique("Main table"), " selected table(s), each as CSV and PNG"),
    "",
    "## Supplementary outputs",
    "",
    paste0("- `supplementary_figures/`: ", .count_class("Supplementary figure"), " figure(s)"),
    paste0("- `supplementary_tables/`: ", .count_class("Supplementary table"), " table file(s)"),
    "",
    "Edit the config lists to change the main selections. Set either `include_supplementary_*` option to `false` to omit that supplementary tree.",
    "See `OUTPUT_MANIFEST.csv` for the source and classification of every copied file."
  )
  writeLines(.readme, file.path(.pub_root, "README.md"), useBytes = TRUE)

  if (any(!.manifest$copied)) {
    stop("One or more publication outputs could not be copied; see OUTPUT_MANIFEST.csv.")
  }
  log_line("Publication outputs: ", normalizePath(.pub_root, winslash = "/"))
  log_line("Main figures      : ", .count_unique("Main figure"))
  log_line("Main tables       : ", .count_unique("Main table"))
  log_line("Supplementary figs: ", .count_class("Supplementary figure"))
  log_line("Supplementary files: ", .count_class("Supplementary table"))
  if (exists("session_record_module", envir = .GlobalEnv)) {
    session_record_module("publication_outputs", "OK", 0)
  }
}
