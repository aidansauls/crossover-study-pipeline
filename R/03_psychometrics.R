## =============================================================================
## R/03_psychometrics.R
## Psychometric item analysis for both test forms.
## Produces: item difficulty, discrimination, item-rest correlations,
##           KR-20/Cronbach alpha, McDonald's omega, split-half reliability,
##           ceiling/floor effects, item information, and DIF by sequence.
## Copyright (c) 2026 Aidan Sauls — see LICENSE for terms.
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
log_h1("03  PSYCHOMETRICS")

cfg      <- read_config()
raw_data <- load_rds("raw_data")

x_items           <- raw_data$x_items
y_items           <- raw_data$y_items
x_cols_full       <- raw_data$x_cols_full
x_cols_restricted <- raw_data$x_cols_restricted
y_cols_full       <- raw_data$y_cols_full
y_cols_restricted <- raw_data$y_cols_restricted
x_excluded        <- raw_data$x_excluded
y_excluded        <- raw_data$y_excluded

dat             <- load_rds("analysis_data")
scale_to        <- as.numeric(cfg$scores$scale_to %||% 10)
int_label       <- cfg$study$intervention_label %||% "Intervention"
ctl_label       <- cfg$study$control_label      %||% "Control"
form_x_label    <- cfg$study$form_x_label       %||% "Form X"
form_y_label    <- cfg$study$form_y_label       %||% "Form Y"

# =============================================================================
# HELPER: Full item-level analysis for one form
# =============================================================================

item_analysis <- function(items_df, item_cols, form_label, excluded_cols = character(0)) {
  mat <- as.matrix(dplyr::select(items_df, dplyr::all_of(item_cols)))
  storage.mode(mat) <- "numeric"
  n     <- nrow(mat)
  k     <- length(item_cols)
  
  # --- Item difficulty: proportion correct ---
  p_correct <- colMeans(mat, na.rm = TRUE)
  
  # --- Total score (for point-biserial) ---
  total <- rowSums(mat, na.rm = TRUE)
  
  # --- Point-biserial correlation (item vs total including that item) ---
  pb_whole <- sapply(item_cols, function(col) {
    suppressWarnings(cor(mat[, col], total, use = "pairwise.complete.obs"))
  })
  
  # --- Item-rest correlation (item vs total of remaining items) ---
  ir_corr <- sapply(seq_along(item_cols), function(i) {
    rest <- rowSums(mat[, -i, drop = FALSE], na.rm = TRUE)
    suppressWarnings(cor(mat[, i], rest, use = "pairwise.complete.obs"))
  })
  
  # --- Alpha-if-item-deleted (KR-20 without item) ---
  alpha_full <- kr20(mat)
  alpha_del  <- sapply(seq_along(item_cols), function(i) {
    m2 <- mat[, -i, drop = FALSE]
    if (ncol(m2) < 2) return(NA_real_)
    suppressWarnings(kr20(m2))
  })
  
  # --- Difficulty category ---
  difficulty_cat <- dplyr::case_when(
    p_correct >= 0.80 ~ "Easy (≥80%)",
    p_correct >= 0.50 ~ "Moderate (50–79%)",
    TRUE              ~ "Hard (<50%)"
  )
  
  # --- Flag items ---
  flag <- dplyr::case_when(
    item_cols %in% excluded_cols ~ "excluded",
    p_correct > 0.95 | p_correct < 0.05 ~ "extreme difficulty",
    ir_corr < 0.10 ~ "low discrimination",
    TRUE           ~ ""
  )
  
  result_tbl <- tibble::tibble(
    form            = form_label,
    item            = item_cols,
    n               = n,
    p_correct       = p_correct,
    difficulty_cat  = difficulty_cat,
    pb_corr         = pb_whole,
    item_rest_corr  = ir_corr,
    alpha_if_deleted = alpha_del,
    excluded        = item_cols %in% excluded_cols,
    flag            = flag
  )

  # Log per-item details so the log is a standalone audit trail
  log_line(sprintf("  %-6s  %-22s  p_cor  pb_r   ir_r   a_del  flag",
                   "Item", "Difficulty"))
  for (i in seq_len(nrow(result_tbl))) {
    r <- result_tbl[i, ]
    log_line(sprintf("  %-6s  %-22s  %.3f  %+.3f  %+.3f  %.3f  %s",
      toupper(r$item),
      r$difficulty_cat,
      r$p_correct,
      r$pb_corr,
      r$item_rest_corr,
      r$alpha_if_deleted,
      if (nzchar(r$flag)) paste0("[", r$flag, "]") else "-"))
  }

  result_tbl
}

# =============================================================================
# RELIABILITY: KR-20, alpha, omega
# =============================================================================

reliability_summary <- function(mat, label) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"
  k <- ncol(mat)
  n <- nrow(mat)
  
  out <- list(
    label = label,
    n     = n,
    k     = k
  )
  
  # KR-20 (== Cronbach alpha for binary items)
  out$kr20 <- suppressWarnings(kr20(mat))
  
  # Cronbach alpha via psych if available
  if (requireNamespace("psych", quietly = TRUE)) {
    a <- suppressWarnings(psych::alpha(as.data.frame(mat), check.keys = FALSE))
    out$alpha_std   <- a$total$std.alpha
    out$alpha_raw   <- a$total$raw_alpha
    out$mean_inter_corr <- a$total$average_r
    
    # McDonald's omega (total)
    tryCatch({
      om <- suppressWarnings(
        psych::omega(as.data.frame(mat), nfactors = 1,
                     plot = FALSE, warnings = FALSE)
      )
      out$omega_t <- om$omega.tot
      out$omega_h <- om$omega_h
    }, error = function(e) {
      out$omega_t <<- NA_real_
      out$omega_h <<- NA_real_
    })
  } else {
    out$alpha_std       <- out$kr20
    out$alpha_raw       <- out$kr20
    out$mean_inter_corr <- NA_real_
    out$omega_t         <- NA_real_
    out$omega_h         <- NA_real_
  }
  
  # Split-half reliability (odd vs even items)
  if (k >= 2) {
    odd  <- mat[, seq(1, k, by = 2), drop = FALSE]
    even <- mat[, seq(2, k, by = 2), drop = FALSE]
    s_odd  <- rowSums(odd,  na.rm = TRUE)
    s_even <- rowSums(even, na.rm = TRUE)
    r_half <- suppressWarnings(cor(s_odd, s_even, use = "pairwise.complete.obs"))
    # Spearman-Brown correction
    out$split_half_r        <- r_half
    out$split_half_corrected <- (2 * r_half) / (1 + r_half)
  } else {
    out$split_half_r <- out$split_half_corrected <- NA_real_
  }
  
  out
}

# =============================================================================
# RUN ITEM ANALYSES
# =============================================================================

log_h2("Item analysis — Form X")
ia_x_full <- item_analysis(x_items, x_cols_full, form_x_label, x_excluded)

log_h2("Item analysis — Form Y")
ia_y_full <- item_analysis(y_items, y_cols_full, form_y_label, y_excluded)

ia_combined <- dplyr::bind_rows(ia_x_full, ia_y_full)

# Restricted versions (only if exclusions exist)
ia_x_restricted <- if (length(x_excluded) > 0) {
  item_analysis(x_items, x_cols_restricted, paste0(form_x_label, " (restricted)"))
} else ia_x_full
ia_y_restricted <- if (length(y_excluded) > 0) {
  item_analysis(y_items, y_cols_restricted, paste0(form_y_label, " (restricted)"))
} else ia_y_full

log_h2("Reliability — Form X (full)")
rel_x_full <- reliability_summary(
  dplyr::select(x_items, dplyr::all_of(x_cols_full)), form_x_label)
log_calc("KR-20 (Form X)",
  formula = "(k / (k-1)) * (1 - sum(p*q) / var_total)",
  inputs  = list(k = rel_x_full$k, n = rel_x_full$n),
  result  = paste0("KR-20=", round(rel_x_full$kr20, 4),
                   "  alpha=", round(rel_x_full$alpha_std, 4),
                   "  omega_t=", round(rel_x_full$omega_t %||% NA_real_, 4),
                   "  split-half(SB)=", round(rel_x_full$split_half_corrected %||% NA_real_, 4),
                   "  mean_r=", round(rel_x_full$mean_inter_corr %||% NA_real_, 4)))
if (exists("session_record_result"))
  session_record_result(
    paste0("reliability_", form_x_label, "_full"),
    paste0("KR-20=", round(rel_x_full$kr20, 3),
           "  alpha=", round(rel_x_full$alpha_std, 3),
           "  omega_t=", round(rel_x_full$omega_t %||% NA_real_, 3),
           "  split-half(SB)=", round(rel_x_full$split_half_corrected %||% NA_real_, 3),
           "  mean_r=", round(rel_x_full$mean_inter_corr %||% NA_real_, 3),
           "  k=", rel_x_full$k, "  n=", rel_x_full$n))

log_h2("Reliability — Form Y (full)")
rel_y_full <- reliability_summary(
  dplyr::select(y_items, dplyr::all_of(y_cols_full)), form_y_label)
log_calc("KR-20 (Form Y)",
  formula = "(k / (k-1)) * (1 - sum(p*q) / var_total)",
  inputs  = list(k = rel_y_full$k, n = rel_y_full$n),
  result  = paste0("KR-20=", round(rel_y_full$kr20, 4),
                   "  alpha=", round(rel_y_full$alpha_std, 4),
                   "  omega_t=", round(rel_y_full$omega_t %||% NA_real_, 4),
                   "  split-half(SB)=", round(rel_y_full$split_half_corrected %||% NA_real_, 4),
                   "  mean_r=", round(rel_y_full$mean_inter_corr %||% NA_real_, 4)))
if (exists("session_record_result"))
  session_record_result(
    paste0("reliability_", form_y_label, "_full"),
    paste0("KR-20=", round(rel_y_full$kr20, 3),
           "  alpha=", round(rel_y_full$alpha_std, 3),
           "  omega_t=", round(rel_y_full$omega_t %||% NA_real_, 3),
           "  split-half(SB)=", round(rel_y_full$split_half_corrected %||% NA_real_, 3),
           "  mean_r=", round(rel_y_full$mean_inter_corr %||% NA_real_, 3),
           "  k=", rel_y_full$k, "  n=", rel_y_full$n))

rel_x_restricted <- if (length(x_excluded) > 0) {
  reliability_summary(
    dplyr::select(x_items, dplyr::all_of(x_cols_restricted)),
    paste0(form_x_label, " (restricted)"))
} else rel_x_full

if (length(x_excluded) > 0 && exists("session_record_result"))
  session_record_result(
    paste0("reliability_", form_x_label, "_restricted"),
    paste0("excluded=", paste(x_excluded, collapse = ","),
           "  KR-20=", round(rel_x_restricted$kr20, 3),
           "  alpha=", round(rel_x_restricted$alpha_std, 3),
           "  omega_t=", round(rel_x_restricted$omega_t %||% NA_real_, 3),
           "  split-half(SB)=", round(rel_x_restricted$split_half_corrected %||% NA_real_, 3),
           "  mean_r=", round(rel_x_restricted$mean_inter_corr %||% NA_real_, 3),
           "  k=", rel_x_restricted$k, "  n=", rel_x_restricted$n))

rel_y_restricted <- if (length(y_excluded) > 0) {
  reliability_summary(
    dplyr::select(y_items, dplyr::all_of(y_cols_restricted)),
    paste0(form_y_label, " (restricted)"))
} else rel_y_full

if (length(y_excluded) > 0 && exists("session_record_result"))
  session_record_result(
    paste0("reliability_", form_y_label, "_restricted"),
    paste0("excluded=", paste(y_excluded, collapse = ","),
           "  KR-20=", round(rel_y_restricted$kr20, 3),
           "  alpha=", round(rel_y_restricted$alpha_std, 3),
           "  omega_t=", round(rel_y_restricted$omega_t %||% NA_real_, 3),
           "  split-half(SB)=", round(rel_y_restricted$split_half_corrected %||% NA_real_, 3),
           "  mean_r=", round(rel_y_restricted$mean_inter_corr %||% NA_real_, 3),
           "  k=", rel_y_restricted$k, "  n=", rel_y_restricted$n))

# =============================================================================
# DIF BY SEQUENCE GROUP
# =============================================================================
# For each item, compare proportion correct between sequence groups.
# A chi-squared test (or Fisher's exact for small n) tests DIF.

run_dif <- function(items_df, item_cols, form_label, assignment_df) {
  items_with_seq <- dplyr::left_join(
    items_df,
    dplyr::select(assignment_df, participant, sequence_group),
    by = "participant"
  )
  
  purrr::map_dfr(item_cols, function(col) {
    tbl <- table(items_with_seq$sequence_group, items_with_seq[[col]])
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
      return(tibble::tibble(form = form_label, item = col,
                            dif_p = NA_real_, dif_test = "insufficient data"))
    }
    use_fisher <- any(tbl < 5)
    res <- suppressWarnings(
      if (use_fisher) fisher.test(tbl) else chisq.test(tbl, correct = FALSE)
    )
    tibble::tibble(
      form      = form_label,
      item      = col,
      dif_p     = res$p.value,
      dif_test  = if (use_fisher) "Fisher" else "Chi-sq"
    )
  })
}

log_h2("DIF analysis")
dif_x <- run_dif(x_items, x_cols_full, form_x_label, dat)
dif_y <- run_dif(y_items, y_cols_full, form_y_label, dat)
dif   <- dplyr::bind_rows(dif_x, dif_y) |>
  dplyr::mutate(
    dif_flag = !is.na(.data$dif_p) & .data$dif_p < 0.05
  )

log_check("DIF flags (p<.05): ",
          sum(dif$dif_flag, na.rm = TRUE), " / ", nrow(dif), " items")
if (any(dif$dif_flag, na.rm = TRUE)) {
  flagged_dif <- dif[!is.na(dif$dif_flag) & dif$dif_flag, ]
  log_warn("Items with possible DIF (p < .05 by sequence group):")
  for (i in seq_len(nrow(flagged_dif))) {
    log_warn(sprintf("  %-8s  test=%-8s  p=%.4f",
      toupper(flagged_dif$item[i]),
      flagged_dif$dif_test[i],
      flagged_dif$dif_p[i]))
  }
} else {
  log_check("No DIF detected in any item at p < .05")
}

# =============================================================================
# DIF BY PERIOD (do items behave differently in Period 1 vs Period 2?)
# Uses the period in which each participant sat each form.
# =============================================================================
log_h2("DIF by period")

run_dif_period <- function(items_df, item_cols, form_label, assignment_df,
                            form_period_col) {
  items_with_period <- dplyr::left_join(
    items_df,
    dplyr::select(assignment_df, participant,
                  item_period = dplyr::all_of(form_period_col)),
    by = "participant"
  ) |>
    dplyr::mutate(period_grp = dplyr::if_else(.data$item_period == 1L,
                                               "Period 1", "Period 2"))

  purrr::map_dfr(item_cols, function(col) {
    tbl <- table(items_with_period$period_grp, items_with_period[[col]])
    if (nrow(tbl) < 2 || ncol(tbl) < 2) {
      return(tibble::tibble(form = form_label, item = col,
                            dif_period_p    = NA_real_,
                            dif_period_test = "insufficient data",
                            p_p1 = NA_real_, p_p2 = NA_real_))
    }
    use_fisher <- any(tbl < 5)
    res <- suppressWarnings(
      if (use_fisher) fisher.test(tbl) else chisq.test(tbl, correct = FALSE)
    )
    # proportions per period
    p_by_period <- prop.table(tbl, margin = 1)[, "1", drop = FALSE]
    tibble::tibble(
      form            = form_label,
      item            = col,
      dif_period_p    = res$p.value,
      dif_period_test = if (use_fisher) "Fisher" else "Chi-sq",
      p_p1            = if ("Period 1" %in% rownames(p_by_period))
                          p_by_period["Period 1", 1] else NA_real_,
      p_p2            = if ("Period 2" %in% rownames(p_by_period))
                          p_by_period["Period 2", 1] else NA_real_
    )
  })
}

dif_period_x <- run_dif_period(x_items, x_cols_full, form_x_label,
                                dat, "form_x_period")
dif_period_y <- run_dif_period(y_items, y_cols_full, form_y_label,
                                dat, "form_y_period")
dif_period   <- dplyr::bind_rows(dif_period_x, dif_period_y) |>
  dplyr::mutate(dif_period_flag = !is.na(.data$dif_period_p) &
                                    .data$dif_period_p < 0.05)

log_check("DIF-by-period flags (p<.05): ",
          sum(dif_period$dif_period_flag, na.rm = TRUE),
          " / ", nrow(dif_period), " items")

# =============================================================================
# ABILITY-STRATIFIED ITEM ANALYSIS
# For each item, compute p(correct) within N_STRATA ability strata
# (based on total score on the same form).
# This reveals items that discriminate poorly across the ability range
# or that high/low scorers answer "wrongly" (suspicion flags).
# =============================================================================
log_h2("Ability-stratified item analysis")

n_strata <- as.integer(cfg_get("psychometrics", "ability_strata", default = 4L))
strata_labels <- paste0("Q", seq_len(n_strata),
                        c("(low)", rep("", n_strata - 2), "(high)")[
                          pmax(1, pmin(c(1, rep(2, n_strata-2), 3), 3))])
strata_labels <- paste0("Q", seq_len(n_strata))   # simple Qn labels

score_stratified_item_analysis <- function(items_df, item_cols,
                                            total_scores, form_label,
                                            n_q = 4L) {
  breaks  <- unique(quantile(total_scores, probs = seq(0, 1, 1/n_q), na.rm = TRUE))
  if (length(breaks) < 2) {
    log_warn("Cannot create ability strata for ", form_label,
             " — not enough unique scores.")
    return(NULL)
  }
  q_labels <- paste0("Q", seq_len(length(breaks) - 1))
  ability_q <- cut(total_scores, breaks = breaks, labels = q_labels,
                   include.lowest = TRUE)

  mat <- as.matrix(dplyr::select(items_df, dplyr::all_of(item_cols)))
  storage.mode(mat) <- "numeric"

  purrr::map_dfr(seq_along(item_cols), function(i) {
    col   <- item_cols[i]
    iv    <- mat[, i]
    purrr::map_dfr(q_labels, function(q) {
      idx  <- !is.na(ability_q) & ability_q == q
      vals <- iv[idx]
      vals <- vals[!is.na(vals)]
      tibble::tibble(
        form            = form_label,
        item            = col,
        ability_quartile = q,
        n               = length(vals),
        p_correct       = if (length(vals) > 0) mean(vals) else NA_real_
      )
    })
  })
}

# Compute from total-score on the full item set
x_total_scores <- rowSums(
  dplyr::select(x_items, dplyr::all_of(x_cols_full)), na.rm = TRUE)
y_total_scores <- rowSums(
  dplyr::select(y_items, dplyr::all_of(y_cols_full)), na.rm = TRUE)

strat_x <- score_stratified_item_analysis(
  x_items, x_cols_full, x_total_scores, form_x_label, n_strata)
strat_y <- score_stratified_item_analysis(
  y_items, y_cols_full, y_total_scores, form_y_label, n_strata)
strat_all <- dplyr::bind_rows(strat_x, strat_y)

log_df(strat_all, "Ability-stratified item analysis")

# =============================================================================
# SUSPICIOUS ITEM DETECTION
# An item is flagged as suspicious when high-ability students underperform
# on it OR low-ability students overperform relative to what is expected.
#
# Criteria (configurable via config psychometrics.*):
#   1. Top-quartile miss rate  >  top_miss_threshold  (default: 0.25)
#      → High performers are getting this wrong — unusual
#   2. Bottom-quartile hit rate > bottom_hit_threshold (default: 0.60)
#      → Low performers are getting it right — unusual
#   3. Reversed discrimination: p_correct(Q1) > p_correct(Q_top)
#      → Easier for weak students — classic red flag
#   4. Any-quartile inversion: a lower quartile outperforms a higher one
# =============================================================================
log_h2("Suspicious item detection")

top_miss_thr   <- as.numeric(cfg_get("psychometrics", "top_miss_threshold",
                                      default = 0.25))
bot_hit_thr    <- as.numeric(cfg_get("psychometrics", "bottom_hit_threshold",
                                      default = 0.60))
min_item_rest_r <- as.numeric(cfg_get("psychometrics", "min_item_rest_r",
                                       default = 0.10))

detect_suspicious_items <- function(strat_df, ia_df, form_label) {
  if (is.null(strat_df)) return(tibble::tibble())

  strat_f <- dplyr::filter(strat_df, .data$form == form_label)
  if (nrow(strat_f) == 0) return(tibble::tibble())

  q_levs  <- sort(unique(strat_f$ability_quartile))
  q_top   <- q_levs[length(q_levs)]
  q_bot   <- q_levs[1]

  items_in_form <- unique(strat_f$item)

  purrr::map_dfr(items_in_form, function(it) {
    rows <- dplyr::filter(strat_f, .data$item == it)
    p_by_q <- stats::setNames(rows$p_correct, rows$ability_quartile)

    p_top <- p_by_q[q_top]
    p_bot <- p_by_q[q_bot]
    top_miss <- if (!is.na(p_top)) 1 - p_top else NA_real_
    bot_hit  <- p_bot

    # item-rest correlation from ia_df
    ia_row <- dplyr::filter(ia_df, .data$item == it)
    ir     <- if (nrow(ia_row) > 0) ia_row$item_rest_corr[1] else NA_real_

    flags <- character(0)

    if (!is.na(top_miss) && top_miss > top_miss_thr)
      flags <- c(flags, sprintf("top-Q miss %.0f%%", top_miss * 100))

    if (!is.na(bot_hit) && bot_hit > bot_hit_thr)
      flags <- c(flags, sprintf("bot-Q hit %.0f%%", bot_hit * 100))

    if (!is.na(p_top) && !is.na(p_bot) && p_bot > p_top)
      flags <- c(flags, "reversed discrimination")

    # check for any inversion across adjacent quartiles
    q_ord  <- q_levs
    p_ord  <- p_by_q[q_ord]
    if (sum(!is.na(p_ord)) >= 2) {
      p_ord2 <- p_ord[!is.na(p_ord)]
      if (any(diff(p_ord2) < -0.10))   # a drop >10pp going up the ability scale
        flags <- c(flags, "non-monotone across Q")
    }

    # low or negative item-rest correlation is suspect (configurable threshold)
    if (!is.na(ir) && ir < min_item_rest_r)
      flags <- c(flags, sprintf("item-rest r below threshold (%.3f < %.2f)",
                                ir, min_item_rest_r))

    tibble::tibble(
      form          = form_label,
      item          = it,
      p_bottom_Q    = round(p_bot  %||% NA_real_, 3),
      p_top_Q       = round(p_top  %||% NA_real_, 3),
      top_miss_rate = round(top_miss %||% NA_real_, 3),
      item_rest_r   = round(ir %||% NA_real_, 3),
      n_flags       = length(flags),
      suspicious    = length(flags) > 0,
      flags         = paste(flags, collapse = "; ")
    )
  })
}

suspicious_x <- detect_suspicious_items(strat_x, ia_x_full, form_x_label)
suspicious_y <- detect_suspicious_items(strat_y, ia_y_full, form_y_label)
suspicious_all <- dplyr::bind_rows(suspicious_x, suspicious_y)

n_sus <- sum(suspicious_all$suspicious, na.rm = TRUE)
log_check("Suspicious items flagged: ", n_sus, " / ", nrow(suspicious_all))

if (n_sus > 0) {
  sus_items <- dplyr::filter(suspicious_all, .data$suspicious)
  for (i in seq_len(nrow(sus_items))) {
    r <- sus_items[i, ]
    log_flag(r$item, r$form,
             paste0("Sus (", r$n_flags, " flags)"),
             r$flags)
  }
}

# =============================================================================
# MISSING RESPONSE PATTERN ANALYSIS
# Log per-item missing counts + per-participant missing counts
# =============================================================================
log_h2("Missing response patterns")

missing_analysis <- function(items_df, item_cols, form_label) {
  mat  <- as.matrix(dplyr::select(items_df, dplyr::all_of(item_cols)))
  n    <- nrow(mat)

  item_miss <- tibble::tibble(
    form      = form_label,
    item      = item_cols,
    n_total   = n,
    n_missing = colSums(is.na(mat)),
    pct_missing = colSums(is.na(mat)) / n * 100
  )

  pp_miss <- tibble::tibble(
    form         = form_label,
    participant  = items_df$participant,
    n_items      = length(item_cols),
    n_missing    = rowSums(is.na(mat)),
    pct_missing  = rowSums(is.na(mat)) / length(item_cols) * 100
  )

  high_miss_items <- dplyr::filter(item_miss, .data$pct_missing > 5)
  if (nrow(high_miss_items) > 0) {
    log_warn(form_label, ": items with >5% missing responses: ",
             paste(toupper(high_miss_items$item), collapse = ", "))
  }

  high_miss_pp <- dplyr::filter(pp_miss, .data$pct_missing > 20)
  if (nrow(high_miss_pp) > 0) {
    log_warn(form_label, ": participants missing >20% of items: ",
             paste(high_miss_pp$participant, collapse = ", "))
  }

  list(item_missing = item_miss, pp_missing = pp_miss)
}

miss_x <- missing_analysis(x_items, x_cols_full, form_x_label)
miss_y <- missing_analysis(y_items, y_cols_full, form_y_label)

item_missing_all <- dplyr::bind_rows(miss_x$item_missing, miss_y$item_missing)
pp_missing_all   <- dplyr::bind_rows(miss_x$pp_missing,   miss_y$pp_missing)

# =============================================================================
# SAVE ALL PSYCHOMETRICS
# =============================================================================

psychometrics <- list(
  item_analysis   = ia_combined,
  item_analysis_x = ia_x_full,
  item_analysis_y = ia_y_full,
  item_analysis_x_restricted = ia_x_restricted,
  item_analysis_y_restricted = ia_y_restricted,
  reliability_x   = rel_x_full,
  reliability_y   = rel_y_full,
  reliability_x_restricted = rel_x_restricted,
  reliability_y_restricted = rel_y_restricted,
  dif             = dif,
  dif_period      = dif_period,
  strat_all       = strat_all,
  suspicious_all  = suspicious_all,
  item_missing    = item_missing_all,
  pp_missing      = pp_missing_all
)

save_rds(psychometrics, "psychometrics")

# =============================================================================
# TABLES
# =============================================================================

log_h2("Tables")

# --- Table: Item difficulty & discrimination ---
tbl_items <- ia_combined |>
  dplyr::transmute(
    Form          = .data$form,
    Item          = toupper(.data$item),
    N             = .data$n,
    `P Correct`   = round(.data$p_correct, 3),
    `Difficulty`  = .data$difficulty_cat,
    `Point-Biserial r` = round(.data$pb_corr, 3),
    `Item-Rest r`      = round(.data$item_rest_corr, 3),
    `Alpha-if-Deleted` = round(.data$alpha_if_deleted, 3),
    Excluded      = .data$excluded,
    Flag          = .data$flag
  )

save_table(tbl_items, "item_analysis",
           subfolder = "psychometrics",
           caption   = "Item-level psychometric statistics for both test forms")

# --- Table: Reliability summary ---
fmt_rel <- function(r) {
  tibble::tibble(
    Form                      = r$label,
    N                         = r$n,
    Items                     = r$k,
    `KR-20`                   = round(r$kr20, 3),
    `Cronbach alpha`          = round(r$alpha_std, 3),
    `McDonald omega (total)`  = round(r$omega_t %||% NA_real_, 3),
    `McDonald omega_h`        = round(r$omega_h %||% NA_real_, 3),
    `Mean inter-item r`       = round(r$mean_inter_corr %||% NA_real_, 3),
    `Split-half r`            = round(r$split_half_r %||% NA_real_, 3),
    `Split-half (S-B)`        = round(r$split_half_corrected %||% NA_real_, 3)
  )
}

tbl_reliability <- dplyr::bind_rows(
  fmt_rel(rel_x_full), fmt_rel(rel_y_full),
  if (length(x_excluded) > 0) fmt_rel(rel_x_restricted) else NULL,
  if (length(y_excluded) > 0) fmt_rel(rel_y_restricted) else NULL
)

save_table(tbl_reliability, "reliability_summary",
           subfolder = "psychometrics",
           caption   = "Internal consistency and reliability for both test forms")

# --- Table: DIF ---
tbl_dif <- dif |>
  dplyr::transmute(
    Form   = .data$form,
    Item   = toupper(.data$item),
    Test   = .data$dif_test,
    `p`    = round(.data$dif_p, 3),
    `DIF flag (p < .05)` = .data$dif_flag
  )

save_table(tbl_dif, "dif_by_sequence",
           subfolder = "psychometrics",
           caption   = "Differential item functioning (DIF) by sequence group")

# =============================================================================
# FIGURES
# =============================================================================

log_h2("Figures")

# ---- Figure: Item difficulty by form ----
fig_difficulty <- ia_combined |>
  dplyr::mutate(
    item_label = toupper(.data$item),
    item_num   = as.numeric(gsub("^\\D+", "", .data$item)),
    excluded   = factor(.data$excluded, levels = c(FALSE, TRUE), labels = c("Included", "Excluded"))
  ) |>
  dplyr::arrange(.data$form, .data$item_num) |>
  dplyr::mutate(
    item_label = factor(.data$item_label, levels = unique(.data$item_label))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = .data$item_label,
                               y = .data$p_correct,
                               fill = .data$form,
                               alpha = .data$excluded)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::geom_hline(yintercept = c(0.5, 0.8), linetype = "dashed",
                      colour = "grey50", linewidth = 0.4) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(),
                               limits = c(0, 1), name = "Proportion Correct") +
  ggplot2::scale_alpha_manual(values = c(Included = 1, Excluded = 0.35),
                              guide = "none") +
  ggplot2::scale_fill_manual(
    values = c(stats::setNames(
      c(cfg_get("figures","color_intervention", default="#2E8B57"),
        cfg_get("figures","color_control",      default="#CD853F")),
      c(form_x_label, form_y_label)
    )),
    name = "Form"
  ) +
  ggplot2::xlab("Item") +
  theme_clean() +
  ggplot2::theme(
    axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  ) +
  ggplot2::facet_wrap(~ form, scales = "free_x", ncol = 2)

save_figure(fig_difficulty, "item_difficulty_by_form",
            subfolder = "psychometrics", height = 4.5)

# ---- Figure: Item-rest correlations ----
fig_ir <- ia_combined |>
  dplyr::mutate(
    item_num = as.numeric(gsub("^\\D+", "", .data$item))
  ) |>
  dplyr::arrange(.data$form, .data$item_num) |>
  dplyr::mutate(
    item_label = factor(toupper(.data$item), levels = unique(toupper(.data$item)))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = .data$item_label,
                               y = .data$item_rest_corr,
                               colour = .data$form,
                               shape  = .data$excluded)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed",
                      colour = "grey50", linewidth = 0.4) +
  ggplot2::scale_y_continuous(name = "Item-Rest Correlation",
                               limits = c(-0.1, 1)) +
  ggplot2::scale_colour_manual(
    values = stats::setNames(
      c(cfg_get("figures","color_intervention", default="#2E8B57"),
        cfg_get("figures","color_control",      default="#CD853F")),
      c(form_x_label, form_y_label)
    ), name = "Form"
  ) +
  ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                              labels = c("Included", "Excluded"),
                              name = "Status") +
  ggplot2::xlab("Item") +
  theme_clean() +
  ggplot2::theme(
    axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  ) +
  ggplot2::facet_wrap(~ form, scales = "free_x", ncol = 2)

save_figure(fig_ir, "item_rest_correlations",
            subfolder = "psychometrics", height = 4.5)

# Alpha-if-deleted: superseded by Fig 26 in 05_figures.R (item_analysis/alpha_if_deleted)
# which marks excluded items, shows both scoring schemes, and has better lollipop layout.
log_line("Alpha-if-deleted figure skipped here: see item_analysis/alpha_if_deleted (05_figures.R)")

# ---- Figure: Score distributions by form ----
score_dist_data <- dplyr::bind_rows(
  dplyr::transmute(dat,
    form  = form_x_label,
    score = .data$x_score_full,
    seq   = .data$sequence_group),
  dplyr::transmute(dat,
    form  = form_y_label,
    score = .data$y_score_full,
    seq   = .data$sequence_group)
)

fig_score_dist <- score_dist_data |>
  ggplot2::ggplot(ggplot2::aes(x = .data$score)) +
  ggplot2::geom_histogram(bins = ceiling(scale_to) + 1,
                          fill = "steelblue", colour = "white",
                          linewidth = 0.3, boundary = 0) +
  ggplot2::scale_x_continuous(
    name   = cfg_get("figures","y_axis_score_label", default = "Score (0-10)"),
    limits = c(0, scale_to)
  ) +
  ggplot2::scale_y_continuous(
    name   = "Count (participants)",
    breaks = function(x) {
      nice <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 500)
      step <- nice[which(ceiling(max(x) / nice) <= 9)[1]]
      if (is.na(step)) step <- nice[length(nice)]
      seq(0, ceiling(max(x) / step) * step, by = step)
    }
  ) +
  ggplot2::facet_wrap(~ form, ncol = 2) +
  theme_clean()

save_figure(fig_score_dist, "score_distributions_by_form",
            subfolder = "psychometrics")

# ---- Table: DIF by period ----
tbl_dif_period <- dif_period |>
  dplyr::transmute(
    Form            = .data$form,
    Item            = toupper(.data$item),
    Test            = .data$dif_period_test,
    `p`             = round(.data$dif_period_p, 3),
    `P-Correct P1`  = round(.data$p_p1, 3),
    `P-Correct P2`  = round(.data$p_p2, 3),
    `Period DIF flag (p<.05)` = .data$dif_period_flag
  )

save_table(tbl_dif_period, "dif_by_period",
           subfolder = "psychometrics",
           caption   = "DIF by period: do items behave differently in Period 1 vs Period 2?")

# ---- Table: Ability-stratified item difficulty ----
if (!is.null(strat_all) && nrow(strat_all) > 0) {
  tbl_strat <- strat_all |>
    tidyr::pivot_wider(
      id_cols     = c(form, item),
      names_from  = ability_quartile,
      values_from = p_correct,
      names_prefix = "p_"
    ) |>
    dplyr::left_join(
      dplyr::select(ia_combined, form, item, p_correct, item_rest_corr, flag),
      by = c("form", "item")
    ) |>
    dplyr::transmute(
      Form         = .data$form,
      Item         = toupper(.data$item),
      `Overall P`  = round(.data$p_correct, 3),
      dplyr::across(dplyr::starts_with("p_Q"), ~ round(.x, 3)),
      `Item-Rest r` = round(.data$item_rest_corr, 3),
      Flag          = .data$flag
    )

  save_table(tbl_strat, "ability_stratified_item_difficulty",
             subfolder = "psychometrics",
             caption   = paste0(
               "Item difficulty (proportion correct) by ability quartile ",
               "(Q1=lowest, Q", n_strata, "=highest). ",
               "Red flags: inverted or flat gradients indicate poor discrimination."))
}

# ---- Table: Suspicious items ----
if (!is.null(suspicious_all) && nrow(suspicious_all) > 0) {
  tbl_sus <- suspicious_all |>
    dplyr::transmute(
      Form          = .data$form,
      Item          = toupper(.data$item),
      `P (low Q)`   = .data$p_bottom_Q,
      `P (high Q)`  = .data$p_top_Q,
      `Top miss %`  = round(.data$top_miss_rate * 100, 1),
      `Item-Rest r` = .data$item_rest_r,
      Flags         = .data$n_flags,
      Suspicious    = .data$suspicious,
      `Flag details` = .data$flags
    ) |>
    dplyr::arrange(dplyr::desc(.data$Suspicious), dplyr::desc(.data$Flags))

  save_table(tbl_sus, "suspicious_items",
             subfolder = "psychometrics",
             caption   = paste0(
               "Items flagged for psychometric irregularities. ",
               "Criteria: top-quartile miss rate > ", round(top_miss_thr*100), "%; ",
               "bottom-quartile hit rate > ", round(bot_hit_thr*100), "%; ",
               "reversed or non-monotone discrimination pattern; ",
               "item-rest r < ", min_item_rest_r, "."))
}

# ---- Table: Missing response patterns ----
tbl_miss_item <- item_missing_all |>
  dplyr::filter(.data$pct_missing > 0) |>
  dplyr::transmute(
    Form         = .data$form,
    Item         = toupper(.data$item),
    `N total`    = .data$n_total,
    `N missing`  = .data$n_missing,
    `% missing`  = round(.data$pct_missing, 1)
  ) |>
  dplyr::arrange(dplyr::desc(.data$`% missing`))

if (nrow(tbl_miss_item) > 0) {
  save_table(tbl_miss_item, "item_missing_patterns",
             subfolder = "psychometrics",
             caption   = "Item-level missing response counts")
}

# =============================================================================
# NEW FIGURES
# =============================================================================

# ---- Figure: Ability-stratified item difficulty heatmap ----
if (!is.null(strat_all) && nrow(strat_all) > 0) {
  fig_strat <- strat_all |>
    dplyr::mutate(
      item_num   = as.numeric(gsub("^\\D+", "", .data$item)),
      item_label = toupper(.data$item)
    ) |>
    dplyr::arrange(.data$form, .data$item_num) |>
    dplyr::mutate(
      item_label = factor(.data$item_label, levels = unique(.data$item_label))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$ability_quartile,
                                  y = .data$item_label,
                                  fill = .data$p_correct)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(
      label = sprintf("%.0f%%", .data$p_correct * 100)),
      size = 2.8, colour = "grey10") +
    ggplot2::scale_fill_gradient2(
      low     = "#D62728", mid = "#FFDD89", high = "#2E8B57",
      midpoint = 0.50,
      limits  = c(0, 1),
      labels  = scales::percent_format(),
      name    = "P(correct)"
    ) +
    ggplot2::facet_wrap(~ form, scales = "free_y", ncol = 2) +
    ggplot2::xlab("Ability Quartile (Q1 = lowest)") +
    ggplot2::ylab("Item") +
    theme_clean() +
    ggplot2::theme(
      axis.text.y    = ggplot2::element_text(size = 7),
      panel.grid     = ggplot2::element_blank(),
      legend.position = "right"
    )

  nrows_strat <- max(length(x_cols_full), length(y_cols_full))
  save_figure(fig_strat, "ability_stratified_difficulty_heatmap",
              subfolder = "psychometrics",
              height    = max(4.5, nrows_strat * 0.35 + 1.5))
}

# ---- Figure: Suspicious items bubble chart
#      X = bottom-quartile hit rate, Y = top-quartile miss rate
#      bubble size = n_flags, colour = form
# ----
if (!is.null(suspicious_all) && nrow(suspicious_all) > 0) {
  sus_plot_data <- suspicious_all |>
    dplyr::mutate(
      item_label = toupper(.data$item),
      label_show = dplyr::if_else(.data$suspicious, .data$item_label, NA_character_)
    )

  fig_sus <- ggplot2::ggplot(sus_plot_data,
      ggplot2::aes(x    = .data$p_bottom_Q,
                   y    = .data$top_miss_rate,
                   size = .data$n_flags + 0.1,
                   colour = .data$form)) +
    ggplot2::geom_vline(xintercept = bot_hit_thr,  linetype = "dashed",
                        colour = "#1F77B4", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = top_miss_thr, linetype = "dashed",
                        colour = "#D62728", linewidth = 0.5) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_size_continuous(range = c(2, 7), name = "# flags",
                                   guide = "none") +
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        c(cfg_get("figures","color_intervention", default="#2E8B57"),
          cfg_get("figures","color_control",      default="#CD853F")),
        c(form_x_label, form_y_label)
      ), name = "Form"
    ) +
    ggplot2::scale_x_continuous(
      name   = paste0("Lowest-quartile hit rate (>",
                      round(bot_hit_thr*100), "% = suspicious)"),
      limits = c(0, 1), labels = scales::percent_format()
    ) +
    ggplot2::scale_y_continuous(
      name   = paste0("Highest-quartile miss rate (>",
                      round(top_miss_thr*100), "% = suspicious)"),
      limits = c(0, 1), labels = scales::percent_format()
    ) +
    ggplot2::annotate("rect",
      xmin = bot_hit_thr, xmax = 1,
      ymin = top_miss_thr, ymax = 1,
      fill = "red", alpha = 0.07) +
    theme_clean() +
    ggplot2::theme(legend.position = "right")

  # Add item labels for suspicious items if possible
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    fig_sus <- fig_sus +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = .data$label_show),
        size = 3, max.overlaps = 15, na.rm = TRUE)
  } else {
    fig_sus <- fig_sus +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$label_show),
        size = 3, vjust = -0.8, na.rm = TRUE)
  }

  save_figure(fig_sus, "suspicious_items_scatter",
              subfolder = "psychometrics", width = 7, height = 5.5)
}

# ---- Figure: Per-item correct vs incorrect total score boxplot ----
# For each item, compare total scores of participants who got it right vs wrong.
# Items where the "correct" group scores similarly to the "incorrect" group
# are discriminating poorly (echoes item-rest r but is visually clear).

fig_item_discrim_list <- lapply(list(
  list(items = x_items, cols = x_cols_full, form = form_x_label,
       total = x_total_scores),
  list(items = y_items, cols = y_cols_full, form = form_y_label,
       total = y_total_scores)
), function(fd) {
  if (length(fd$cols) == 0) return(NULL)
  mat <- as.matrix(dplyr::select(fd$items, dplyr::all_of(fd$cols)))
  storage.mode(mat) <- "numeric"

  long <- purrr::map_dfr(seq_along(fd$cols), function(i) {
    col <- fd$cols[i]
    response <- mat[, i]
    tibble::tibble(
      item     = toupper(col),
      item_num = i,
      response = factor(dplyr::if_else(response == 1L, "Correct", "Incorrect"),
                        levels = c("Incorrect", "Correct")),
      total_score = fd$total
    )
  }) |>
    dplyr::filter(!is.na(.data$response))

  n_items <- length(fd$cols)
  h       <- max(4, n_items * 0.4 + 1)

  fig <- ggplot2::ggplot(long,
      ggplot2::aes(x = .data$total_score, y = reorder(.data$item, .data$item_num),
                   fill = .data$response)) +
    ggplot2::geom_boxplot(
      width = 0.55, alpha = 0.7, outlier.size = 1.2,
      position = ggplot2::position_dodge(0.65),
      linewidth = 0.4, colour = "grey30"
    ) +
    ggplot2::scale_fill_manual(
      values = c(Correct = "#2E8B57", Incorrect = "#CD853F"),
      name   = "Response"
    ) +
    ggplot2::xlab("Total Score") +
    ggplot2::ylab("Item") +
    ggplot2::facet_wrap(~ "Correct vs Incorrect: Total Score Distribution",
                        ncol = 1) +
    theme_clean() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.y     = ggplot2::element_text(size = 7))

  list(fig = fig, form = fd$form, height = h)
})

for (fd in fig_item_discrim_list) {
  if (!is.null(fd))
    save_figure(fd$fig, paste0("item_discrimination_boxplot_",
                                tolower(gsub("\\s+", "_", fd$form))),
                subfolder = "psychometrics",
                height = fd$height, width = 7)
}

# ---- Figure: Missing patterns heatmap (items × participants) ----
for (form_info in list(
  list(items = x_items, cols = x_cols_full, form = form_x_label),
  list(items = y_items, cols = y_cols_full, form = form_y_label)
)) {
  mat <- as.matrix(dplyr::select(form_info$items,
                                  dplyr::all_of(form_info$cols)))
  if (!any(is.na(mat))) next  # skip if no missing data

  total_score <- rowSums(mat, na.rm = TRUE)
  order_idx   <- order(total_score)

  miss_df <- tibble::as_tibble(mat[order_idx, ]) |>
    dplyr::mutate(participant_rank = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols       = dplyr::all_of(form_info$cols),
      names_to   = "item",
      values_to  = "response"
    ) |>
    dplyr::mutate(
      item_num   = as.numeric(gsub("^\\D+", "", .data$item)),
      item_label = toupper(.data$item),
      status     = dplyr::case_when(
        is.na(.data$response)      ~ "Missing",
        .data$response == 1L       ~ "Correct",
        TRUE                       ~ "Incorrect"
      )
    ) |>
    dplyr::arrange(.data$item_num) |>
    dplyr::mutate(
      item_label = factor(.data$item_label, levels = unique(.data$item_label))
    )

  fig_miss <- ggplot2::ggplot(miss_df,
      ggplot2::aes(x = .data$participant_rank,
                   y = .data$item_label,
                   fill = .data$status)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(
      values = c(Correct   = "#2E8B57",
                 Incorrect = "#CD853F",
                 Missing   = "#888888"),
      name = "Response"
    ) +
    ggplot2::xlab("Participant (sorted by total score, low \u2192 high)") +
    ggplot2::ylab("Item") +
    ggplot2::labs(subtitle = form_info$form) +
    theme_clean() +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_blank(),
      axis.ticks.x  = ggplot2::element_blank(),
      panel.grid    = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.subtitle   = ggplot2::element_text(size = rel(0.9))
    )

  save_figure(fig_miss,
              paste0("response_heatmap_",
                     tolower(gsub("\\s+", "_", form_info$form))),
              subfolder = "psychometrics",
              width  = max(7, nrow(form_info$items) * 0.25 + 2),
              height = max(4, length(form_info$cols) * 0.35 + 1.5))
}

log_h2("PSYCHOMETRICS COMPLETE")
log_line("  Form X (full)  KR-20=", round(rel_x_full$kr20, 3),
         "  alpha=", round(rel_x_full$alpha_std, 3),
         "  omega_t=", round(rel_x_full$omega_t %||% NA_real_, 3))
log_line("  Form Y (full)  KR-20=", round(rel_y_full$kr20, 3),
         "  alpha=", round(rel_y_full$alpha_std, 3),
         "  omega_t=", round(rel_y_full$omega_t %||% NA_real_, 3))
log_line("  Items flagged (Form X): ",
         sum(ia_x_full$flag != "" & ia_x_full$flag != "excluded", na.rm = TRUE),
         " / ", length(x_cols_full))
log_line("  Items flagged (Form Y): ",
         sum(ia_y_full$flag != "" & ia_y_full$flag != "excluded", na.rm = TRUE),
         " / ", length(y_cols_full))
log_line("  DIF flags (sequence): ", sum(dif$dif_flag, na.rm = TRUE), " items")
log_line("  DIF flags (period)  : ", sum(dif_period$dif_period_flag, na.rm = TRUE), " items")
log_line("  Suspicious items    : ", sum(suspicious_all$suspicious, na.rm = TRUE),
         " / ", nrow(suspicious_all))
