# ChangeAnalysisCombined.R
# ------------------------------------------------------------------
# Fuse the per-outcome EMM contrast tables (one model per KOOS subscore)
# into a SINGLE wide flextable:
#   * rows    = follow-up phases
#   * columns = one block per subscore: [group 1 mean | group 2 mean | difference]
#   * the subscore name spans its block as a top header row
#
# Depends on the same project setup as ChangeAnalysis.R:
#   packages: emmeans, dplyr, tidyr, tibble, flextable, officer
#   helpers : .maybe_regrid(), .standardize_emm_cols()   (from your utils)
# ------------------------------------------------------------------

#' Combine EMM contrast tables across several outcomes into one wide table.
#'
#' @param models           Named list of model containers (S4 objects with an @lm
#'                          slot), one per subscore. The list names become the
#'                          subscore column headers unless `subscore_labels` is given.
#' @param formula          emmeans-style formula `group | by`
#'                          (e.g. `~ HighImprover | FollowUp`).
#'                          Operand 1 = the 2-level grouping factor -> the two group
#'                          columns; operand 2 = the by/phase variable -> the rows.
#' @param followup         Vector of follow-up levels, in the row order you want.
#' @param subscore_labels  Optional header labels (default `names(models)`).
#' @param group_labels     Optional length-2 labels for the two group columns
#'                          (default = the factor levels).
#' @param diff_label       Header for each difference column.
#' @param phase_labels     Optional relabelling of the phase rows (length == #phases).
#' @param value_sep        Separator inside the CI parentheses (default " to ").
#' @param digits           Rounding digits (default 2).
#' @param show_p           If TRUE, add a p-value column per subscore (default FALSE).
#' @param bold_sig         If TRUE, bold each difference cell when p < 0.05.
#' @return A flextable. The tidy data is attached as `attr(ft, "data")`.
#' @export
table_emm_contrasts_combined <- function(models,
                                         formula,
                                         followup,
                                         subscore_labels  = NULL,
                                         group_labels     = NULL,
                                         diff_label       = "Mean difference (95% CI)",
                                         phase_labels     = NULL,
                                         value_sep        = " to ",
                                         digits           = 2,
                                         show_p           = FALSE,
                                         bold_sig         = TRUE,
                                         body_font_size   = 9,
                                         header_font_size = 10) {

  if (!is.list(models) || length(models) < 1)
    stop("`models` must be a (named) list of model containers, one per subscore.")
  if (!exists(".maybe_regrid"))
    stop("Helper `.maybe_regrid()` not found - source your ChangeAnalysis.R utils first.")

  # ---- Parse formula:  group | by  -------------------------------------
  lhs       <- formula[[2]]
  group_var <- deparse(lhs[[2]])   # e.g. HighImprover -> the two group columns
  by_var    <- deparse(lhs[[3]])   # e.g. FollowUp     -> the rows
  emm_form  <- as.formula(paste0("~ ", group_var, " | ", by_var))

  if (is.null(subscore_labels)) {
    subscore_labels <- names(models)
    if (is.null(subscore_labels) || any(subscore_labels == ""))
      stop("Supply a *named* `models` list or pass `subscore_labels`.")
  }
  if (length(subscore_labels) != length(models))
    stop("`subscore_labels` must have one entry per model.")

  keys <- paste0("s", seq_along(models))   # internal, syntactic column keys

  # ---- small helpers ----------------------------------------------------
  fmt_ci <- function(est, lo, hi)
    sprintf(paste0("%.", digits, "f (%.", digits, "f", value_sep, "%.", digits, "f)"),
            est, lo, hi)

  fmt_p <- function(p)
    ifelse(is.na(p), "",
           ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))

  # Use your standardizer if it applies, then coalesce any remaining variants
  std_cols <- function(df) {
    out <- tryCatch(.standardize_emm_cols(df), error = function(e) df)
    rn  <- function(d, from, to) {
      if (from %in% names(d) && !(to %in% names(d)))
        names(d)[names(d) == from] <- to
      d
    }
    out <- rn(out, "emmean",    "estimate")
    out <- rn(out, "response",  "estimate")
    out <- rn(out, "asymp.LCL", "lower.CL")
    out <- rn(out, "asymp.UCL", "upper.CL")
    out <- rn(out, "lower.HPD", "lower.CL")
    out <- rn(out, "upper.HPD", "upper.CL")
    out
  }

  grp_levels <- NULL

  # ---- process one subscore --------------------------------------------
  process_one <- function(mc, key) {
    model   <- mc@lm
    at_list <- setNames(list(followup), by_var)

    emm <- emmeans::emmeans(model, emm_form, at = at_list)
    emm <- .maybe_regrid(emm, mc)        # back-transform if log-fitted

    # ---- group means ----
    means_df <- summary(emm, infer = c(TRUE, FALSE)) %>%
      as_tibble() %>% std_cols()

    if (is.null(grp_levels)) {
      lv <- means_df[[group_var]]
      grp_levels <<- if (is.factor(lv)) levels(droplevels(lv))
                     else unique(as.character(lv))
      if (length(grp_levels) != 2)
        stop("Expected exactly 2 groups in '", group_var,
             "'; found: ", paste(grp_levels, collapse = ", "))
    }

    means_wide <- means_df %>%
      mutate(
        .phase = as.character(.data[[by_var]]),
        .gidx  = match(as.character(.data[[group_var]]), grp_levels),
        .gcol  = paste0(key, "_g", .gidx),
        .val   = fmt_ci(estimate, lower.CL, upper.CL)
      ) %>%
      select(.phase, .gcol, .val) %>%
      tidyr::pivot_wider(names_from = .gcol, values_from = .val)

    # ---- between-group difference (pairwise) by phase ----
    ctr    <- emmeans::contrast(emm, method = "pairwise", by = by_var)
    ctr_df <- summary(ctr, infer = c(TRUE, TRUE)) %>%
      as_tibble() %>% std_cols() %>%
      mutate(
        .phase                 = as.character(.data[[by_var]]),
        !!paste0(key, "_diff") := fmt_ci(estimate, lower.CL, upper.CL),
        !!paste0(key, "_p")    := p.value
      ) %>%
      select(.phase, ends_with("_diff"), ends_with("_p"))

    left_join(means_wide, ctr_df, by = ".phase")
  }

  parts <- Map(process_one, models, keys)
  wide  <- Reduce(function(a, b) full_join(a, b, by = ".phase"), parts)

  # ---- order rows by requested follow-up order -------------------------
  wide <- wide %>%
    mutate(.phase = factor(.phase, levels = as.character(followup))) %>%
    arrange(.phase) %>%
    mutate(.phase = as.character(.phase))

  if (!is.null(phase_labels)) {
    if (length(phase_labels) != nrow(wide))
      stop("`phase_labels` must have ", nrow(wide), " entries (one per phase).")
    wide$.phase <- as.character(phase_labels)
  }

  # significance vectors (before p columns are dropped/formatted)
  sig_list <- setNames(
    lapply(keys, function(k) {
      p <- wide[[paste0(k, "_p")]]
      !is.na(p) & p < 0.05
    }), keys)

  if (is.null(group_labels)) group_labels <- grp_levels
  if (length(group_labels) != 2) stop("`group_labels` must be length 2.")

  # ---- column order + p handling ---------------------------------------
  per          <- if (show_p) c("_g1", "_g2", "_diff", "_p")
                  else        c("_g1", "_g2", "_diff")
  ordered_cols <- c(".phase", as.vector(t(outer(keys, per, paste0))))

  if (show_p) {
    for (k in keys) wide[[paste0(k, "_p")]] <- fmt_p(wide[[paste0(k, "_p")]])
  } else {
    wide <- wide %>% select(-ends_with("_p"))
  }
  wide  <- wide %>% select(all_of(ordered_cols))
  n_per <- length(per)

  # ---- header labels ----------------------------------------------------
  bottom <- list(.phase = "Phase")
  for (k in keys) {
    bottom[[paste0(k, "_g1")]]   <- group_labels[1]
    bottom[[paste0(k, "_g2")]]   <- group_labels[2]
    bottom[[paste0(k, "_diff")]] <- diff_label
    if (show_p) bottom[[paste0(k, "_p")]] <- "p"
  }

  # ---- build flextable --------------------------------------------------
  ft <- flextable(wide) %>%
    set_header_labels(values = bottom) %>%
    add_header_row(top = TRUE,
                   values    = c("Phase", subscore_labels),
                   colwidths = c(1, rep(n_per, length(keys)))) %>%
    merge_v(part = "header", j = ".phase") %>%

    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = body_font_size,   part = "body") %>%
    flextable::fontsize(size = header_font_size, part = "header") %>%

    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top",    part = "body") %>%

    border_remove() %>%
    hline_top(part = "header",    border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1.5))

  # vertical separators between subscore blocks
  block_ends <- vapply(keys, function(k) paste0(k, "_diff"), character(1))
  block_ends <- head(block_ends, -1)               # not after the last block
  ft <- ft %>% vline(j = ".phase", part = "all",
                     border = fp_border(color = "grey80", width = 0.5))
  if (length(block_ends))
    ft <- ft %>% vline(j = block_ends, part = "all",
                       border = fp_border(color = "grey80", width = 0.5))

  # bold significant differences
  if (bold_sig) {
    for (k in keys) {
      idx <- which(sig_list[[k]])
      if (length(idx))
        ft <- ft %>% bold(i = idx, j = paste0(k, "_diff"), part = "body")
    }
  }

  # widths (a 5-subscore table is very wide - use landscape; see notes)
  ft <- ft %>% width(j = ".phase", width = 0.7)
  for (k in keys) {
    ft <- ft %>%
      width(j = paste0(k, "_g1"),   width = 1.15) %>%
      width(j = paste0(k, "_g2"),   width = 1.15) %>%
      width(j = paste0(k, "_diff"), width = 1.30)
    if (show_p) ft <- ft %>% width(j = paste0(k, "_p"), width = 0.6)
  }

  ft <- ft %>% delete_part(part = "footer")
  attr(ft, "data") <- wide          # tidy data, in case you want to re-style
  ft
}
