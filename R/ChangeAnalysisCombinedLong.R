# ChangeAnalysisCombinedLong.R
# ------------------------------------------------------------------
# Long-format variant of the combined EMM contrast table.
#
# Layout:
#   rows    = phase header rows (bold) + one row per subscore within each phase
#   columns = Follow-Up | Group 1 mean | Group 2 mean | Mean Difference (95% CI)
# ------------------------------------------------------------------

#' Combined EMM contrast table in long (tall) format.
#'
#' Produces a table where follow-up phases appear as bold row headers, with
#' one data row per outcome (subscore) nested beneath each phase.
#'
#' @param models           Named list of ModelContainer objects, one per subscore.
#' @param formula          emmeans-style formula `~ group | by`.
#' @param followup         Vector of follow-up levels, in the row order you want.
#' @param subscore_labels  Optional display names for the subscores (default `names(models)`).
#' @param group_labels     Optional length-2 labels for the two group columns.
#' @param diff_label       Header for the difference column.
#' @param phase_labels     Optional relabelling of the phase header rows (length == length(followup)).
#' @param value_sep        Separator inside the CI parentheses (default " - ").
#' @param digits           Rounding digits (default 2).
#' @param show_p           If TRUE, add a p-value column (default FALSE).
#' @param bold_sig         If TRUE, bold difference cells where p < 0.05 (default TRUE).
#' @param body_font_size   Font size for body rows (default 9).
#' @param header_font_size Font size for column headers (default 10).
#' @return A flextable.
#' @export
table_emm_contrasts_combined_long <- function(models,
                                              formula,
                                              followup,
                                              subscore_labels  = NULL,
                                              group_labels     = NULL,
                                              diff_label       = "Mean difference (95% CI)",
                                              phase_labels     = NULL,
                                              value_sep        = " - ",
                                              digits           = 2,
                                              show_p           = FALSE,
                                              bold_sig         = TRUE,
                                              body_font_size   = 9,
                                              header_font_size = 10) {

  if (!is.list(models) || length(models) < 1)
    stop("`models` must be a (named) list of model containers, one per subscore.")

  # ---- Parse formula:  group | by ------------------------------------------
  lhs       <- formula[[2]]
  group_var <- deparse(lhs[[2]])
  by_var    <- deparse(lhs[[3]])
  emm_form  <- as.formula(paste0("~ ", group_var, " | ", by_var))

  if (is.null(subscore_labels)) {
    subscore_labels <- names(models)
    if (is.null(subscore_labels) || any(subscore_labels == ""))
      stop("Supply a *named* `models` list or pass `subscore_labels`.")
  }
  if (length(subscore_labels) != length(models))
    stop("`subscore_labels` must have one entry per model.")

  # ---- Helpers -------------------------------------------------------------
  fmt_ci <- function(est, lo, hi)
    sprintf(paste0("%.", digits, "f (%.", digits, "f", value_sep, "%.", digits, "f)"),
            est, lo, hi)

  fmt_p <- function(p)
    ifelse(is.na(p), "",
           ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))

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

  # ---- Extract EMMs + contrast for one model/subscore ----------------------
  process_one <- function(mc, label) {
    at_list <- setNames(list(followup), by_var)
    emm     <- emmeans::emmeans(mc@lm, emm_form, at = at_list)
    emm     <- .maybe_regrid(emm, mc)

    means_df <- summary(emm, infer = c(TRUE, FALSE)) %>%
      as_tibble() %>% std_cols()

    if (is.null(grp_levels)) {
      lv <- means_df[[group_var]]
      grp_levels <<- if (is.factor(lv)) levels(droplevels(lv))
                     else unique(as.character(lv))
      if (length(grp_levels) != 2)
        stop("Expected exactly 2 groups; found: ", paste(grp_levels, collapse = ", "))
    }

    means_wide <- means_df %>%
      mutate(
        .phase = as.character(.data[[by_var]]),
        .gidx  = match(as.character(.data[[group_var]]), grp_levels),
        .val   = fmt_ci(estimate, lower.CL, upper.CL)
      ) %>%
      select(.phase, .gidx, .val) %>%
      tidyr::pivot_wider(names_from = .gidx, values_from = .val, names_prefix = "g")

    ctr_df <- summary(
      emmeans::contrast(emm, method = "pairwise", by = by_var),
      infer = c(TRUE, TRUE)
    ) %>%
      as_tibble() %>% std_cols() %>%
      mutate(
        .phase = as.character(.data[[by_var]]),
        diff   = fmt_ci(estimate, lower.CL, upper.CL),
        p_val  = p.value
      ) %>%
      select(.phase, diff, p_val)

    left_join(means_wide, ctr_df, by = ".phase") %>%
      mutate(subscore = label)
  }

  # ---- Combine all subscores -----------------------------------------------
  long_data <- bind_rows(Map(process_one, models, subscore_labels)) %>%
    mutate(.phase = factor(.phase, levels = as.character(followup))) %>%
    arrange(.phase, match(subscore, subscore_labels))

  if (is.null(group_labels)) group_labels <- grp_levels
  if (length(group_labels) != 2) stop("`group_labels` must be length 2.")

  phase_display <- if (!is.null(phase_labels)) {
    if (length(phase_labels) != length(followup))
      stop("`phase_labels` must have ", length(followup), " entries.")
    setNames(as.character(phase_labels), as.character(followup))
  } else {
    setNames(as.character(followup), as.character(followup))
  }

  # ---- Interleave phase header rows + data rows ----------------------------
  rows     <- list()
  sig_rows <- integer(0)
  row_idx  <- 0L

  for (ph in levels(long_data$.phase)) {
    ph_data <- long_data %>% filter(.phase == ph)

    # Phase header row
    row_idx <- row_idx + 1L
    rows[[row_idx]] <- tibble(
      follow_up  = phase_display[[ph]],
      g1         = "",
      g2         = "",
      difference = "",
      p_val      = NA_real_,
      row_type   = "header"
    )

    # One data row per subscore
    for (i in seq_len(nrow(ph_data))) {
      row_idx <- row_idx + 1L
      p <- ph_data$p_val[i]
      rows[[row_idx]] <- tibble(
        follow_up  = ph_data$subscore[i],
        g1         = coalesce(ph_data$g1[i], ""),
        g2         = coalesce(ph_data$g2[i], ""),
        difference = coalesce(ph_data$diff[i], ""),
        p_val      = p,
        row_type   = "data"
      )
      if (!is.na(p) && p < 0.05) sig_rows <- c(sig_rows, row_idx)
    }
  }

  tbl <- bind_rows(rows)

  header_rows <- which(tbl$row_type == "header")
  data_rows   <- which(tbl$row_type == "data")
  # Horizontal lines at the end of each phase block (before the next header)
  hline_rows  <- if (length(header_rows) > 1) header_rows[-1] - 1L else integer(0)

  # ---- Build flextable -----------------------------------------------------
  ft <- tbl %>%
    select(follow_up, g1, g2, difference, p_val) %>%
    flextable() %>%

    set_header_labels(
      follow_up  = "Follow-Up",
      g1         = group_labels[1],
      g2         = group_labels[2],
      difference = diff_label,
      p_val      = "p-value"
    ) %>%

    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = body_font_size,   part = "body") %>%
    flextable::fontsize(size = header_font_size, part = "header") %>%

    # Borders
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1.5)) %>%

    # Alignment
    align(align = "center", part = "header") %>%
    align(j = c("g1", "g2", "difference", "p_val"), align = "center", part = "body") %>%
    valign(valign = "top", part = "body") %>%

    # Phase header rows: left-aligned, bold
    align(i = header_rows, j = "follow_up", align = "left", part = "body") %>%
    bold(i = header_rows, bold = TRUE) %>%

    # Subscore rows: right-aligned in first column to create visual indent
    align(i = data_rows, j = "follow_up", align = "right", part = "body") %>%

    # Column widths
    width(j = "follow_up",  width = 1.2) %>%
    width(j = "g1",         width = 1.5) %>%
    width(j = "g2",         width = 1.5) %>%
    width(j = "difference", width = 1.8) %>%
    width(j = "p_val",      width = 0.7) %>%

    delete_part(part = "footer")

  # Horizontal lines between phase blocks
  if (length(hline_rows) > 0)
    ft <- ft %>% hline(i = hline_rows, part = "body",
                       border = fp_border(color = "grey60", width = 0.5))

  # Bold significant differences
  if (bold_sig && length(sig_rows) > 0)
    ft <- ft %>%
      bold(i = sig_rows, j = "difference", bold = TRUE) %>%
      bold(i = sig_rows, j = "p_val",      bold = TRUE)

  # Hide or format p-value column
  if (show_p) {
    ft <- ft %>%
      compose(j = "p_val", part = "body",
              value = as_paragraph(fmt_p(tbl$p_val)))
  } else {
    ft <- ft %>% void(j = "p_val", part = "all")
  }

  ft
}
