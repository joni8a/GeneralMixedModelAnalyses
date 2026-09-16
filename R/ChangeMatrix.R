# ChangeMatrix.R
# ------------------------------------------------------------------
# Change between every pair of follow-up levels, within each group.
#
# `table_change_analysis()` answers "did this group change over the intervals
# I nominated". This file answers "over which intervals did it change at all",
# by testing every pair and laying the result out as a triangular matrix:
#
#   rows    = the earlier ("from") level
#   columns = the later ("to") level
#
# Reading a row across shows how far a group has moved from that level; reading
# the last column down shows the earliest level from which nothing further
# happens — which is how a stabilisation point is located.
# ------------------------------------------------------------------

#' Every pair of follow-up levels.
#'
#' Sibling to [consecutive_intervals()], which returns only adjacent pairs.
#' Adjacent pairs can show that nothing happens between two neighbouring
#' levels, but not that nothing happens from one level onwards, because the
#' longer-range pairs are never formed. This returns all `choose(n, 2)` of them.
#'
#' @param followup Vector of follow-up levels, in order. Numeric or character.
#' @return A list of `c(from, to)` pairs in from-major order, ready to pass as
#'   `intervals` to [table_change_analysis()].
#' @examples
#' all_intervals(c(1.5, 3, 6))
#' # [[1]] 1.5 3   [[2]] 1.5 6   [[3]] 3 6
#' @export
all_intervals <- function(followup) {
  if (length(followup) < 2)
    stop("`followup` needs at least 2 levels to form an interval.")
  if (anyDuplicated(followup))
    stop("`followup` must not contain duplicates.")

  idx <- utils::combn(seq_along(followup), 2)
  lapply(seq_len(ncol(idx)), function(k) followup[idx[, k]])
}

#' Within-group change between every pair of follow-up levels.
#'
#' Fits nothing itself: it contrasts the estimated marginal means of an already
#' fitted model. One matrix is produced per group, each cell holding the change
#' from the row level to the column level with its confidence interval.
#'
#' Confidence intervals are left unadjusted while p-values are adjusted, so the
#' two can disagree; the generated footnote says so. `adjust` is applied within
#' each group, so the family is the pairs of one group, not of the whole table.
#'
#' @param model_container A ModelContainer.
#' @param formula `~ group * time`, as for [table_change_analysis()].
#' @param followup Vector of follow-up levels, in order.
#' @param adjust Multiplicity adjustment passed to `summary()`: `"holm"`
#'   (default), `"none"`, `"BH"`, `"bonferroni"`, or any other `p.adjust`
#'   method emmeans accepts.
#' @param digits Rounding for the displayed estimate and interval.
#' @param value_sep Separator inside the interval parentheses.
#' @param phase_labels Optional display names for the follow-up levels.
#' @param group_labels Optional display names for the groups, in the order the
#'   groups appear in the data.
#' @param include_between If TRUE, append a matrix of the between-group
#'   difference in change. Requires exactly 2 groups.
#' @param body_font_size,header_font_size Font sizes.
#' @return A named list of flextables, one per group, plus
#'   `"Difference in change"` when `include_between = TRUE`. Pass it to
#'   `save_as_docx()` or to `report_to_word(fn_table_change_matrix = )`.
#' @export
table_change_matrix <- function(model_container,
                                formula,
                                followup,
                                adjust           = "holm",
                                digits           = 2,
                                value_sep        = " to ",
                                phase_labels     = NULL,
                                group_labels     = NULL,
                                include_between  = FALSE,
                                body_font_size   = 8,
                                header_font_size = 9) {

  # Same formula convention as table_change_analysis(): ~ group * time.
  factorVariable <- deparse(formula[[2]][[2]])   # the group, e.g. "Cemented"
  grouping_var   <- deparse(formula[[2]][[3]])   # the time,  e.g. "Month"
  model          <- model_container@lm

  if (!is.null(phase_labels) && length(phase_labels) != length(followup))
    stop("`phase_labels` must have ", length(followup), " entries.")

  # ---- Contrast for every pair, named c1..cN ------------------------------
  # Opaque names rather than "Phase 6 - Phase 3": the from/to pair is carried
  # in `lookup` and joined back, so nothing downstream parses a label and no
  # name can collide with a level of the time variable.
  pairs  <- all_intervals(followup)
  custom <- vector("list", length(pairs))
  lookup <- vector("list", length(pairs))

  for (k in seq_along(pairs)) {
    from <- pairs[[k]][1]
    to   <- pairs[[k]][2]

    vec <- numeric(length(followup))
    vec[which(followup == to)]   <-  1      # convention: to - from
    vec[which(followup == from)] <- -1

    nm          <- paste0("c", k)
    custom[[k]] <- vec
    lookup[[k]] <- data.frame(contrast = nm,
                              from     = as.character(from),
                              to       = as.character(to),
                              stringsAsFactors = FALSE)
  }
  names(custom) <- paste0("c", seq_along(pairs))
  lookup        <- bind_rows(lookup)

  at_list <- setNames(list(followup), grouping_var)
  emm     <- emmeans(model, formula, at = at_list)
  emm     <- .maybe_regrid(emm, model_container)

  # ---- Within-group changes ----------------------------------------------
  # With `by`, the adjustment applies within each group, which is the family
  # we want.
  within_df <- .ci_and_p(contrast(emm, method = custom, by = factorVariable),
                         adjust, model_container) %>%
    mutate(contrast = as.character(contrast)) %>%
    left_join(lookup, by = "contrast")

  grp_levels <- unique(as.character(within_df[[factorVariable]]))
  if (is.null(group_labels)) group_labels <- grp_levels
  if (length(group_labels) != length(grp_levels))
    stop("`group_labels` must have ", length(grp_levels), " entries.")

  n_pairs  <- length(pairs)
  from_lev <- utils::head(as.character(followup), -1L)
  to_lev   <- utils::tail(as.character(followup), -1L)
  disp     <- setNames(
    if (is.null(phase_labels)) as.character(followup) else as.character(phase_labels),
    as.character(followup))

  fmt <- function(e, lo, hi)
    sprintf(paste0("%.", digits, "f (%.", digits, "f", value_sep, "%.", digits, "f)"),
            e, lo, hi)

  note <- .adjust_note(adjust, n_pairs)

  build <- function(d, caption) {
    d <- d %>% mutate(cell = fmt(estimate, lower.CL, upper.CL),
                      sig  = !is.na(p.value) & p.value < 0.05)

    cells <- matrix("",    length(from_lev), length(to_lev),
                    dimnames = list(from_lev, to_lev))
    bolds <- matrix(FALSE, length(from_lev), length(to_lev),
                    dimnames = list(from_lev, to_lev))
    for (i in seq_len(nrow(d))) {
      cells[d$from[i], d$to[i]] <- d$cell[i]
      bolds[d$from[i], d$to[i]] <- d$sig[i]
    }

    tab <- data.frame(From = unname(disp[from_lev]), cells,
                      check.names = FALSE, stringsAsFactors = FALSE)
    names(tab)[-1] <- unname(disp[to_lev])

    ft <- flextable(tab) %>%
      flextable::font(fontname = "Arial", part = "all") %>%
      flextable::fontsize(size = body_font_size,   part = "body") %>%
      flextable::fontsize(size = header_font_size, part = "header") %>%
      set_caption(caption) %>%
      border_remove() %>%
      hline_top(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
      hline_bottom(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
      hline_bottom(part = "body",   border = fp_border(color = "black", width = 1.5)) %>%
      align(align = "center", part = "header") %>%
      align(j = seq_along(to_lev) + 1L, align = "center", part = "body") %>%
      align(j = "From", align = "left", part = "body") %>%
      bold(j = "From", part = "body") %>%
      valign(valign = "top", part = "body") %>%
      add_footer_lines(note) %>%
      flextable::fontsize(size = body_font_size - 1L, part = "footer") %>%
      autofit()

    for (jj in seq_along(to_lev)) {
      ii <- which(bolds[, jj])
      if (length(ii) > 0) ft <- bold(ft, i = ii, j = jj + 1L, part = "body")
    }
    ft
  }

  out <- list()
  for (g in seq_along(grp_levels)) {
    d <- within_df %>% filter(.data[[factorVariable]] == grp_levels[g])
    out[[group_labels[g]]] <- build(d, group_labels[g])
  }

  # ---- Between-group difference in change --------------------------------
  if (isTRUE(include_between)) {
    if (length(grp_levels) != 2)
      stop("`include_between = TRUE` needs exactly 2 groups; found ",
           length(grp_levels), ": ", paste(grp_levels, collapse = ", "))

    specs <- list()
    specs[[grouping_var]]   <- custom
    specs[[factorVariable]] <- "pairwise"

    between_df <- .ci_and_p(contrast(emm, interaction = specs),
                            adjust, model_container) %>%
      rename(contrast := matches(paste0("^", grouping_var, "_"))) %>%
      mutate(contrast = as.character(contrast)) %>%
      left_join(lookup, by = "contrast")

    out[["Difference in change"]] <- build(
      between_df,
      paste0("Difference in change, ", group_labels[1], " - ", group_labels[2]))
  }

  out
}

# Unadjusted intervals with adjusted p-values.
#
# emmeans applies `adjust` to the intervals as well as the tests: "holm" has no
# interval analogue, so it falls back to Bonferroni-width intervals, which on a
# family of 15 widens them by about half. The convention in these analyses is
# an adjusted p-value beside an unadjusted interval, so the two are summarised
# separately and joined on whatever labelling columns the contrast carries
# (`contrast`, plus the `by` variable when there is one).
.ci_and_p <- function(ct, adjust, model_container) {
  ci <- summary(ct, infer = c(TRUE, FALSE), adjust = "none") %>%
    as_tibble() %>%
    .standardize_emm_cols() %>%
    .apply_response_scale(model_container)

  pv <- summary(ct, infer = c(FALSE, TRUE), adjust = adjust) %>%
    as_tibble()

  keys <- intersect(names(ci), names(pv))
  keys <- keys[vapply(ci[keys], function(x) is.character(x) || is.factor(x),
                      logical(1))]
  if (length(keys) == 0)
    stop("Could not identify columns to join estimates and p-values on.")

  left_join(ci, pv[c(keys, "p.value")], by = keys)
}

# Footnote describing what was adjusted and what was not. Kept separate so the
# wording stays identical wherever a matrix is rendered.
.adjust_note <- function(adjust, n_pairs) {
  label <- switch(adjust,
                  none       = "unadjusted",
                  holm       = "Holm-adjusted",
                  bonferroni = "Bonferroni-adjusted",
                  BH         = "Benjamini-Hochberg-adjusted",
                  fdr        = "Benjamini-Hochberg-adjusted",
                  tukey      = "Tukey-adjusted",
                  paste0(adjust, "-adjusted"))

  base <- paste0("Change from the row visit to the column visit, as model-based ",
                 "estimates with unadjusted 95% CI. ")

  if (identical(adjust, "none"))
    return(paste0(base, "P-values are unadjusted across the ", n_pairs,
                  " visit pairs within each group. Cells in bold have p < 0.05."))

  paste0(base, "P-values are ", label, " across the ", n_pairs,
         " visit pairs within each group; cells in bold have an adjusted ",
         "p < 0.05. A cell may therefore have an interval excluding 0 without ",
         "being bold.")
}
