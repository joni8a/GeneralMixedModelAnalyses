# ChangeAnalysis.R
# This script provides functions to calculate and visualize
# "Differences of Differences" for dynamic RSA models.
# It now also shows per-group changes alongside the difference.

#' @export
table_change_analysis <- function(model_container, formula, followup, intervals) {

  # 1. Setup variables
  grouping_var   <- deparse(formula[[2]][[3]]) # e.g., "Phase"
  factorVariable <- deparse(formula[[2]][[2]]) # e.g., "BearingType"
  model <- model_container@lm

  # 2. Generate Custom Contrasts
  # Convention: c(from, to) computes "to - from"
  custom_contrasts <- list()
  interval_order   <- character(0)

  for (val in intervals) {
    from <- val[1]
    to   <- val[2]

    # Validate that from and to values exist in followup
    if (!(from %in% followup)) {
      stop("Interval value '", from, "' not found in followup: ",
           paste(followup, collapse = ", "))
    }
    if (!(to %in% followup)) {
      stop("Interval value '", to, "' not found in followup: ",
           paste(followup, collapse = ", "))
    }

    vec <- numeric(length(followup))
    vec[which(followup == to)]   <-  1
    vec[which(followup == from)] <- -1

    name <- paste0("Phase ", to, " - Phase ", from)
    custom_contrasts[[name]] <- vec
    interval_order <- c(interval_order, name)
  }

  # 3. Calculate Emmeans
  at_list <- setNames(list(followup), grouping_var)
  emm <- emmeans(model, formula, at = at_list)

  # Back-transform to original scale if the model was fit on log-transformed data
  emm <- .maybe_regrid(emm, model_container)

  # ------------------------------------------------------------------
  # 4A. Per-group changes (change within each level of the factor)
  # ------------------------------------------------------------------
  group_changes <- contrast(emm, method = custom_contrasts, by = factorVariable)
  group_df <- summary(group_changes, infer = c(TRUE, TRUE)) %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(
      Interval             = contrast,
      Group                = as.character(.data[[factorVariable]]),
      `Change (95% CI)`    = paste0(estimate, " (", lower.CL, " - ", upper.CL, ")"),
      `Difference (95% CI)` = "",
      p_value              = NA_real_,
      p_fmt                = "",
      row_type             = "group"
    ) %>%
    select(Interval, Group, `Change (95% CI)`,
           `Difference (95% CI)`, p_value, p_fmt, row_type)

  # ------------------------------------------------------------------
  # 4B. Difference of differences (interaction contrast)
  # ------------------------------------------------------------------
  interaction_specs <- list()
  interaction_specs[[grouping_var]]   <- custom_contrasts
  interaction_specs[[factorVariable]] <- "pairwise"

  diff_of_difs <- contrast(emm, interaction = interaction_specs)
  diff_df <- summary(diff_of_difs, infer = c(TRUE, TRUE)) %>%
    as_tibble() %>%
    rename(
      Interval := matches(paste0("^", grouping_var, "_")),
      Group    := matches(paste0("^", factorVariable, "_"))
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(
      `Change (95% CI)`     = "",
      `Difference (95% CI)` = paste0(estimate, " (", lower.CL, " - ", upper.CL, ")"),
      p_value               = p.value,
      p_fmt                 = if_else(p.value < 0.001, "< 0.001", as.character(p.value)),
      row_type              = "diff"
    ) %>%
    select(Interval, Group, `Change (95% CI)`,
           `Difference (95% CI)`, p_value, p_fmt, row_type)

  # ------------------------------------------------------------------
  # 5. Combine & interleave (group rows first, then diff row per interval)
  # ------------------------------------------------------------------
  tbl_data <- bind_rows(group_df, diff_df) %>%
    mutate(
      Interval = factor(Interval, levels = interval_order),
      row_type = factor(row_type, levels = c("group", "diff"))
    ) %>%
    arrange(Interval, row_type, Group) %>%
    select(Interval, Group, `Change (95% CI)`,
           `Difference (95% CI)`, p_value, p_fmt)

  # Row indices at interval boundaries (for horizontal lines)
  line_indices <- tbl_data %>%
    mutate(row_num = row_number()) %>%
    group_by(Interval) %>%
    summarise(last_row = max(row_num), .groups = "drop") %>%
    pull(last_row)

  # ------------------------------------------------------------------
  # 6. Create Flextable
  # ------------------------------------------------------------------
  ft <- tbl_data %>%
    flextable() %>%

    # -- A. FONTS & SIZING --
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = 10, part = "body") %>%
    flextable::fontsize(size = 11, part = "header") %>%

    # Adjust column widths
    width(j = "Interval",             width = 1.6) %>%
    width(j = "Group",                width = 1.6) %>%
    width(j = "Change (95% CI)",      width = 2.0) %>%
    width(j = "Difference (95% CI)",  width = 2.0) %>%
    width(j = "p_fmt",                width = 0.8) %>%

    # -- B. MERGING --
    merge_v(j = "Interval") %>%

    # -- C. BORDERS & ALIGNMENT --
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "body",   border = fp_border(color = "black", width = 1.5)) %>%
    hline(i = line_indices, part = "body",
          border = fp_border(color = "grey", width = 0.5)) %>%

    valign(valign = "top", part = "body") %>%
    align(align = "center", part = "header") %>%
    align(j = 3:6, align = "center", part = "body") %>%

    # -- D. CONDITIONAL FORMATTING --
    bold(i = ~ p_value < 0.05, j = "p_fmt",                bold = TRUE) %>%
    bold(i = ~ p_value < 0.05, j = "Difference (95% CI)",  bold = TRUE) %>%

    # -- E. LABELS --
    set_header_labels(p_fmt = "p-value") %>%

    # -- F. CLEANUP --
    delete_part(part = "footer")

  # Hide the raw p_value column (used only for conditional bolding logic)
  ft <- ft %>% void(j = "p_value", part = "all")

  return(ft)
}