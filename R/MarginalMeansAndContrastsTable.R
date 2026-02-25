# MarginalMeansAndContrastsTable.R

library(emmeans)
library(dplyr)
library(flextable)
library(officer)

#' @export
table_emm_contrasts <- function(model, formula, followup) {
  # 1. Setup variables and data
  grouping_var <- deparse(formula[[2]][[3]]) # e.g., "Phase"
  factorVariable <- deparse(formula[[2]][[2]]) # e.g., "BearingType"
  
  at_list <- setNames(list(followup), grouping_var)
  
  # 2. Get EMMeans and Contrasts
  emm <- emmeans(model@lm, formula, at = at_list)
  emm_df <- as.data.frame(emm)
  contrast_df <- as.data.frame(contrast(emm, method = "pairwise", infer = c(TRUE, TRUE)))

  # 3. Merge them into one dataframe
  # We print grouping_var just for debugging visibility in console
  print(paste("Grouping by:", grouping_var)) 
  df <- merge(emm_df, contrast_df, by = grouping_var, suffix = c("_emm", "_contrast"))

  caption <- paste0(model@predictor_variable, ", Marginal Means (95% CI) and Contrasts (95% CI).")
  footnote <- paste0(model@name, ". ", deparse(formula(model@lm)), collapse = " ")

  # 4. Calculate formatting indices for the table
  line_indices <- df %>%
    group_by(!!sym(grouping_var)) %>%
    summarise(last_row = n()) %>%
    mutate(cum_row = cumsum(last_row)) %>%
    pull(cum_row)

  # 5. Extract original levels dynamically to avoid NA errors
  # This handles both Factor and Character data types safely
  original_levels <- levels(as.factor(model@data[[factorVariable]]))

  # 6. Build the Table
  tbl <- df %>%
    select(!!sym(grouping_var), !!sym(factorVariable),
           emmean, lower.CL_emm, upper.CL_emm,
           estimate, lower.CL_contrast, upper.CL_contrast,
           p.value) %>%
    # Round numerics
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    # Handle p-value display logic (show only on first row of group)
    group_by(!!sym(grouping_var)) %>%
    mutate(
      p.value = ifelse(row_number() == 1, p.value, NA)
    ) %>%
    ungroup() %>%
    # Sort
    group_by(!!sym(grouping_var), !!sym(factorVariable)) %>%
    arrange(!!sym(grouping_var)) %>%
    ungroup() %>%
    # Format text columns
    mutate(
      Mean = paste0(emmean, " (", lower.CL_emm, " - ", upper.CL_emm, ")"),
      Difference = paste0(estimate, " (", lower.CL_contrast, " - ", upper.CL_contrast, ")"),
    ) %>%
    select(
      !!sym(grouping_var), !!sym(factorVariable), Mean, Difference, p.value
    ) %>%
    rename(
      Group = !!sym(factorVariable),
      Phase = !!sym(grouping_var),
      `p-value` = p.value
    )
  
  # Apply group labels if group_mapping is available
  # Use named lookup instead of positional mapping to avoid label swapping
  if (!is.null(model@group_mapping) && !is.null(model@group_mapping$labels)) {
    # Create a named vector for lookup: original_level -> display_label
    # If labels are named (e.g., c("A" = "Label A")), use names as keys
    # If labels are unnamed, assume they match the order of names(colors)
    labels_vec <- model@group_mapping$labels
    if (is.null(names(labels_vec))) {
      # Unnamed labels: use names from colors as keys
      names(labels_vec) <- names(model@group_mapping$colors)
    }
    
    tbl <- tbl %>%
      mutate(
        Group = labels_vec[as.character(Group)]
      )
  }
  
  tbl <- tbl %>%
    flextable() %>%
    
    # --- NAMESPACE FIX HERE ---
    flextable::font(fontname = "Arial") %>%
    
    fontsize(size = 10, part = "body") %>%
    set_caption(as_paragraph(as_b(caption)), align_with_table = FALSE) %>%
    add_footer_row(value = as_paragraph(footnote), colwidth = c(5)) %>%
    fontsize(part = "footer", size = 8)  %>%
    width(1:1, 1.0) %>%
    width(3:4, 1.9) %>%
    merge_v(1:1) %>%       # Merge Phase column
    merge_v(j = "Group") %>% # Merge Group column
    merge_v(4:4) %>%       # Merge Difference column
    merge_v(5:5) %>%       # Merge p-value column
    valign(j = "Difference", valign = "top", part = "body") %>%
    align(j = 1:1, align = "left", part = "body") %>%
    align(j = 1:1, align = "left", part = "header") %>%
    hline(i = line_indices, part = "body", border = fp_border(color = "grey", width = 1) ) %>%
    bold(i = ~ `p-value` < 0.05, j = "p-value", bold = TRUE) %>%
    fix_border_issues()

  return(tbl)
}