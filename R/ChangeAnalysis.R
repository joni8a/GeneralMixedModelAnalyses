# ChangeAnalysis.R
# This script provides functions to calculate and visualize 
# "Differences of Differences" for dynamic RSA models.

library(emmeans)
library(dplyr)
library(flextable)
library(officer)

#' @export
table_change_analysis <- function(model_container, formula, followup, intervals) {
  
  # 1. Setup variables
  # We extract the names from the formula to build the emmeans call
  grouping_var   <- deparse(formula[[2]][[3]]) # e.g., "Phase"
  factorVariable <- deparse(formula[[2]][[2]]) # e.g., "BearingType"
  model <- model_container@lm
  
  # 2. Generate Custom Contrasts
  custom_contrasts <- list()
  
  for (val in intervals) {
    start <- val[1]
    end   <- val[2]
    
    vec <- numeric(length(followup))
    vec[which(followup == end)]   <- 1
    vec[which(followup == start)] <- -1
    
    name <- paste0("Phase ", end, " - Phase ", start)
    custom_contrasts[[name]] <- vec
  }
  
  # 3. Calculate Emmeans & Interaction
  at_list <- setNames(list(followup), grouping_var)
  emm <- emmeans(model, formula, at = at_list)
  
  interaction_specs <- list()
  interaction_specs[[grouping_var]]   <- custom_contrasts
  interaction_specs[[factorVariable]] <- "pairwise"
  
  # This generates the "Difference of Differences"
  diff_of_difs <- contrast(emm, interaction = interaction_specs)
  df_res <- summary(diff_of_difs, infer = c(TRUE, TRUE)) %>% as_tibble()
  
  # 4. Prepare Data (Positional Fix Applied)
  tbl_data <- df_res %>%
    as_tibble() %>% 
    
    # RENAME BY POSITION: emmeans often renames columns in interactions 
    # (e.g., "Phase_custom"). Standardizing them here ensures select() won't fail.
    rename(
      !!grouping_var := 1,
      !!factorVariable := 2
    ) %>%
    
    # Round numeric columns for cleaner text generation
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  
    # Format the "Difference of Differences" column and p-value
    mutate(
      `Diff of Diffs` = paste0(estimate, " (", lower.CL, " - ", upper.CL, ")"),
      p_fmt = if_else(p.value < 0.001, "< 0.001", as.character(p.value))
    ) %>%
  
    # Organize columns (These names are now guaranteed to exist due to rename step)
    select(
      !!sym(grouping_var),          
      !!sym(factorVariable),    
      `Diff of Diffs`, 
      p.value,        
      p_fmt           
    ) %>%
    arrange(!!sym(grouping_var))    

  # 5. Create Flextable
  ft <- tbl_data %>%
    flextable() %>%
    
    # -- A. FONTS & SIZING --
    # FIX: Explicitly namespace these to avoid conflict with ggpubr::font or others
    flextable::font(fontname = "Arial", part = "all") %>%
    flextable::fontsize(size = 10, part = "body") %>%
    flextable::fontsize(size = 11, part = "header") %>%
    
    # Adjust column widths
    width(j = 1, width = 1.5) %>% 
    width(j = 2, width = 1.5) %>% 
    width(j = 3, width = 2.0) %>% 
  
    # -- B. MERGING --
    # Merges repeating Phase labels for a cleaner look
    merge_v(j = grouping_var) %>% 
  
    # -- C. BORDERS & ALIGNMENT --
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
    hline_bottom(part = "body", border = fp_border(color = "black", width = 1.5)) %>%
    hline(border = fp_border(color = "grey", width = 0.5)) %>%
  
    valign(valign = "top", part = "body") %>%
    align(align = "center", part = "header") %>%
    align(j = 3:5, align = "center", part = "body") %>% 
  
    # -- D. CONDITIONAL FORMATTING --
    bold(i = ~ p.value < 0.05, j = "p_fmt", bold = TRUE) %>%
    bold(i = ~ p.value < 0.05, j = "Diff of Diffs", bold = TRUE) %>%
  
    # -- E. LABELS --
    set_header_labels(
      p_fmt = "p-value"
    ) %>%
  
    # -- F. CLEANUP --
    delete_part(part = "footer") %>% 
    colformat_double(j = "p.value", digits = 3) 
  
  # Final Step: Hide the raw p.value column (used for bolding logic) from the final output
  ft <- ft %>% void(j = "p.value", part = "all")
  
  return(ft)
}