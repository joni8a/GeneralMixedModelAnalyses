library(ggplot2)
library(emmeans)
library(dplyr)
library(RColorBrewer) # Ensure this is loaded for color palette generation

#' @export
plot_contrasts <- function(model, formula, followup, x_lab = "Follow-Up (Months)") {
  grouping_var <- deparse(formula[[2]][[3]]) # e.g. "Phase"
  at_list <- setNames(list(followup), grouping_var)
  
  # 1. Calculate EMMeans and Contrasts
  emm <- emmeans(model@lm, formula, at = at_list)
  contrast_df <- as.data.frame(contrast(emm, method = "pairwise", infer = c(TRUE, TRUE)))
  
  title <- paste0(model@predictor_variable, " Estimated Contrasts.")
  y_lab <- paste0(model@predictor_variable, " Mean Difference (95% CI)") # Improved label
  caption <- paste0(model@name, ". ", deparse(formula(model@lm)), collapse = " ")

  # 2. Setup Robust Color Mapping
  # Get all unique contrast names that actually exist in the data
  unique_contrasts <- unique(contrast_df$contrast)
  n_contrasts <- length(unique_contrasts)
  
  # Generate a palette with enough colors for the number of contrasts found
  # We use a high-quality palette (Dark2 or Set1) and extend it if needed
  if (n_contrasts <= 8) {
    palette_colors <- RColorBrewer::brewer.pal(max(3, n_contrasts), "Dark2")[1:n_contrasts]
  } else {
    # If huge number of contrasts, use a larger palette
    palette_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_contrasts)
  }
  
  # Create a NAMED vector. This matches specific colors to specific names.
  # This prevents the "swapping" issue if data sorting changes.
  contrast_colors <- setNames(palette_colors, unique_contrasts)

  # 3. Setup Plotting Parameters
  spacing <- min(diff(sort(as.double(unique(model@data[[grouping_var]])))))
  bar_width <- spacing * 0.25
  dodge_width <- spacing * 0.4

  # 4. Plot
  p <- contrast_df %>%
    ggplot(aes(x = !!sym(grouping_var), y = estimate, color = contrast, group = contrast)) +
    geom_point(position = position_dodge(width = dodge_width)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = bar_width), 
                  position = position_dodge(width = dodge_width)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    
    # --- ROBUST FIX START ---
    # We use scale_color_manual with the named vector we created above.
    # 'breaks' ensures the legend uses the exact keys we defined.
    scale_color_manual(
      values = contrast_colors,
      breaks = names(contrast_colors)
    ) + 
    # --- ROBUST FIX END ---
    
    labs(
      title = title,
      x = x_lab,
      y = y_lab,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Arial", size = 12),
      plot.caption = element_text(hjust = 0),
      legend.title = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )

  return(p)
}