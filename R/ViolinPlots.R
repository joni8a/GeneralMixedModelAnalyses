library(ggplot2)
library(dplyr)

#' @export
plot_violin_distribution <- function(model_container, formula, x_lab = "Follow-Up (Months)") {
  
  # 1. Setup variables from formula and model container
  # formula is usually ~ BearingType * Phase or similar
  grouping_var   <- deparse(formula[[2]][[3]]) # e.g., "Phase"
  factorVariable <- deparse(formula[[2]][[2]]) # e.g., "BearingType"
  y_var          <- model_container@predictor_variable
  df             <- model_container@data
  
  # 2. Define labels and titles
  title  <- paste0(y_var, " Distribution by ", factorVariable)
  y_lab  <- paste0(y_var, " (Raw Values)")
  caption <- paste0(model_container@name, ". Distribution analysis.")

  # 3. Build the Plot
  p <- df %>%
    # Ensure Phase/Time is treated as a factor for the X-axis mapping in a violin plot
    ggplot(aes(x = as.factor(!!sym(grouping_var)), 
               y = !!sym(y_var), 
               fill = !!sym(factorVariable), 
               color = !!sym(factorVariable))) +
    
    # -- VIOLIN LAYER --
    # alpha makes it transparent so we can see points/markers
    geom_violin(aes(fill = !!sym(factorVariable)), 
                position = position_dodge(width = 0.8), 
                alpha = 0.2, 
                color = "grey60") +
    
    # -- INDIVIDUAL POINTS (JITTERED) --
    geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
                alpha = 0.4, 
                size = 1) +
    
    # -- MEDIAN MARKER (Horizontal Line or Point) --
    stat_summary(fun = median, geom = "point", shape = 95, size = 10, 
                 position = position_dodge(width = 0.8), show.legend = FALSE) +
    
    # -- MEAN MARKER (Large Diamond or Point) --
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, 
                 position = position_dodge(width = 0.8), color = "black") +
    
    labs(
      title = title,
      x = x_lab,
      y = y_lab,
      caption = caption,
      subtitle = "Diamonds (◆) represent Mean | Horizontal bars represent Median"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Arial", size = 12, face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8),
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  
  # Apply custom colors and labels if group_mapping is available
  # Using 'breaks' ensures colors are mapped by name, not by factor level order
  if (!is.null(model_container@group_mapping) && !is.null(model_container@group_mapping$colors)) {
    # Get the keys (factor levels) from colors
    color_keys <- names(model_container@group_mapping$colors)
    
    # Get labels in the same order as color_keys (using named lookup)
    labels_vec <- model_container@group_mapping$labels[color_keys]
    
    p <- p +
      scale_color_manual(
        values = model_container@group_mapping$colors,
        breaks = color_keys,
        labels = labels_vec
      ) +
      scale_fill_manual(
        values = model_container@group_mapping$colors,
        breaks = color_keys,
        labels = labels_vec
      )
  }

  return(p)
}