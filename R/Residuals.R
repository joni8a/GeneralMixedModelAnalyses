library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)

residuals_qq_plot <- function(model) {
  model@data$residuals <- residuals(model@lm)
  model@data$id <- seq_len(nrow(model@data))
  title <- paste0(model@predictor_variable, ". ", "Residuals QQ Plot.")

  # Compute QQ data manually to preserve IDs
  qq <- qqnorm(model@data$residuals, plot.it = FALSE)
  qq_data <- data.frame(
    theoretical = qq$x,
    sample = qq$y,
    id = model@data$id
  )

  p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "gray") +
    geom_text_repel(
      aes(label = id),
      size = 2.5,
      box.padding = 0.25,
      min.segment.length = 0,
      max.overlaps = 20
    ) +
    theme_minimal() +
    labs(
      title = title,
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme(
      plot.title = element_text(family = "Arial", size = 10),
      axis.title.x = element_text(family = "Arial", size = 10),
      axis.title.y = element_text(family = "Arial", size = 10),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )

  return(p)
}

residuals_vs_fitted_plot <- function(model) {
  model@data$residuals <- residuals(model@lm)
  model@data$fitted <- fitted(model@lm)
  model@data$id <- seq_len(nrow(model@data))
  title <- paste0(model@predictor_variable, ". ", "Residuals VS Fitted.")

  p <- model@data %>%
    ggplot(aes(x = fitted, y = residuals, label = id)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#E69F00") +
    geom_text_repel(
      size = 2.5,
      box.padding = 0.25,
      min.segment.length = 0,
      max.overlaps = 20
    ) +
    theme_minimal() +
    labs(title = title) +
    theme(
      plot.title = element_text(family = "Arial", size = 10),
      axis.title.x = element_text(family = "Arial", size = 10),
      axis.title.y = element_text(family = "Arial", size = 10),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )

  return(p)
}

#' @export
analyse_residuals <- function(model) {
  res_qq_plot <- residuals_qq_plot(model)
  res_vs_fitted_plot <- residuals_vs_fitted_plot(model)

  combined <- arrangeGrob(res_qq_plot,
                          res_vs_fitted_plot,
                          ncol = 2)

  title <- textGrob(paste0(model@predictor_variable, ". Residual Analyses for ", model@name, "."),
                    just = "left",
                    x = 0.06,
                    gp = gpar(fontsize = 12, fontfamily = "Arial")
                    )

  caption <- textGrob(
    paste0(model@name, ". ", deparse(formula(model@lm)), collapse = " "),
    just = "left",
    x = 0.06,
    gp = gpar(fontsize = 10, fontfamily = "Arial")
  )

  full_plot <- arrangeGrob(
    title,
    combined,
    caption,
    ncol = 1,
    heights = unit.c(unit(1, "lines"), unit(1, "null"), unit(0.8, "lines"))
  )
  return(full_plot)
}
