library(ggplot2)
library(grid)
library(officer)
library(flextable)
library(dplyr)
library(emmeans)

#' @export
report_to_word <- function(title,
                           path,
                           model,
                           fn_residual_analyses,
                           fn_emm_contrast_tbl,
                           fn_plot_marginal_means,
                           fn_plot_contrasts,
                           fn_table_model_summary,
                           fn_table_change,
                           fn_violin_chart,
                           emm_formula = NULL,
                           followup = NULL,
                           include_raw_emmeans = FALSE
                           ) {

  # 1. Save Images to Disk --------------------------------------------------

  ## Estimated marginal means (ggplot object)
  grob_path_emm = paste0(path, "/", "emm_", title, ".png")
  ggsave(
    filename = grob_path_emm,
    plot = fn_plot_marginal_means,
    width = 2000 / 300,
    height = 1200 / 300,
    dpi = 300,
    bg = "transparent"
  )

  ## Residual analyses (grid object)
  grob_path_residual = paste0(path, "/", "residual_analyses_", title, ".png")
  png(grob_path_residual, width = 2000, height = 1200, res = 300, bg = "transparent")
  grid.draw(fn_residual_analyses)
  dev.off()

  ## Contrasts (grid object)
  grob_path_contrasts = paste0(path, "/", "contrasts_", title, ".png")
  png(grob_path_contrasts, width = 2000, height = 1200, res = 300, bg = "transparent")
  grid.draw(fn_plot_contrasts)
  dev.off()
  
  ## Change Analysis Plot (ggplot object) - NEW
  grob_path_violin = paste0(path, "/", "change_analysis_", title, ".png")
  if (!is.null(fn_violin_chart)) {
    ggsave(
      filename = grob_path_violin,
      plot = fn_violin_chart,
      width = 2000 / 300,
      height = 1200 / 300,
      dpi = 300,
      bg = "transparent"
    )
  }

  # 2. Build Word Document --------------------------------------------------
  
  doc <- read_docx() %>%
    # -- Residuals --
    body_add_par("Residual Analyses", style = "heading 1") %>%
    body_add_img(grob_path_residual, width = 6, height = 3.6) %>%
    body_add_break()

  doc <- doc %>%
    # -- Marginal Means Plot --
    body_add_par("Estimated Marginal Means", style = "heading 1") %>%
    body_add_img(grob_path_emm, width = 6, height = 3.6) %>%
    body_add_break()

  doc <- doc %>%
    # -- Violin Plot --
    body_add_par("Violin Chart", style = "heading 1") %>%
    body_add_img(grob_path_violin, width = 6, height = 3.6) %>%
    body_add_break()

  doc <- doc %>%
    # -- Contrasts Plot --
    body_add_par("Contrasts", style = "heading 1") %>%
    body_add_img(grob_path_contrasts, width = 6, height = 3.6) %>%
    body_add_break()
  
  # -- Change Analysis (New Section) --
  if (!is.null(fn_table_change)) {
    doc <- doc %>%
      body_add_par("Change Analysis (Difference in Change)", style = "heading 1") %>%
      body_add_flextable(fn_table_change) %>%
      body_add_break()
  }

  # -- EMM & Contrast Table --
  doc <- doc %>%
    body_add_par("Estimated Marginal Mean And Contrasts", style = "heading 1") %>%
    body_add_flextable(fn_emm_contrast_tbl) %>%
    body_add_break()

  # -- Model Summary --
  doc <- doc %>%
    body_add_par("Model Summary", style = "heading 1")

  for(tbl in fn_table_model_summary) {
    doc <- doc %>%
      body_add_flextable(tbl) %>%
      body_add_break()
  }

  # -- Group Summary --
  table_groups <- model@data %>%
    group_by(!!sym(model@group)) %>%
    summarize(n = n_distinct(ID)) %>%
    flextable() %>%
    autofit()
    
  doc <- doc %>%
    body_add_par("Model Groups", style = "heading 1")
  doc <- doc %>%
    body_add_flextable(table_groups) %>%
    body_add_break()

  # -- Raw Emmeans Output (for verification) --
  # -- Raw Emmeans Output (for verification) --
  if (include_raw_emmeans && !is.null(emm_formula) && !is.null(followup)) {
    # Extract grouping variable from formula for the 'at' list
    grouping_var <- deparse(emm_formula[[2]][[3]])
    at_list <- setNames(list(followup), grouping_var)
    
    # Calculate emmeans and contrasts internally
    raw_emm <- emmeans(model@lm, emm_formula, at = at_list)
    raw_contrasts <- contrast(raw_emm, method = "pairwise", infer = c(TRUE, TRUE))
    
    doc <- doc %>%
      body_add_par("Raw Emmeans Output (Verification)", style = "heading 1") %>%
      body_add_par("The following tables show the unprocessed emmeans output for verification purposes.", 
                   style = "Normal") %>%
      body_add_break()
    
    # Raw Marginal Means
    raw_emm_tbl <- as.data.frame(raw_emm) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      flextable() %>%
      set_caption("Raw Estimated Marginal Means") %>%
      flextable::font(fontname = "Arial", part = "all") %>%
      fontsize(size = 9, part = "all") %>%
      autofit()
    
    doc <- doc %>%
      body_add_flextable(raw_emm_tbl) %>%
      body_add_break()
    
    # Raw Contrasts
    raw_contrast_tbl <- as.data.frame(raw_contrasts) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      flextable() %>%
      set_caption("Raw Pairwise Contrasts") %>%
      flextable::font(fontname = "Arial", part = "all") %>%
      fontsize(size = 9, part = "all") %>%
      autofit()
    
    doc <- doc %>%
      body_add_flextable(raw_contrast_tbl) %>%
      body_add_break()
  }

  # 3. Save Document --------------------------------------------------------
  print(doc, target = paste0(path, "/", title, ".docx"))

}