# imports.R
# Central file for package-level imports.
# This ensures all dependencies are properly declared in the NAMESPACE
# instead of using library() or require() calls in individual files.

#' @import dplyr
#' @import ggplot2
#' @import emmeans
#' @import flextable
#' @import officer
#' @importFrom ggrepel geom_text_repel
#' @importFrom grid textGrob gpar unit unit.c grid.draw
#' @importFrom gridExtra arrangeGrob
#' @importFrom RColorBrewer brewer.pal
#' @importFrom broom.mixed tidy
#' @importFrom methods new setClass
#' @importFrom rlang sym .data :=
#' @importFrom stats residuals fitted qqnorm formula nobs AIC BIC logLik cor setNames
#' @importFrom grDevices png dev.off colorRampPalette
#' @importFrom lme4 VarCorr getME
NULL
