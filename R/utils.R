# utils.R
# Internal helper functions for the package.

# Back-transform emmeans to the original (response) scale when
# the model was fit on log-transformed data.
# Uses emmeans::make.tran("genlog", offset) which handles log(y + offset)
# and emmeans::regrid() to convert means and contrasts to the original scale.
#
# @param emm An emmGrid object from emmeans()
# @param model_container A ModelContainer object
# @return The (possibly regridded) emmGrid object
.maybe_regrid <- function(emm, model_container) {
  if (model_container@log_transform) {
    tran <- make.tran("genlog", model_container@log_offset)
    emm <- update(emm, tran = tran)
    emm <- regrid(emm, transform = "response")
    # regrid() changes the estimate column name from "emmean" to "response".
    # Restore it so downstream code (plots, tables) can reference "emmean".
    emm@misc$estName <- "emmean"
  }
  return(emm)
}

# Standardise column names in data frames produced from emmGrid objects.
# After regrid(), df becomes Inf and emmeans renames CI columns from
# lower.CL / upper.CL to asymp.LCL / asymp.UCL.  This helper restores
# the standard names so the rest of the package can rely on them.
.standardize_emm_cols <- function(df) {
  col_map <- c(
    "asymp.LCL"     = "lower.CL",
    "asymp.UCL"     = "upper.CL",
    "response"       = "emmean",          # safety net
    "asymp.LCL_emm" = "lower.CL_emm",    # after merge with suffix
    "asymp.UCL_emm" = "upper.CL_emm",
    "asymp.LCL_contrast" = "lower.CL_contrast",
    "asymp.UCL_contrast" = "upper.CL_contrast"
  )
  for (old_name in names(col_map)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- col_map[[old_name]]
    }
  }
  df
}

# Generate a log-transform note string for captions/footnotes.
# Returns an empty string if log_transform is FALSE.
#
# @param model_container A ModelContainer object
# @return A character string
.log_note <- function(model_container) {
  if (model_container@log_transform) {
    return(paste0(
      "Note: Analysis performed on log-transformed data (log(y + ",
      model_container@log_offset,
      ")); results are back-transformed to the original scale."
    ))
  }
  return("")
}
