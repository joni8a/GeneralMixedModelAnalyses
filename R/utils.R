# utils.R
# Internal helper functions for the package.

# Resolve the effective transform type from a ModelContainer.
# Handles backward compatibility with the legacy log_transform boolean slot.
.resolve_transform <- function(model_container) {
  t <- model_container@transform
  if (!nzchar(t) || t == "none") {
    if (isTRUE(model_container@log_transform)) return("log")
    return("none")
  }
  t
}

# Back-transform emmeans to the original (response) scale based on the
# transform type stored in the model container:
#   "log"  — log(y + offset), uses emmeans::make.tran("genlog", offset)
#   "beta" — logit link from beta regression, uses regrid("response")
#   "none" — no transformation, returned unchanged
#
# @param emm An emmGrid object from emmeans()
# @param model_container A ModelContainer object
# @return The (possibly regridded) emmGrid object
.maybe_regrid <- function(emm, model_container) {
  transform <- .resolve_transform(model_container)

  if (transform == "log") {
    tran <- make.tran("genlog", model_container@log_offset)
    emm  <- update(emm, tran = tran)
    emm  <- regrid(emm, transform = "response")
    # regrid() renames the estimate column; restore expected name
    emm@misc$estName <- "emmean"

  } else if (transform == "beta") {
    emm <- regrid(emm, transform = "response")  # logit → [0, 1]
    emm@misc$estName <- "emmean"
  }

  return(emm)
}

# Multiply numeric estimate/CI columns in a data frame by response_scale.
# Called after converting an emmGrid to a data frame so that beta-regression
# results (on the [0,1] proportion scale) are displayed on the original
# outcome scale (e.g., ×100 for KOOS).
#
# @param df A data frame produced from an emmGrid
# @param model_container A ModelContainer object
# @return The data frame with scaled columns
.apply_response_scale <- function(df, model_container) {
  scale <- model_container@response_scale
  if (is.null(scale) || scale == 1) return(df)

  cols <- intersect(names(df), c(
    "emmean", "estimate", "response",
    "lower.CL", "upper.CL",
    "lower.CL_emm", "upper.CL_emm",
    "lower.CL_contrast", "upper.CL_contrast",
    "asymp.LCL", "asymp.UCL"
  ))
  df[cols] <- df[cols] * scale
  df
}

# Standardise column names in data frames produced from emmGrid objects.
# After regrid(), emmeans renames CI columns from lower.CL/upper.CL to
# asymp.LCL/asymp.UCL. This helper restores the standard names so the
# rest of the package can rely on them.
.standardize_emm_cols <- function(df) {
  col_map <- c(
    "asymp.LCL"          = "lower.CL",
    "asymp.UCL"          = "upper.CL",
    "response"           = "emmean",
    "asymp.LCL_emm"      = "lower.CL_emm",
    "asymp.UCL_emm"      = "upper.CL_emm",
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

# Generate a transformation note string for captions/footnotes.
# Returns an empty string when no transformation was applied.
#
# @param model_container A ModelContainer object
# @return A character string
.log_note <- function(model_container) {
  transform <- .resolve_transform(model_container)

  if (transform == "log") {
    return(paste0(
      "Note: Analysis performed on log-transformed data (log(y + ",
      model_container@log_offset,
      ")); results are back-transformed to the original scale."
    ))
  }

  if (transform == "beta") {
    return(paste0(
      "Note: Analysis performed on beta-scaled data (outcome/",
      model_container@response_scale,
      " with Smithson-Verkuilen adjustment for boundary values); ",
      "results are back-transformed to the original scale."
    ))
  }

  return("")
}
