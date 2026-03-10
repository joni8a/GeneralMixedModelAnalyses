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
  }
  return(emm)
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
