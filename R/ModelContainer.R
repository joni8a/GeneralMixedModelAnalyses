#' @export
ModelContainer <- setClass(
  "ModelContainer",
  slots = c(
    name = "character",
    predictor_variable = "character",
    lm = "ANY",
    data = "data.frame",
    group = "character",
    group_mapping = "list"
  )
)

#' @export
NewModelContainer <- function(name,
                              predictor_variable,
                              lm,
                              data,
                              group,
                              group_mapping) {
  

  # Validate group_mapping structure
  if (!is.null(group_mapping)) {
    # Check that colors is named
    if (!is.null(group_mapping$colors) && is.null(names(group_mapping$colors))) {
      stop("group_mapping$colors must be a named vector (e.g., c('GroupA' = 'red', 'GroupB' = 'blue'))")
    }
    
    # Check that labels is named
    if (!is.null(group_mapping$labels) && is.null(names(group_mapping$labels))) {
      stop("group_mapping$labels must be a named vector (e.g., c('GroupA' = 'Display Label A', 'GroupB' = 'Display Label B')). ",
           "The names should match the factor levels in your data.")
    }
    
    # Check that labels and colors have the same keys
    if (!is.null(group_mapping$labels) && !is.null(group_mapping$colors)) {
      labels_keys <- names(group_mapping$labels)
      colors_keys <- names(group_mapping$colors)
      if (!setequal(labels_keys, colors_keys)) {
        stop("group_mapping$labels and group_mapping$colors must have the same names. ",
             "Labels keys: ", paste(labels_keys, collapse = ", "), ". ",
             "Colors keys: ", paste(colors_keys, collapse = ", "), ".")
      }
    }
    
    # Check that keys match factor levels in data
    if (!is.null(group_mapping$colors) && !is.null(group) && group != "") {
      factor_levels <- levels(as.factor(data[[group]]))
      mapping_keys <- names(group_mapping$colors)
      missing_keys <- setdiff(factor_levels, mapping_keys)
      if (length(missing_keys) > 0) {
        warning("Some factor levels in '", group, "' are not in group_mapping: ", 
                paste(missing_keys, collapse = ", "), ". These will use default colors/labels.")
      }
    }
  }
  
  new("ModelContainer",
      name = name,
      predictor_variable = predictor_variable,
      lm = lm,
      data = data,
      group = group,
      group_mapping = group_mapping)
}
