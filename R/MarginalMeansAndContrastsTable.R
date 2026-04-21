# MarginalMeansAndContrastsTable.R

#' Create Estimated Marginal Means and Contrasts Table
#'
#' Builds the default EMM/contrast table, or an optional compact layout with
#' one row per phase and user-selected group columns.
#'
#' @param model A `ModelContainer` object.
#' @param formula An emmeans formula (for example `~ Group * Phase`).
#' @param followup Follow-up values to include for the phase/grouping axis.
#' @param optional_format Logical flag. If `FALSE` (default), returns the
#'   standard table. If `TRUE`, returns the optional 4-column layout.
#' @param group_1 First selected group for optional format. Used as the left
#'   group column and as the numerator in mean difference (`group_1 - group_2`).
#' @param group_2 Second selected group for optional format.
#'
#' @return A `flextable` object.
#'
#' @examples
#' # Standard layout (default)
#' # tbl <- table_emm_contrasts(model, ~ BearingType * Phase, c(0, 6, 12))
#'
#' # Optional compact layout
#' # tbl_opt <- table_emm_contrasts(
#' #   model = model,
#' #   formula = ~ BearingType * Phase,
#' #   followup = c(0, 6, 12),
#' #   optional_format = TRUE,
#' #   group_1 = "Continues migrators",
#' #   group_2 = "Non-continuous migrators"
#' # )
#' @export
table_emm_contrasts <- function(model,
                                formula,
                                followup,
                                optional_format = FALSE,
                                group_1 = NULL,
                                group_2 = NULL) {
  # 1. Setup variables and data
  grouping_var <- deparse(formula[[2]][[3]]) # e.g., "Phase"
  factorVariable <- deparse(formula[[2]][[2]]) # e.g., "BearingType"
  
  at_list <- setNames(list(followup), grouping_var)
  
  # 2. Get EMMeans and Contrasts
  emm <- emmeans(model@lm, formula, at = at_list)

  # Back-transform to original scale if the model was fit on log-transformed data
  emm <- .maybe_regrid(emm, model)

  emm_df <- as.data.frame(emm)
  emm_df <- .standardize_emm_cols(emm_df)
  contrast_df <- as.data.frame(contrast(emm, method = "pairwise", infer = c(TRUE, TRUE)))
  contrast_df <- .standardize_emm_cols(contrast_df)

  # 3. Merge them into one dataframe
  # We print grouping_var just for debugging visibility in console
  print(paste("Grouping by:", grouping_var)) 
  df <- merge(emm_df, contrast_df, by = grouping_var, suffix = c("_emm", "_contrast"))

  caption <- paste0(model@predictor_variable, ", Marginal Means (95% CI) and Contrasts (95% CI).")
  log_note <- .log_note(model)
  footnote <- paste0(model@name, ". ", deparse(formula(model@lm)),
                     if (nzchar(log_note)) paste0(" ", log_note),
                     collapse = " ")

  if (isTRUE(optional_format)) {
    available_groups <- unique(as.character(model@data[[factorVariable]]))

    if (is.null(group_1) || is.null(group_2)) {
      stop("For optional_format = TRUE, both group_1 and group_2 must be provided.")
    }

    resolve_group <- function(x, available, group_mapping) {
      x_chr <- as.character(x)
      if (x_chr %in% available) {
        return(x_chr)
      }

      if (!is.null(group_mapping) && !is.null(group_mapping$labels)) {
        labels_vec <- group_mapping$labels
        if (is.null(names(labels_vec)) && !is.null(group_mapping$colors)) {
          names(labels_vec) <- names(group_mapping$colors)
        }
        if (!is.null(names(labels_vec))) {
          hit <- names(labels_vec)[labels_vec == x_chr]
          if (length(hit) > 0) {
            return(hit[[1]])
          }
        }
      }

      stop(
        "Group '", x_chr, "' not found. Available data groups: ",
        paste(available, collapse = ", "),
        "."
      )
    }

    group_1_raw <- resolve_group(group_1, available_groups, model@group_mapping)
    group_2_raw <- resolve_group(group_2, available_groups, model@group_mapping)

    if (identical(group_1_raw, group_2_raw)) {
      stop("group_1 and group_2 must be different groups.")
    }

    labels_vec <- NULL
    if (!is.null(model@group_mapping) && !is.null(model@group_mapping$labels)) {
      labels_vec <- model@group_mapping$labels
      if (is.null(names(labels_vec)) && !is.null(model@group_mapping$colors)) {
        names(labels_vec) <- names(model@group_mapping$colors)
      }
    }

    group_1_label <- if (!is.null(labels_vec) && group_1_raw %in% names(labels_vec)) {
      labels_vec[[group_1_raw]]
    } else {
      group_1_raw
    }
    group_2_label <- if (!is.null(labels_vec) && group_2_raw %in% names(labels_vec)) {
      labels_vec[[group_2_raw]]
    } else {
      group_2_raw
    }

    emm_small <- emm_df %>%
      select(!!sym(grouping_var), !!sym(factorVariable), emmean, lower.CL, upper.CL) %>%
      mutate(
        group_raw = as.character(!!sym(factorVariable)),
        mean_ci = paste0(
          round(emmean, 3), " (",
          round(lower.CL, 3), " - ",
          round(upper.CL, 3), ")"
        )
      )

    g1_df <- emm_small %>%
      filter(group_raw == group_1_raw) %>%
      select(!!sym(grouping_var), mean_ci)
    g2_df <- emm_small %>%
      filter(group_raw == group_2_raw) %>%
      select(!!sym(grouping_var), mean_ci)

    group_1_col <- paste0(group_1_label, " (Mean, 95% CI)")
    group_2_col <- paste0(group_2_label, " (Mean, 95% CI)")
    names(g1_df)[2] <- group_1_col
    names(g2_df)[2] <- group_2_col

    if (!"contrast" %in% names(contrast_df)) {
      stop("Expected a 'contrast' column in pairwise contrast output.")
    }

    # Normalize a contrast label for robust matching:
    # strip parentheses (emmeans wraps names containing hyphens in parens),
    # collapse multiple spaces to one, and trim edges.
    .norm <- function(x) trimws(gsub("\\s+", " ", gsub("[()]", "", x)))

    contrast_key_1 <- .norm(paste0(group_1_raw, " - ", group_2_raw))
    contrast_key_2 <- .norm(paste0(group_2_raw, " - ", group_1_raw))

    # Print actual contrast labels from emmeans to aid diagnosis if matching fails.
    message("Available contrast labels: ",
            paste(unique(.norm(as.character(contrast_df$contrast))), collapse = " | "))

    diff_df <- contrast_df %>%
      mutate(
        contrast_norm = .norm(as.character(contrast)),
        sign_flip     = contrast_norm == contrast_key_2,
        estimate_adj  = if_else(sign_flip, -estimate, estimate),
        lower_adj     = if_else(sign_flip, -upper.CL, lower.CL),
        upper_adj     = if_else(sign_flip, -lower.CL, upper.CL)
      ) %>%
      filter(contrast_norm %in% c(contrast_key_1, contrast_key_2)) %>%
      mutate(
        `Mean Difference (95% CI)` = paste0(
          round(estimate_adj, 3), " (",
          round(lower_adj, 3), " - ",
          round(upper_adj, 3), ")"
        )
      ) %>%
      group_by(!!sym(grouping_var)) %>%
      slice(1) %>%
      ungroup() %>%
      select(!!sym(grouping_var), `Mean Difference (95% CI)`)

    if (nrow(diff_df) == 0) {
      stop(
        "Could not find pairwise contrasts for '", group_1_raw, "' and '", group_2_raw, "'.\n",
        "Expected one of:\n  '", contrast_key_1, "'\n  '", contrast_key_2, "'\n",
        "Actual contrast labels from emmeans:\n  ",
        paste(unique(.norm(as.character(contrast_df$contrast))), collapse = "\n  ")
      )
    }

    phase_df <- data.frame(phase_value = followup)
    names(phase_df) <- grouping_var

    tbl_opt <- phase_df %>%
      left_join(g1_df, by = grouping_var) %>%
      left_join(g2_df, by = grouping_var) %>%
      left_join(diff_df, by = grouping_var) %>%
      rename(Phase = !!sym(grouping_var))

    ft_opt <- tbl_opt %>%
      flextable() %>%
      flextable::font(fontname = "Arial") %>%
      fontsize(size = 10, part = "body") %>%
      set_caption(as_paragraph(as_b(caption)), align_with_table = FALSE) %>%
      add_footer_row(value = as_paragraph(footnote), colwidth = c(4)) %>%
      fontsize(part = "footer", size = 8) %>%
      width(j = "Phase", width = 1.0) %>%
      width(j = group_1_col, width = 2.0) %>%
      width(j = group_2_col, width = 2.0) %>%
      width(j = "Mean Difference (95% CI)", width = 2.0) %>%
      align(j = "Phase", align = "left", part = "all") %>%
      align(j = c(group_1_col, group_2_col, "Mean Difference (95% CI)"), align = "center", part = "all") %>%
      fix_border_issues()

    return(ft_opt)
  }

  # 4. Calculate formatting indices for the table
  line_indices <- df %>%
    group_by(!!sym(grouping_var)) %>%
    summarise(last_row = n()) %>%
    mutate(cum_row = cumsum(last_row)) %>%
    pull(cum_row)

  # 6. Build the Table
  tbl <- df %>%
    select(!!sym(grouping_var), !!sym(factorVariable),
           emmean, lower.CL_emm, upper.CL_emm,
           estimate, lower.CL_contrast, upper.CL_contrast,
           p.value) %>%
    # Round numerics
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    # Handle p-value display logic (show only on first row of group)
    group_by(!!sym(grouping_var)) %>%
    mutate(
      p.value = ifelse(row_number() == 1, p.value, NA)
    ) %>%
    ungroup() %>%
    # Sort
    group_by(!!sym(grouping_var), !!sym(factorVariable)) %>%
    arrange(!!sym(grouping_var)) %>%
    ungroup() %>%
    # Format text columns
    mutate(
      Mean = paste0(emmean, " (", lower.CL_emm, " - ", upper.CL_emm, ")"),
      Difference = paste0(estimate, " (", lower.CL_contrast, " - ", upper.CL_contrast, ")"),
    ) %>%
    select(
      !!sym(grouping_var), !!sym(factorVariable), Mean, Difference, p.value
    ) %>%
    rename(
      Group = !!sym(factorVariable),
      Phase = !!sym(grouping_var),
      `p-value` = p.value
    )
  
  # Apply group labels if group_mapping is available
  # Use named lookup instead of positional mapping to avoid label swapping
  if (!is.null(model@group_mapping) && !is.null(model@group_mapping$labels)) {
    # Create a named vector for lookup: original_level -> display_label
    # If labels are named (e.g., c("A" = "Label A")), use names as keys
    # If labels are unnamed, assume they match the order of names(colors)
    labels_vec <- model@group_mapping$labels
    if (is.null(names(labels_vec))) {
      # Unnamed labels: use names from colors as keys
      names(labels_vec) <- names(model@group_mapping$colors)
    }
    
    tbl <- tbl %>%
      mutate(
        Group = labels_vec[as.character(Group)]
      )
  }
  
  tbl <- tbl %>%
    flextable() %>%
    
    # --- NAMESPACE FIX HERE ---
    flextable::font(fontname = "Arial") %>%
    
    fontsize(size = 10, part = "body") %>%
    set_caption(as_paragraph(as_b(caption)), align_with_table = FALSE) %>%
    add_footer_row(value = as_paragraph(footnote), colwidth = c(5)) %>%
    fontsize(part = "footer", size = 8)  %>%
    width(1:1, 1.0) %>%
    width(3:4, 1.9) %>%
    merge_v(1:1) %>%       # Merge Phase column
    merge_v(j = "Group") %>% # Merge Group column
    merge_v(4:4) %>%       # Merge Difference column
    merge_v(5:5) %>%       # Merge p-value column
    valign(j = "Difference", valign = "top", part = "body") %>%
    align(j = 1:1, align = "left", part = "body") %>%
    align(j = 1:1, align = "left", part = "header") %>%
    hline(i = line_indices, part = "body", border = fp_border(color = "grey", width = 1) ) %>%
    bold(i = ~ `p-value` < 0.05, j = "p-value", bold = TRUE) %>%
    fix_border_issues()

  return(tbl)
}