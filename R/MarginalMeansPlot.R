library(dplyr)
library(ggplot2)
library(emmeans)
library(ggrepel)

#' @export
plot_marginal_means <- function(model, formula, followup, 
                                x_lab = "Follow-Up (Months)", 
                                append_zero = FALSE, 
                                plot_trajectories = FALSE, 
                                n_similar = 0, 
                                similarity_metric = "euclidean",
                                label_ids = FALSE,
                                return_ids = FALSE) { 
  
  # 1. Setup variables
  grouping_var <- deparse(formula[[2]][[3]])   
  factorVariable <- deparse(formula[[2]][[2]]) 
  outcome_var <- model@predictor_variable      
  
  at_list <- setNames(list(followup), grouping_var)
  emm <- emmeans(model@lm, formula, at = at_list)
  emm_df <- as.data.frame(emm)

  title <- paste0(outcome_var, " Estimated Marginal Means")
  y_lab <- paste0(outcome_var, " Mean (95% CI)")
  caption <- paste0(model@name, ". ", deparse(formula(model@lm)), collapse = " ")

  if(append_zero == TRUE) {
    new_rows <- data.frame(
      placeholder_group = levels(model@data[[factorVariable]]),
      placeholder_var = 0,
      emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0
    )
    names(new_rows)[names(new_rows) == "placeholder_group"] <- factorVariable
    names(new_rows)[names(new_rows) == "placeholder_var"] <- grouping_var
    new_rows <- new_rows[, c(factorVariable, grouping_var, "emmean", "SE", "df", "lower.CL", "upper.CL")]
    emm_df <- rbind(new_rows, emm_df)
  }

  traj_external <- NULL
  traj_similar <- NULL
  
  if (is.data.frame(plot_trajectories)) {
    traj_external <- plot_trajectories
    traj_external[[grouping_var]] <- as.numeric(as.character(traj_external[[grouping_var]]))
    
    if (n_similar > 0) {
      ref_pattern <- traj_external %>%
        group_by(!!sym(grouping_var)) %>%
        summarise(Ref_Value = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")
      
      candidates <- model@data
      candidates[[grouping_var]] <- as.numeric(as.character(candidates[[grouping_var]]))
      all_ids <- unique(candidates$ID)
      scores <- data.frame(ID = all_ids, Score = NA)
      
      for (i in seq_along(all_ids)) {
        pid <- all_ids[i]
        p_data <- candidates[candidates$ID == pid, ]
        joined <- inner_join(p_data, ref_pattern, by = grouping_var)
        if (nrow(joined) >= 2) {
          if (similarity_metric == "euclidean") {
            diffs <- joined[[outcome_var]] - joined$Ref_Value
            scores$Score[i] <- sqrt(mean(diffs^2))
          } else if (similarity_metric == "correlation") {
            cor_val <- suppressWarnings(cor(joined[[outcome_var]], joined$Ref_Value))
            if(!is.na(cor_val)) scores$Score[i] <- cor_val
          }
        }
      }
      if (similarity_metric == "euclidean") {
        top_ids <- scores %>% arrange(Score) %>% head(n_similar) %>% pull(ID)
      } else {
        top_ids <- scores %>% arrange(desc(Score)) %>% head(n_similar) %>% pull(ID)
      }
      traj_similar <- candidates %>% filter(ID %in% top_ids)
    }
    
  } else if (isTRUE(plot_trajectories)) {
    all_ids <- unique(model@data$ID)
    selected_ids <- if (length(all_ids) > 10) sample(all_ids, 10) else all_ids
    traj_similar <- model@data %>% dplyr::filter(ID %in% selected_ids)
    traj_similar[[grouping_var]] <- as.numeric(as.character(traj_similar[[grouping_var]]))
    
  } else if (!is.logical(plot_trajectories) && !is.null(plot_trajectories)) {
    traj_similar <- model@data %>% dplyr::filter(ID %in% plot_trajectories)
    traj_similar[[grouping_var]] <- as.numeric(as.character(traj_similar[[grouping_var]]))
  }

  # 5. Plotting Setup
  spacing <- min(diff(sort(unique(as.double(model@data[[grouping_var]])))))
  bar_width <- spacing * 0.25
  dodge_width <- spacing * 0.4

  p <- ggplot() 
  
  if (!is.null(traj_external)) {
    p <- p +
      geom_line(data = traj_external,
                aes(x = !!sym(grouping_var), 
                    y = !!sym(outcome_var), 
                    group = ID), 
                color = "grey60", alpha = 0.4, linewidth = 0.3)
  }

  if (!is.null(traj_similar)) {
    p <- p +
      geom_line(data = traj_similar,
                aes(x = !!sym(grouping_var), 
                    y = !!sym(outcome_var), 
                    group = ID, 
                    color = as.factor(!!sym(factorVariable))), 
                alpha = 0.5, linewidth = 0.4, show.legend = FALSE)
    
    if (label_ids) {
      label_data <- traj_similar %>%
        group_by(ID) %>%
        filter(!!sym(grouping_var) == max(!!sym(grouping_var))) %>%
        ungroup()
      
      p <- p + geom_text_repel(data = label_data,
                               aes(x = !!sym(grouping_var), 
                                   y = !!sym(outcome_var), 
                                   label = ID,
                                   color = as.factor(!!sym(factorVariable))),
                               size = 3, fontface = "bold", show.legend = FALSE,
                               direction = "y", nudge_x = 0.2)
    }
  }

  p <- p +
    geom_point(data = emm_df, 
               aes(x = !!sym(grouping_var), y = emmean, 
                   group = as.factor(!!sym(factorVariable)), 
                   color = as.factor(!!sym(factorVariable))),
               position = position_dodge(width = dodge_width)) +
    geom_errorbar(data = emm_df, 
                  aes(x = !!sym(grouping_var), ymin = lower.CL, ymax = upper.CL, 
                      color = as.factor(!!sym(factorVariable)), width = bar_width), 
                  position = position_dodge(width = dodge_width)) +
    geom_line(data = emm_df, 
              aes(x = !!sym(grouping_var), y = emmean, 
                  group = as.factor(!!sym(factorVariable)), 
                  color = as.factor(!!sym(factorVariable))), 
              position = position_dodge(width = dodge_width), linewidth = 1)
  
  # Apply custom colors and labels if group_mapping is available
  if (!is.null(model@group_mapping) && !is.null(model@group_mapping$colors)) {
    # Get the keys (factor levels) from colors
    color_keys <- names(model@group_mapping$colors)
    
    # Get labels in the same order as color_keys (using named lookup)
    labels_vec <- model@group_mapping$labels[color_keys]
    
    p <- p + scale_color_manual(
      values = model@group_mapping$colors,
      breaks = color_keys,
      labels = labels_vec 
    )
  }

  p <- p +
    scale_x_continuous(breaks = unique(followup)) +
    labs(title = title, x = x_lab, y = y_lab, caption = caption) +
    theme_minimal() +
    theme(plot.title = element_text(family = "Arial", size = 12),
          plot.caption = element_text(hjust = 0),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA))

  # --- OUTPUT LOGIC ---
  
  selected_ids <- if (!is.null(traj_similar)) unique(traj_similar$ID) else NULL
  
  if (!is.null(selected_ids)) {
    message("IDs plotted: ", paste(selected_ids, collapse = ", "))
  }

  if (return_ids) {
    return(list(plot = p, ids = selected_ids))
  }

  return(p)
}