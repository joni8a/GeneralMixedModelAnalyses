#' @export
table_model_summary <- function(model, digits = 3) {
  require(flextable)
  require(broom.mixed)
  require(dplyr)

  # 1️⃣ Model info ----
  model_formula <- deparse(formula(model))
  method <- ifelse("lmerMod" %in% class(model) && lme4::getME(model, "REML"),
                   "REML", "ML")
  n_obs <- nobs(model)
  fit_indices <- data.frame(
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    Observations = n_obs
  )

  model_info_tbl <- data.frame(
    Formula = model_formula,
    Estimation = method,
    Observations = n_obs
  )

  ft_model_info <- flextable(model_info_tbl) |>
    set_caption("Model Information") |>
    autofit() %>%
    fix_border_issues()

  # 2️⃣ Fixed effects ----
  fixed <- broom.mixed::tidy(model, effects = "fixed")
  fixed <- fixed |>
    mutate(across(where(is.numeric), round, digits = digits)) |>
    rename(
      Term = term,
      Estimate = estimate,
      SE = std.error,
      Statistic = statistic,
      `p-value` = p.value
    )

  ft_fixed <- flextable(fixed) |>
    set_caption("Fixed Effects") |>
    autofit() %>%
    fix_border_issues()

  # 3️⃣ Random effects ----
  random <- as.data.frame(VarCorr(model))
  random <- random |>
    mutate(across(where(is.numeric), round, digits = digits)) |>
    rename(
      `Grouping Factor` = grp,
      `Random Effect` = var1,
      Variance = vcov,
      SD = sdcor
    ) |>
    select(`Grouping Factor`, `Random Effect`, Variance, SD)

  ft_random <- flextable(random) |>
    set_caption("Random Effects") |>
    autofit() %>%
    fix_border_issues()

  # 4️⃣ Fit indices ----
  ft_fit <- flextable(fit_indices) |>
    set_caption("Model Fit Indices") |>
    autofit() %>%
    fix_border_issues()

  # ✅ Return a named list of flextables
  return(list(
    model_info = ft_model_info,
    fixed_effects = ft_fixed,
    random_effects = ft_random,
    fit_indices = ft_fit
  ))
}
