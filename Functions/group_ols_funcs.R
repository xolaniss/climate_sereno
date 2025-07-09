ols_model <- function(data,
                      formula,
                      vars, 
                      group_var = variable, 
                      country_var,
                      country_name){
  # Nest the data
  data |>
    dplyr::select(Date, all_of( {{ vars }})) |>
    pivot_longer(cols = -c(Date, {{ country_var }}), 
                 names_to = "variable", 
                 values_to = "value") |> 
    group_by({{ group_var }}) |> 
    nest() |> 
    # Fit the model
    mutate(models = map(data, ~coeftest(lm(formula, data = .), vcov = vcovHAC
    ))) |> 
    mutate(models_coef = map(models, ~tidy(.))) |> 
    mutate(obs = map(data, ~glance(lm(formula, data = .))$nobs)) |> 
    # Pretty results
    unnest(cols = models_coef, names_repair = "universal") |> 
    unnest(cols = obs, names_repair = "universal") |>
    dplyr::select( {{ group_var }}, term, estimate, p.value, obs) |>
    mutate(
      stars = ifelse(p.value < 0.01, "***", 
                     ifelse(p.value < 0.05, "**", 
                            ifelse(p.value < 0.1, "*", "")))) |>
    dplyr::select({{ group_var }},  term, estimate, p.value, stars, obs) |>
    mutate(across(2, ~strtrim(., 5))) |>
    mutate(across(3, ~strtrim(., 5))) |> 
    mutate(Estimate = paste0(estimate, stars)) |>
    dplyr::select(-estimate, -p.value, -stars)  |> 
    pivot_longer(-c( {{ group_var }}, term, obs)) |> 
    spread(key = term, value = value) |>
    dplyr::select(-name) |> 
    relocate(obs, .after = last_col()) |> 
    mutate(group = country_name) |> 
    rename("Constant" = `(Intercept)`,
           "N" = obs,
           "Change in yield" = {{ country_var }},
           "Factor" = variable)
}


robust_residuals <- 
  function(data, 
           group_var = variable, 
           vars, 
           country_var, 
           country_name, 
           country_replace,
           formula){
    data |> 
      dplyr::select(Date, all_of( {{ vars }})) |>
      pivot_longer(cols = -c(Date, {{ country_var }}), 
                   names_to = "variable", 
                   values_to = "value") |> 
      group_by({{ group_var }}) |> 
      nest() |> 
      # Fit the model
      mutate(models = map(data, ~coeftest(lm(formula, data = .), 
                                          vcov = vcovHAC
      ))) |> 
      mutate(models_coef = map(models, ~tidy(.))) |> 
      dplyr::select(-models) |> 
      unnest(cols = models_coef, names_repair = "universal") |> 
      unnest(cols = data, names_repair = "universal") |> 
      dplyr::select(-c(std.error, statistic, p.value)) |>
      mutate(term = str_replace(term, country_replace, "estimate")) |>
      mutate(term = str_replace(term, "\\(Intercept\\)", "constant")) |>
      pivot_wider( names_from = term,
                   values_from = estimate,
                   names_repair = "unique") |>
      mutate(
        residuals = value - constant - estimate * {{ country_var }}
      ) |>
      dplyr::select(Date, variable, residuals) |>
      ungroup() |>
      mutate(group = country_name)
  }
