list_cleanup <-
function(list) {
  list |>
    unlist(recursive = FALSE) |>
    unlist(recursive = FALSE) |>
    bind_rows(.id = "model") |>
    mutate(model = str_remove(model, "temp_regressions.|precip_regressions.|precip_reg_list.|temp_reg_list.|temp_inflation_regs_list.|precip_inflation_regs_list.|temp_hemisphere_reg_list.|precip_hemisphere_reg_list.|temp_income_reg_list.|precip_income_reg_list.")) |>
    mutate(sector = str_extract(model, "([^.]+)$")) |>
    mutate(model = str_remove(model, "_tbl.([^.]+)$")) |>
    relocate(sector, model)
}
