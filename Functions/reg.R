reg <-
function(data, rows_keep = 5) {
  data |>
    filter(!col_industry %in% c("Communication", "Education")) |>
    group_by(col_industry) |>
    group_map( ~ {
      lm(formula = formula, data = .x) |>
        coeftest(vcov = vcovHC, type = "HC1") |>
        tidy() |>
        slice_head(n = rows_keep) |>
        mutate(
          conf.low = estimate - 1.96 * std.error,
          conf.high = estimate + 1.96 * std.error,
        ) |>
        mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
          p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
        )))
    }) |>
    set_names(industry_names)
}
