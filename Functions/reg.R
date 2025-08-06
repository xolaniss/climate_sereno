reg <-
function(data) {
  data |>
    filter(!col_industry %in% c("Communication", "Education")) |>
    group_by(col_industry) |>
    group_map( ~ {
      lm(formula = formula, data = .x) |>
        coeftest(vcov = vcovHC, type = "HC1") |>
        tidy() |>
        mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
          p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
        )))
    }) |>
    set_names(industry_names)
}
