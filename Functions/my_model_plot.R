my_model_plot <-
function(data, term_var, low_model, high_model ){
  data |>
    dplyr::filter(term == term_var) |>
    filter(model %in% c(low_model, high_model)) |>
    ggplot(aes(y = sector, x = estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(size = 1.5, position = position_dodge(0.5)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  width = 0.5, position = position_dodge(0.4)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    theme(
      text = element_text(size = 9),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 7),
      plot.tag = element_text(size = 8)) +
    labs(x = "Coefficients", y = "") +
    scale_color_manual(values = pnw_palette("Bay", 2))
    # coord_flip()
}
