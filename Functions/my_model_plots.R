my_model_plots <-
function(args_list, data, title, model_type){
  map(args_list, function(x) {
    term_var <- x[1]
    low_model <- x[2]
    high_model <- x[3]

    my_model_plot(data, term_var, low_model, high_model) +
      labs(
        title = glue("{model_type} estimates for {term_var}")
      )})
}
