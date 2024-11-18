agg <-
function(i) {
  data <- exactextractr::exact_extract(data_sr,
                                       filter(world_shp, id == i),
                                       max_cells_in_memory = 3e+20) %>%
    as.data.table() %>%
    dt_pivot_longer(
      cols = -c(coverage_fraction),
      names_to = "time",
      values_to = "t2m"
    ) %>%
    # .[, coverage_fraction := NULL] %>%
    dt_separate(time, into = c("time", "seconds"), sep = ".") %>%
    .[, seconds := as.numeric(seconds)] %>%
    .[, date :=  as.Date(lubridate::as_datetime(seconds, "1970-01-01"))] %>%
    .[, seconds := NULL] %>%
    .[, t2m := t2m - 273.15] %>%     # changing to Celsius
    .[, .(t2m = mean(t2m, na.rm = TRUE)), by = .(date)]

  return(data)

  gc()
}

agg_coverage <-
  function(i) {

    data <- exactextractr::exact_extract(data_sr,
                                         filter(world_shp, id == i),
                                         max_cells_in_memory = 3e+20) %>%
      as.data.table() %>%
      dt_pivot_longer(
        cols = -c(coverage_fraction),
        names_to = "time",
        values_to = "t2m"
      ) %>%
      dt_separate(time, into = c("time", "seconds"), sep = ".") %>%
      .[, seconds := as.numeric(seconds)] %>%
      .[, date :=  as.Date(lubridate::as_datetime(seconds, "1970-01-01"))] %>%
      .[, seconds := NULL] %>%
      .[, t2m := t2m - 273.15] %>%  # changing to Celsius
      .[, .(t2m = sum(t2m * coverage_fraction) / sum(coverage_fraction)), by = .(date)]


    return(data)

    gc()
  }
