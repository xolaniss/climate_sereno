temp_nc_extraction <-
function(data_sr = data_sr, sf_shp = sf_shp){
  # Import data
  data_sr # import with terra Spatraster

  # sf shp and crs transform
  sf_shp %>% #
    dplyr::select(name, region, geometry) %>%
    st_transform(crs = st_crs(data_sr))

  # Extracting data to data.table
  extraction_dt <- exactextractr::exact_extract(data_sr, world_shp, max_cells_in_memory = 3e+20) %>%
    bind_rows(.id = "id") %>%
    as.data.table()

  # Summarising to days
  extraction_long_tbl <-
    extraction_dt  %>%
    dt_pivot_longer(cols = -c(id, coverage_fraction), names_to = "time", values_to = "t2m") %>%
    dt_separate(time, into = c("time", "seconds"), sep = "=") %>%
    .[, seconds := as.numeric(seconds)] %>%
    .[, date :=  lubridate::as_datetime(seconds, "1970-01-01")] %>%
    .[, date := as.Date(date)] %>%
    .[, seconds := NULL] %>%
    .[, c("id", "date", "t2m") := .(id, date, t2m)] %>%
    .[, id := as.numeric(id)] %>%
    .[, .(t2m = mean(t2m, na.rm = TRUE)), by = .(id, date)] %>%
    .[, t2m := t2m - 273.15] %>%  # changing to Celsius
    as_tibble()

  # Joining sf data
  world_2m_temp_shp <- world_shp %>%
    mutate(id = seq_len(nrow(.))) %>%
    left_join(extraction_long_tbl, by = "id") %>%
    dplyr::select(id, date, name, region, t2m)

  # Returning shp with data
  return(world_2m_temp_shp)
}
