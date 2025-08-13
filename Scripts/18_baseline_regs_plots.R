# Description
# Consolidating models for reporting - Xolani Sibande August 2025

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)
library(qs2)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
list_cleanup <- function(list) {
  list |>
    unlist(recursive = FALSE) |>
    unlist(recursive = FALSE) |>
    bind_rows(.id = "model") |>
    mutate(model = str_remove(model, "temp_regressions.|precip_regressions.")) |>
    mutate(sector = str_extract(model, "([^.]+)$")) |>
    mutate(model = str_remove(model, "_tbl.([^.]+)$")) |>
    relocate(sector, model)
}
my_model_plot <- function(data, term_var, low_model, high_model ){
  data |>
    dplyr::filter(term == term_var) |>
    filter(model %in% c(low_model, high_model)) |>
    ggplot(aes(y = sector, x = estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(size = 3, position = position_dodge(0.4)) +
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
    labs(x = "Coefficient estimates and 95% confidence interval", y = "") +
    scale_color_manual(values = pnw_palette("Bay", 2)) +
    coord_flip()
}
my_model_plots <- function(arg_list){
  map(args_list, function(x) {
    term_var <- x[1]
    low_model <- x[2]
    high_model <- x[3]

    my_model_plot(baseline_regs_tbl, term_var, low_model, high_model) +
      labs(
        title = glue("Baseline estimates for {term_var}")
      )})
}

# Import and clean up -------------------------------------------------------------
baseline_regs_tbl <- read_rds(here("Outputs", "artifacts_baseline_regressions.rds")) |>
  list_cleanup()

# Argument list -----------------------------
args_list <- list(
  # Inflation
  c("lag(inflation_rate)",
    "temp_baseline_10",
    "temp_baseline_90"),
  c("lag(inflation_rate)",
    "precip_baseline_10",
    "precip_baseline_90"),
  c("lag(inflation_rate)",
    "temp_baseline_5",
    "temp_baseline_95"),
  c("lag(inflation_rate)",
    "precip_baseline_5",
    "precip_baseline_95"),

  # Domestic agricultural temperature and precipitation shocks
  c("domestic_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"),
  c("domestic_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"),
  c("domestic_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"),
  c("domestic_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"),

  # Domestic non agricultural temperature and precipitation shocks
  c("domestic_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"),
  c("domestic_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"),
  c("domestic_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"),
  c("domestic_non_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"),

  # Foreign agricultural temperature and precipitation shocks
  c("foreign_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"),
  c("foreign_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"),
  c("foreign_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"),
  c("foreign_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95")
)

# Multiple model plots ---------------------------------------------------
baseline_regs_plots_list <-
  my_model_plots(args_list)

# Export ---------------------------------------------------------------
artifacts_baseline_regs <- list (
  baseline_regs_plots_list = baseline_regs_plots_list
)

write_rds(artifacts_baseline_regs, file = here("Outputs", "artifacts_baseline_regs.rds"))


