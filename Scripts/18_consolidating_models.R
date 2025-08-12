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

# Import -------------------------------------------------------------
baseline_regs_list <- read_rds(here("Outputs", "artifacts_baseline_regressions.rds")) |>
  unlist(recursive = FALSE) |>
  unlist(recursive = FALSE)

baseline_regs_tbl <-
  baseline_regs_list |>
  bind_rows(.id = "model") |>
  mutate(sector = str_extract(model, "[A-Z][a-z]*")) |>
  mutate(model = str_remove(model, ".[A-Z][a-z]*")) |>
  mutate(model = str_remove(model, "[A-Z][a-z]*")) |>
  mutate(model = str_remove(model, "temp_regressions.|precip_regressions.")) |>
  mutate(model = str_remove(model, "_tbl")) |>
  relocate(sector, model)

baseline_regs_tbl$term

ggplot(baseline_regs_tbl |>
         filter(term == "domestic_agricultural_temperature_shock") |>
         filter(model %in% c("temp_baseline_5", "temp_baseline_95")),
       aes(y = sector,
           x = estimate,
           color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(x = estimate,
                 y = sector),
             size = 3,
             position = position_dodge(0.5)) +
  geom_errorbar(aes(xmin = conf.low,
              xmax = conf.high),
              width = 0.5,
              position = position_dodge(0.5)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(
    text = element_text(size = 9),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 7),
    plot.tag = element_text(size = 8)
  ) +
  labs(x = "Coefficient estimates and 95% confidence interval", y = "") +
  scale_color_manual(values = pnw_palette("Bay", 2))


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


