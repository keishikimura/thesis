library(targets)
library(tarchetypes)
library(qs2)

source("targets_config.R")

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tibble",
               "tidyverse",
               "readr",
               "readxl",
               "lubridate",
               "sandwich",
               "lmtest",
               "dfadjust",
               "knitr",
               "kableExtra",
               "scales",
               "stargazer"),
  format = "qs",
  controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
)

DO_ALL_GEOTYPES <- TRUE
city_types <- if (DO_ALL_GEOTYPES) c("MSA", "CSA", "CBSA") else c("MSA")
