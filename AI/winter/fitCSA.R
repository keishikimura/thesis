library(tidyverse)
library(lubridate)

dta_ai <- read.csv("AI/data/generated/AIpatentCSA.csv")

run_model <- function(data, shock_year, cutoff_year, app = FALSE, log = FALSE){
  data <- data %>%
    mutate(
      pub_dt = ymd(pub_dt),
      filing_date = ymd(filing_date),
      year = if(app) year(filing_date) else year(pub_dt),
      trend = year - shock_year
    ) %>%
    filter(
      predict50_any_ai == 1
    )
  
  by_msa <- data %>%
    group_by(csa_inventor, csaname_inventor, year, trend) %>%
    summarize(count = n()) %>%
    filter(csaname_inventor != "")
  
  msa_index <- dta_ai %>%
    filter(univ == 1) %>%
    filter(year < shock_year & year > shock_year - 10) %>%
    group_by(csa_inventor, csaname_inventor) %>%
    summarize(count = n()) %>%
    filter(csaname_inventor != "") %>%
    rename(index = count)
  
  msa_index_all <- by_msa %>%
    left_join(msa_index, by = "csa_inventor") %>%
    mutate(
      index = if_else(is.na(index), 0 , index)
    ) %>%
    group_by(csa_inventor) %>%
    summarize(mean_index = mean(index)) %>%
    mutate(std_index = scale(mean_index))
  
  fit_dta <- by_msa %>%
    left_join(msa_index_all, by = "csa_inventor") %>%
    mutate(
      postshock = year >= shock_year,
      dlog = diff(log(count))
    ) %>%
    filter(year <= cutoff_year) 
  
  fit_csa <- lm(log(1+count) ~ std_index*trend*postshock + factor(csa_inventor), 
                data = fit_dta)
  
  fit_csa_basic <- lm(log(1+count) ~ std_index*postshock + factor(csa_inventor) + factor(year), 
                      data = fit_dta)
  
  return(list(fit_csa,fit_csa_basic))
}