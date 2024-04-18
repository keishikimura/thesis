# (0) Summary statistics: time series, Ellison-Glaeser, university, etc.
library(tidyverse)

dta_ai <- read.csv("AI/data/generated/AIpatentMSA.csv") %>%
  mutate(
    pub_dt = ymd(pub_dt),
    year = year(pub_dt),
    filing_date = ymd(filing_date),
    app_year = year(filing_date)
    #trend = year - 1987
  ) %>%
  filter(
    app_year >= 1976
  )

dta_ai_cut <- dta_ai %>%
  filter(
    app_year <= 2015
  )

count_by_year <- function(){
  class_vec <- c("any_ai","nlp","kr","planning","hardware","vision", "speech", "ml")
  
  count_helper <- function(class){
    class_var <- paste0("predict50_", class)
    
    count_dta <- dta_ai_cut %>%
      filter(.data[[class_var]] == 1) %>%
      group_by(app_year) %>%
      summarize(count = n()) %>%
      mutate(type = class)
    
    return(count_dta)
  }
  
  df <- lapply(class_vec, function(x) count_helper(x)) %>%
    bind_rows()
  
  return(df)
}

by_year_counts <- count_by_year()

by_msa <- dta_ai %>%
  group_by(city_inventor, cityname_inventor, year) %>%
  summarize(count = n()) %>%
  filter(cityname_inventor != "")

by_year_counts %>%
  ggplot(aes(x=app_year, y= count, color = type)) +
  geom_line() +
  labs(
    y = "Patent count",
    x = "Application year",
    color = "Component"
  )

# Old time series ---------------------------------------------------------

#University vs. Industry Patents (time series)
dta_ai_cut %>%
  mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
  group_by(app_year, univ) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=app_year, y= count, color = univ)) +
  geom_line() +
  labs(
    x = "Application year",
    y = "Patent count",
    color = "University"
  )
# 
# ##Close-up to 1987
# dta_ai %>% 
#   mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
#   group_by(year, univ) %>%
#   filter(year <= 1997) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x=year, y= count, color = univ)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)
# 
# ##Same as above but only for knowledge processing (expert systems)
# dta_ai %>% 
#   filter(predict50_kr == 1) %>%
#   mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
#   group_by(year, univ) %>%
#   filter(year <= 1997) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x=year, y= count, color = univ)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)
# 
# ##Knowledge processing but with P >= 0.9
# dta_ai %>% 
#   filter(ai_score_kr >= 0.9) %>%
#   mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
#   group_by(year, univ) %>%
#   filter(year <= 1997) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x=year, y= count, color = univ)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)
# 
# ##By region (time series)
# by_msa %>%
#   filter(year <= 1993 & year >= 1985) %>%
#   mutate(region = ifelse(csa_inventor %in% c("CS488",
#                                                   "CS148",
#                                                   "CS220",
#                                                   "CS348",
#                                                   "CS408"), csaname_inventor, "Other")) %>%
#   group_by(region ,year) %>%
#   summarize(mean_count = mean(count)) %>%
#   ggplot(aes(x=year, y= mean_count, color = region)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)
# 
# ##By application
# dta_ai %>% 
#   mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
#   group_by(app_year, univ) %>%
#   filter(app_year <= 1997 & app_year >= 1981) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x=app_year, y= count, color = univ)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)
# 
# 
# by_msa %>%
#   filter(app_year <= 1993 & app_year >= 1985) %>%
#   mutate(region = ifelse(csa_inventor %in% c("CS488",
#                                              "CS148",
#                                              "CS220",
#                                              "CS348",
#                                              "CS408"), csaname_inventor, "Other")) %>%
#   group_by(region, app_year) %>%
#   summarize(mean_count = mean(count)) %>%
#   ggplot(aes(x=app_year, y= mean_count, color = region)) +
#   geom_line() +
#   geom_vline(xintercept = 1987) +
#   #Lag?
#   geom_vline(xintercept = 1989)