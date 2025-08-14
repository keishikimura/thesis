library(tidyverse)

dta_ai <- read.csv("AI/data/generated/AIpatentMSA.csv") %>%
  mutate(
    pub_dt = ymd(pub_dt),
    year = year(pub_dt),
    filing_date = ymd(filing_date),
    app_year = year(filing_date)
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

ggsave("AI/figures/fig1.png", width = 6, height = 5, units = "in")