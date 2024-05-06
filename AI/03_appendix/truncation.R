library(tidyverse)
library(readr)
library(lubridate)

# Data --------------------------------------------------------------------

patent_dta <- read_csv("AI/data/generated/AIpatentMSA.csv") %>%
  filter(predict50_any_ai == 1) %>%
  select(doc_id, city_inventor, cityname_inventor, filing_date,
         predict50_ml, predict50_nlp, predict50_planning,
         predict50_kr, predict50_hardware, predict50_speech,
         predict50_vision)

ai_citations <- read_tsv("AI/data/raw/uspto/g_us_patent_citation.tsv") %>%
  select(patent_id, citation_patent_id) %>%
  right_join(patent_dta, by = c("citation_patent_id" = "doc_id")) %>%
  filter(!is.na(patent_id))

# Citation lifespan -------------------------------------------------------

#Get citing patent information and merge
patent <- read_tsv("AI/data/raw/uspto/g_patent.tsv") %>%
  select(patent_id, patent_date)

ai_citations_merged <- ai_citations %>%
  left_join(patent, by = "patent_id")

ai_citations_merged <- ai_citations_merged %>%
  #Set patent application year, citation year, and years of lag
  mutate(
    app_year = year(filing_date),
    citation_year = year(patent_date),
    citation_lag = citation_year - app_year
  )

#Identify highly cited patents (top 1 percent in decade)
highly_cited <- function(start_year){
  highly_cited_pat <- ai_citations %>%
    mutate(app_year = year(filing_date)) %>%
    filter(app_year >= start_year & app_year <= start_year + 9) %>%
    group_by(citation_patent_id, filing_date) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(q99 = quantile(count, 0.95)) %>%
    filter(count > q99) %>%
    mutate(decade_start = start_year) %>%
    select(c(citation_patent_id, decade_start))
  
  return(highly_cited_pat)
}

decades <- c(1980, 1990, 2000)

highly_cited_df <- lapply(decades, highly_cited) %>%
  bind_rows()

#Find sum citations on patent per lag year
ai_citations_merged <- ai_citations_merged %>%
  group_by(citation_lag, citation_patent_id) %>%
  mutate(sum_lag_citation = n()) %>%
  ungroup()

#Filter for AI patents (7 classifications) and apps between 1976 and 2015
ai_citations_merged <- ai_citations_merged %>%
  filter(app_year >= 1976 & app_year <= 2015) %>%
  filter(rowSums(select(., predict50_ml, predict50_nlp, predict50_planning,
                        predict50_kr, predict50_hardware, predict50_speech,
                        predict50_vision) == 0) != 7)

#Take one observation per cited patent
citations_by_lag <- ai_citations_merged %>%
  group_by(citation_patent_id, citation_lag) %>%
  slice_head(n=1) %>%
  select(-c(patent_id, patent_date)) %>%
  ungroup()

#Take time-invariant patent info to join later
patent_info <- citations_by_lag %>%
  group_by(citation_patent_id) %>%
  slice_head(n=1) %>%
  select(-c(citation_lag, sum_lag_citation, citation_year))

# Generate a complete sequence of years for each patent up to the max year found
citations_expanded <- citations_by_lag %>%
  select(citation_patent_id, citation_lag, sum_lag_citation) %>%
  complete(citation_patent_id, citation_lag = full_seq(citation_lag, 1),
           fill = list(sum_lag_citation = 0)) %>%
  left_join(patent_info, by = "citation_patent_id")

# Calculate the cumulative sum of citations per patent
cumulative_citations <- citations_expanded %>%
  arrange(citation_patent_id, citation_lag) %>%
  group_by(citation_patent_id) %>%
  mutate(cumulative_count = cumsum(sum_lag_citation)) %>%
  select(citation_patent_id, citation_lag, cumulative_count)

#Find mean citations per lag year
mean_citations_by_lag <- citations_expanded %>%
  group_by(citation_lag) %>%
  filter(citation_lag + app_year <= 2023) %>%
  summarize(avg_citation = mean(sum_lag_citation))

#Plot
mean_citations_by_lag %>% 
  filter(citation_lag >= 0) %>%
  ggplot(aes(x = citation_lag, y = avg_citation)) +
  geom_line()
ggsave("AI/figures/citations/figA1.png")

#Do the same for highly cited patents
mean_citations_hc <- citations_expanded %>%
  right_join(highly_cited_df, by = "citation_patent_id") %>%
  group_by(citation_lag, decade_start) %>%
  filter(citation_lag + app_year <= 2023) %>%
  summarize(avg_citation = mean(sum_lag_citation))

mean_citations_hc %>% 
  filter(citation_lag >= 0) %>%
  ggplot(aes(x = citation_lag, y = avg_citation, color = as.factor(decade_start))) +
  geom_line() +
  labs(
    color = "Decade"
  )
ggsave("AI/figures/citations/figA2.png")

#Do the same for each class
count_by_year <- function(){
  class_vec <- c("nlp","kr","planning","hardware","vision", "speech", "ml")
  
  count_helper <- function(class){
    class_var <- paste0("predict50_", class)
    
    count_dta <- citations_expanded %>%
      filter(.data[[class_var]] == 1) %>%
      group_by(citation_lag) %>%
      filter(citation_lag + app_year <= 2023) %>%
      summarize(avg_citation = mean(sum_lag_citation)) %>%
      mutate(type = class)
    
    return(count_dta)
  }
  
  df <- lapply(class_vec, function(x) count_helper(x)) %>%
    bind_rows()
  
  return(df)
}

by_class <- count_by_year()

#Plot by class
by_class %>% 
  filter(citation_lag >= 0) %>%
  ggplot(aes(x = citation_lag, y = avg_citation, color = type)) +
  geom_line()
ggsave("AI/figures/citations/figA3.png")

#Do the same but for each year within a 10 year cohort
within_cohort <- function(cohort){
  year_vec <- c(cohort, cohort+5)
  
  count_helper <- function(year){
    
    count_dta <- citations_expanded %>%
      filter(app_year >= year & app_year <= year + 4) %>%
      group_by(citation_lag) %>%
      filter(citation_lag + app_year <= 2023) %>%
      summarize(avg_citation = mean(sum_lag_citation)) %>%
      mutate(type = year)
    
    return(count_dta)
  }
  
  df <- lapply(year_vec, function(x) count_helper(x)) %>%
    bind_rows()
  
  df %>% 
    filter(citation_lag >= 0) %>%
    ggplot(aes(x = citation_lag, y = avg_citation, color = as.factor(type))) +
    geom_line() +
    labs(
      color = "Split start year"
    )
}

within_cohort(1980)
ggsave("AI/figures/citations/figA4a.png")

within_cohort(1985)
ggsave("AI/figures/citations/figA4b.png")

within_cohort(1990)
ggsave("AI/figures/citations/figA4c.png")

within_cohort(1995)
ggsave("AI/figures/citations/figA4d.png")

within_cohort(2000)
ggsave("AI/figures/citations/figA4e.png")
