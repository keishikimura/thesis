library(tidyverse)
library(readr)
library(lubridate)

# Produce citation counts -------------------------------------------------
# citations <- read_tsv("AI/data/raw/uspto/g_us_patent_citation.tsv") %>%
#   group_by(citation_patent_id) %>%
#   summarize(count = n())
# 
# write_csv(citations, "AI/data/generated/citation_counts.csv")

# Outside citations -------------------------------------------------------

#Location
inventor_dta <- read_tsv("AI/data/raw/uspto/g_inventor_disambiguated.tsv")

location_dta <- read_tsv("AI/data/raw/uspto/g_location_disambiguated.tsv") %>%
  select(-c(latitude, longitude))

msa2013_dta <- read_csv("AI/data/raw/nber/cbsatocountycrosswalk.csv") %>%
  filter(!is.na(y2013)) %>%
  select(-ssacounty) %>%
  unique() %>%
  select(fipscounty, msa)

merged_location <- 

#AI data
patent_dta <- read_csv("AI/data/generated/AIpatentMSA.csv") %>%
  filter(predict50_any_ai == 1)

citations <- read_tsv("AI/data/raw/uspto/g_us_patent_citation.tsv") %>%
  right_join(patent_dta, by = c("citation_patent_id" = "doc_id"))