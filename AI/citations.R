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

#AI data
patent_dta <- read_csv("AI/data/generated/AIpatentMSA.csv") %>%
  filter(predict50_any_ai == 1) %>%
  select(doc_id, city_inventor, cityname_inventor, filing_date)

ai_citations <- read_tsv("AI/data/raw/uspto/g_us_patent_citation.tsv") %>%
  select(patent_id, citation_patent_id) %>%
  right_join(patent_dta, by = c("citation_patent_id" = "doc_id")) %>%
  filter(!is.na(patent_id))

#Location on patents
inventor_location_dta <- read_tsv("AI/data/raw/uspto/g_inventor_disambiguated.tsv") %>%
  left_join(read_tsv("AI/data/raw/uspto/g_location_disambiguated.tsv"), by = "location_id") %>%
  ##Only want US-based inventors
  filter(disambig_country == "US")

#From dataprep.R
csa_dta <- csa_dta %>%
  select(fipscounty, MSA.Code, MSA.Title, countyname, state)

inventor_MSA_dta <- inventor_location_dta %>%
  ##Link with MSAs
  ##(NAs on many Japanese inventors registered in Ebeye island???)
  mutate(
    fips = as.numeric(paste0(state_fips, county_fips))
  ) %>%
  na.omit() %>%
  left_join(csa_dta, by = c("fips" = "fipscounty")) %>%
  ##Preserve county name and state for those not in CITY_TYPE
  mutate(
    in.city = !is.na(MSA.Code),
    city = ifelse(in.city, MSA.Code, paste0(countyname, state)),
    cityname = ifelse(in.city, MSA.Title, paste0(countyname, state))
  ) %>%
  select(c(patent_id, inventor_sequence, city, cityname, in.city)) %>%
  ##Get most frequent CITY_TYPE
  ##If tie, select CITY_TYPE of highest ranked inventor
  group_by(patent_id, city, cityname, in.city) %>%
  summarise(
    freq = n(),
    min_inventor_sequence = min(inventor_sequence)
  ) %>%
  arrange(desc(freq), min_inventor_sequence) %>%
  group_by(patent_id) %>%
  slice_head(n = 1) %>%
  ungroup()

ai_citations_loc <- ai_citations %>%
  left_join(inventor_MSA_dta, by = "patent_id") %>%
  #Some inventor data are missing! If missing, select outside as TRUE.
  mutate(
    outside = (city_inventor != city),
    outside = ifelse(is.na(outside), TRUE, outside)
  ) %>%
  group_by(citation_patent_id) %>%
  summarize(
    count = n(),
    sum_outside = sum(outside)
  )

write_csv(ai_citations_loc, "AI/data/generated/citation_outside.csv")

# test <- dta_all_merged %>%
#   select(doc_id, forward_citations) %>%
#   left_join(ai_citations_loc, by = c("doc_id" = "citation_patent_id")) %>%
#   filter(forward_citations != count)
