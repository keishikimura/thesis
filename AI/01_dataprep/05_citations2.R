library(tidyverse)
library(readr)
library(lubridate)

#AI patents
patent_dta <- read_csv("AI/data/generated/AIpatentMSA.csv") %>%
  filter(predict50_any_ai == 1) %>%
  select(doc_id, city_inventor, cityname_inventor, filing_date,
         predict50_ml, predict50_nlp, predict50_planning,
         predict50_kr, predict50_hardware, predict50_speech,
         predict50_vision)

#Get citations
ai_citations <- read_tsv("AI/data/raw/uspto/g_us_patent_citation.tsv") %>%
  select(patent_id, citation_patent_id) %>%
  right_join(patent_dta, by = c("citation_patent_id" = "doc_id")) %>%
  filter(!is.na(patent_id))

#General patent info
patent <- read_tsv("AI/data/raw/uspto/g_patent.tsv") %>%
  select(patent_id, patent_date)

#Patent location info
inventor_location_dta <- read_tsv("AI/data/raw/uspto/g_inventor_disambiguated.tsv") %>%
  left_join(read_tsv("AI/data/raw/uspto/g_location_disambiguated.tsv"), by = "location_id") %>%
  ##Only want US-based inventors
  filter(disambig_country == "US")

#CSA info
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
  ) 

# Joining -----------------------------------------------------------------

ai_citations_merged <- ai_citations_loc %>%
  left_join(patent, by = "patent_id")

ai_citations_merged <- ai_citations_merged %>%
  #Set patent application year, citation year, and years of lag
  mutate(
    app_year = year(filing_date),
    citation_year = year(patent_date),
    citation_lag = citation_year - app_year
  )

#Find sum citations on patent per lag year
ai_citations_merged <- ai_citations_merged %>%
  group_by(citation_lag, citation_patent_id) %>%
  mutate(sum_lag_citation = n(),
         sum_lag_outside = sum(outside)) %>%
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
  select(-c(citation_lag, sum_lag_citation, citation_year, sum_lag_outside))

# Generate a complete sequence of years for each patent up to the max year found
citations_expanded <- citations_by_lag %>%
  select(citation_patent_id, citation_lag, sum_lag_citation, sum_lag_outside) %>%
  complete(citation_patent_id, citation_lag = full_seq(citation_lag, 1),
           fill = list(sum_lag_citation = 0, sum_lag_outside = 0))

# Calculate the cumulative sum of citations per patent
cumulative_citations <- citations_expanded %>%
  arrange(citation_patent_id, citation_lag) %>%
  group_by(citation_patent_id) %>%
  mutate(cum_count = cumsum(sum_lag_citation),
         cum_outside = cumsum(sum_lag_outside)) %>%
  select(citation_patent_id, citation_lag, cum_count, cum_outside)

write_csv(cumulative_citations, "AI/data/generated/cum_citations_outside.csv")

# Test --------------------------------------------------------------------

# original <- read_csv("AI/data/generated/citation_outside.csv")
# 
# cum_citations <- function(lag){
#   cit_varname <- paste0("count_", as.character(lag))
#   out_varname <- paste0("out_", as.character(lag))
#   
#   df <- read_csv("AI/data/generated/cum_citations_outside.csv") %>%
#     filter(citation_lag == lag) %>%
#     select(-citation_lag) %>%
#     rename(!!cit_varname := cum_count,
#            !!out_varname := cum_outside)
# }
# 
# citation_cum_47 <- cum_citations(47)
# 
# merged_test <- citation_cum_47 %>%
#   left_join(original, by = "citation_patent_id") %>%
#   filter(count_47 != count | out_47 != sum_outside)