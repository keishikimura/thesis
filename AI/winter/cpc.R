library(tidyverse)
library(readr)
library(lubridate)

cpc_dta <- read_tsv("AI/data/raw/uspto/g_cpc_current.tsv")
dta_ai <- read.csv("AI/data/generated/AIpatentCSA.csv")
cpc_past <- read_tsv("AI/data/raw/uspto/g_cpc_at_issue.tsv")
uspc_past <- read_tsv("AI/data/raw/uspto/g_uspc_at_issue.tsv")

# Data --------------------------------------------------------------------

#Upon reviewing AI patents, the training set seems competent at getting expert 
#systems stuff by "AND" with USPC classifications

##CPC (present)
cpc_dta_es <- cpc_dta %>%
  filter(grepl("^G06N5", cpc_group)) %>%
  select(c(patent_id, cpc_group)) %>%
  mutate(
    patent_id = as.character(patent_id)
  ) %>%
  group_by(patent_id) %>%
  slice_head()

#CPC (past)
#less observations than above -- will ignore
#based on graphs, we only have observations from early 2010s,
#which imply classifications were made then. anything else is assigned
#retrospectively.
cpc_dta_past_es <- cpc_past %>%
  filter(grepl("^G06N5", cpc_group)) %>%
  select(c(patent_id, cpc_group)) %>%
  mutate(
    patent_id = as.character(patent_id)
  ) %>%
  group_by(patent_id) %>%
  slice_head()

#USPC (past)
#Same thing for this but from 1996ish only up to 2015
#no "current" USPC classification available; moved onto CPC from 2013
uspc_dta_es <- uspc_past %>%
  filter(uspc_subclass_id %in% paste("706", seq(45,61), sep="/")) %>%
  mutate(
    patent_id = as.character(patent_id)
  ) %>%
  group_by(patent_id) %>%
  slice_head()


# Graphs ------------------------------------------------------------------

es_all_merged <- uspc_dta_es %>%
  left_join(ai_dta, by = c("patent_id" = "doc_id")) %>%
  mutate(
    pub_dt = ymd(pub_dt),
    year = year(pub_dt)
  )

es_all_merged %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=year, y= count)) +
  geom_line() +
  geom_vline(xintercept = 1987) +
  #Lag?
  geom_vline(xintercept = 1989)

es_ai_merged <- dta_ai %>%
  left_join(uspc_dta_es, by = c("doc_id"="patent_id"))

es_ai_merged <- es_ai_merged %>%
  mutate(
    pub_dt = ymd(pub_dt),
    filing_date = ymd(filing_date),
    year = year(pub_dt),
    app_year = year(filing_date)
  ) %>%
  filter(
    predict50_any_ai == 1
  )

es_ai_merged %>% 
  filter(!is.na(cpc_group)) %>%
  mutate(univ = as.logical(ifelse(is.na(univ), 0, univ))) %>%
  group_by(year, univ) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=year, y= count, color = univ)) +
  geom_line() +
  geom_vline(xintercept = 1987) +
  #Lag?
  geom_vline(xintercept = 1989)
