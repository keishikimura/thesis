library(readr)
library(tidyverse)
library(readxl)

# AI ----------------------------------------------------------------------

ai_dta <- read_tsv("AI/data/raw/uspto/ai_model_predictions.tsv") %>%
  filter(
    flag_patent == 1
  ) %>%
  select(-c(appl_id, flag_patent, flag_train_any, flag_train_ml,
            flag_train_evo, flag_train_nlp, flag_train_speech,
            flag_train_vision, flag_train_kr, flag_train_planning,
            flag_train_hardware))

# Application -------------------------------------------------------------

app_dta <- read_tsv("AI/data/raw/uspto/g_application.tsv") %>%
  select(c(patent_id, filing_date))

# Location ----------------------------------------------------------------

location_dta <- read_tsv("AI/data/raw/uspto/g_location_disambiguated.tsv") %>%
  select(-c(latitude, longitude))

# FIPS-MSA ----------------------------------------------------------------

msa2013_dta <- read_csv("AI/data/raw/nber/cbsatocountycrosswalk.csv") %>%
  filter(!is.na(y2013)) %>%
  select(-ssacounty) %>%
  unique()

csa2013_dta <- read_excel("AI/data/raw/census/qcew-county-msa-csa-crosswalk.xlsx",
                          sheet = 3)

names(csa2013_dta)<-make.names(names(csa2013_dta),unique = TRUE)

csa_dta <- msa2013_dta %>%
  mutate(
    fipscounty = as.numeric(fipscounty)
  ) %>%
  left_join(csa2013_dta, by = c("fipscounty"="County.Code")) %>%
  rename(
    CBSA.Code = cbsa,
    CBSA.Title = cbsaname
  )

# Inventor-MSA ------------------------------------------------------------

inventor_dta <- read_tsv("AI/data/raw/uspto/g_inventor_disambiguated.tsv")

inventor_location_dta <- inventor_dta %>%
  left_join(location_dta, by = "location_id") %>%
  ##Only want US-based inventors
  filter(disambig_country == "US")

inventor_prep <- function(city_type){
  
  city_var <- paste0(city_type, ".Code")
  title_var <- paste0(city_type, ".Title")

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
      in.city = !is.na(.data[[city_var]]),
      in.msa = !is.na(MSA.Code),
      city = ifelse(in.city, .data[[city_var]], paste0(countyname, state)),
      cityname = ifelse(in.city, .data[[title_var]], paste0(countyname, state))
    ) %>%
    select(c(patent_id, inventor_sequence, city, cityname, in.city, in.msa)) %>%
    ##Get most frequent CITY_TYPE
    ##If tie, select CITY_TYPE of highest ranked inventor
    group_by(patent_id, city, cityname, in.city, in.msa) %>%
    summarise(
      freq = n(),
      min_inventor_sequence = min(inventor_sequence)
    ) %>%
    arrange(desc(freq), min_inventor_sequence) %>%
    group_by(patent_id) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(inventor_MSA_dta)
}

# Assignee-MSA ------------------------------------------------------------

startup_dta <- read_csv("AI/data/raw/uspto/_patent_ocpb_augmented.csv")

assignee_location_dta <- startup_dta %>%
  left_join(location_dta, by = "location_id")  %>%
  ##Only want US-based assignees
  filter(disambig_country == "US")

assignee_prep <- function(city_type){
  
  city_var <- paste0(city_type, ".Code")
  title_var <- paste0(city_type, ".Title")
  
  assignee_MSA_dta <- assignee_location_dta %>%
    ##Link with MSAs
    ##Again, many NAs in Ebeye
    mutate(
      fips = as.numeric(paste0(state_fips, county_fips))
    ) %>%
    filter(!is.na(fips)) %>%
    left_join(csa_dta, by = c("fips" = "fipscounty")) %>%
    mutate(
      in.city = !is.na(.data[[city_var]]),
      in.msa = !is.na(MSA.Code),
      city = ifelse(in.city, .data[[city_var]], paste0(countyname, state.x)),
      cityname = ifelse(in.city, .data[[title_var]], paste0(countyname, state.x))
    ) %>%
    select(c(patent_id, assignee_sequence, founding_year, assignee_sequence, 
             assignee_type, organization, university, UO_DISCERN, SUB_DISCERN,
             first_year_publicly_listed, VC_backed_assignee, city, cityname,
             in.city, in.msa)) %>%
    ##Since only 284 patents have multiple assignees and all have 2,
    ##we just choose the highest ranked assignee for all patents
    group_by(patent_id) %>%
    #Keep university indicator and location irrespective of assignee order
    mutate(univ = if_else(any(university == 1), 1, 0),
           univcity = paste(unique(city[university == 1]), collapse = ", "),
           univcityname = paste(unique(cityname[university == 1]), collapse = ", ")) %>%
    arrange(assignee_sequence) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(assignee_MSA_dta)
}

# Merge -------------------------------------------------------------------

master_prep <- function(city_type){
  inventor_MSA_dta <- inventor_prep(city_type)
  print("Inventor Data Prepared.")
  assignee_MSA_dta <- assignee_prep(city_type)
  print("Assignee Data Prepared.")
  
  assignee_MSA_dta_merge <- assignee_MSA_dta %>%
    rename(city_assignee = city, cityname_assignee = cityname) %>%
    select(-assignee_sequence)
  
  inventor_MSA_dta_merge <- inventor_MSA_dta %>%
    rename(city_inventor = city, cityname_inventor = cityname) %>%
    select(-c(min_inventor_sequence, freq))
  
  merged_dta <- ai_dta %>%
    left_join(inventor_MSA_dta_merge, by = c("doc_id" = "patent_id")) %>%
    left_join(assignee_MSA_dta_merge, by = c("doc_id" = "patent_id")) %>%
    left_join(app_dta, by = c("doc_id" = "patent_id")) %>%
    filter(!is.na(city_inventor))
  print("Data merged.")
  
  write.csv(merged_dta, paste0("AI/data/generated/AIpatent", city_type, ".csv"), row.names = FALSE)
}

master_prep("CBSA")
master_prep("CSA")
master_prep("MSA")
