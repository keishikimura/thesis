# Disambiguated inventor data

## Code plan

## Merge by patent ID, set unique inventor/assignee IDs to raw locations by year to track movement
## Should be enough data to flag incumbent/entree/migrant/exitee and assignee/non-assignees
## Also run simple analyses of migratory patterns of breakthrough inventors, by itself

# USPTO disambiguated inventor data has already run the UMass algorithm...
# Double-checking...looks good:
location_test <- inventor_location_dta %>%
  group_by(inventor_id) %>%
  summarize(n = n_distinct(location_id)) %>%
  filter(n > 1)

# Why not add the VC column from assignee data?
# Can make a "VC index"
# Also would be interesting to decompose growth by university, VC, 
# recent IPO, startup (founded <10 years ago) vectors 
# (other: old companies, private companies, etc. )

# 1. Process inventor data
# Idea: before summarizing counts, set flags for breakthrough co-authors,
# incumbents, etc. and have separate patent counts for each. Then summarize
# by year-MSA pairs.
# Need to consider co-authors with different MSAs. For ease of analysis now,
# just look at the first. Then might be interesting to see for all of them, and
# whether they get closer (within 10km) to themselves or to assignee firm.
# To set the flag, we need to merge earlier to get the years on patents, and also
# fix the pre and post year bins. If the address switches, then is migrant. If not, 
# incumbent. If observed for first time in post, then entrant. For people who never
# show up again, exitees.

# 2. Process assignee data
# Same idea with inventors, but with a location dimension and type-of-firm (inventor, startup, etc.) dimension.
# Can run the regression with a three-way interaction.

# 3. Process breakthrough inventor data
# Simple summary stats on if a breakthrough inventor exits or not, and regress
# that on city and assignee characteristics.

# 8/24 --------------------------------------------------------------------

#Making dataset of inventor-year pairs with MSA location

library(targets)
library(tidyverse)

tar_load(inventor_location_dta)
tar_load(csa_dta)
tar_load(inventor_MSA_map)
tar_load(app_dta)

city_type <- "MSA"

city_var <- paste0(city_type, ".Code")
title_var <- paste0(city_type, ".Title")

inventor_year <- inventor_location_dta %>%
  left_join(app_dta, by = "patent_id") %>%
  mutate(fips = as.numeric(paste0(state_fips, county_fips))) %>%
  filter(!is.na(fips)) %>%
  left_join(csa_dta, by = c("fips" = "fipscounty")) %>%
  ##Preserve county name and state for those not in CITY_TYPE
  mutate(
    filler = ifelse(is.na(countyname) | is.na(state), 
                    NA_character_, paste0(countyname, state)),
    in.city = !is.na(.data[[city_var]]),
    in.msa = !is.na(MSA.Code),
    city = ifelse(in.city, .data[[city_var]], filler),
    cityname = ifelse(in.city, .data[[title_var]], filler)
  ) %>%
  mutate(year = year(filing_date)) %>%
  select(c(inventor_id, city, cityname, in.city, in.msa, Division, year)) %>%
  distinct()

# For these people, should decide whether they go back and forth across the two 
# time periods. 1985-1999 v. 2000-2014 (breakthrough period: 1990-1999)
inventor_year %>%
  group_by(inventor_id, year) %>%
  summarize(count= n()) %>%
  filter(count > 1)

# Since what matters is their residence pre-2000...
pre_migration <- inventor_year %>%
  as_tibble() %>%
  filter(year < 2000) %>%
  rename(pre_city = city) %>%
  distinct(inventor_id, pre_city)

post_migration <- inventor_year %>%
  as_tibble() %>%
  filter(year >= 2000) %>%
  rename(post_city = city) %>%
  distinct(inventor_id, post_city)

#Join this back up with patents, then list all the inventors' pre and post MSAs!
patent_pre_post <- inventor_location_dta %>%
  left_join(pre_migration, by = "inventor_id", relationship = "many-to-many") %>%
  distinct(patent_id, pre_city) %>%
  left_join(inventor_MSA_map, by = "patent_id") %>%
  mutate(match = city == pre_city) %>%
  group_by(patent_id) %>%
  summarize(migrant_status = case_when(
    sum(match, na.rm = T) > 0 ~ "incumbent", #At least one inventor has been in patent city
    all(is.na(match)) ~ "entrant", #All inventors not observed in pre-period
    TRUE ~ "migrant" #All inventors in pre-period have not been in patent city
  ))

inventor_pre_post <- inventor_MSA_cw %>%
  select(patent_id, inventor_id, city) %>%
  #Many inventor-patents to many inventor-pre-city
  left_join(pre_migration, by = "inventor_id", relationship = "many-to-many") %>%
  mutate(match = city == pre_city) %>%
  group_by(inventor_id, patent_id) %>%
  summarize(
    migrant_status = case_when(
      sum(match, na.rm = T) > 0 ~ "incumbent", #Inventor has been in patent city
      all(is.na(match)) ~ "entrant", #Inventor not observed in pre-period
      TRUE ~ "migrant" #Inventor has not been in city.
    )
  )

# 8/25 --------------------------------------------------------------------

# Adding breakthrough inventor identification

# Pull breakthrough patents earlier and re-merge...(not the most efficient way, for sure)
# Actually, change of thought. For breakthrough inventors, their share will still be 
# marginal, but we care about where they go afterwards. So I think that warrants a
# separate running dataset for its own analysis (that's also easier anyway.)

# More details of implementation in iPad
tar_load(merge_master)
excl <- FALSE
treatment_year <- 1999
pre_periods <- 9
city_var <- "msa_inventor"

# Take top patents in each class, along with year filed, city (of patent), and class
top_patents <- merge_master %>%
  pivot_longer(values_to = "in_class", names_to = "class",
               cols = c(starts_with("predict50_"), -predict50_any_ai),
               names_prefix = "predict50_") %>%
  filter(in_class == 1) %>%
  filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
  group_by(class) %>%
  mutate(
    top_1_cit = forward_citations > quantile(forward_citations, 0.99)
  ) %>%
  ungroup() %>%
  select(doc_id, class, top_1_cit, msa_inventor, year) %>%
  pivot_wider(
    names_from = class,
    values_from = top_1_cit,
    names_prefix = "top1_predict50_",
    values_fill = FALSE
  ) %>%
  distinct() %>%
  rename(
    filing_year = year
  )

# Produce inventor panels with key characteristics in pre-period
inventor_cov <- merge_master %>%
  left_join(inventor_location_dta %>% select(inventor_id, patent_id), 
            by = c("doc_id" = "patent_id")) %>%
  left_join(top_patents, by = "doc_id") %>%
  group_by(inventor_id) %>%
  filter(year <= treatment_year) %>%
  summarize(
    # AI innovation metrics
    across(
      starts_with("predict50_"),
      list(sum = ~sum(.x, na.rm = TRUE)),
      .names = "{.fn}_{.col}"
    ),
    ai_breadth = sum(colSums(pick(c(starts_with("predict50_"), -predict50_any_ai)), na.rm = TRUE) > 0), #NOT WORKING CORRECTLY!
    # Breakthrough inventions
    across(
      starts_with("top1_predict50_"),
      list(sum = ~sum(.x, na.rm = TRUE)),
      .names = "{.fn}_{.col}"
    ),
    n_bt = n_distinct(doc_id[if_any(starts_with("top1_predict50_"), ~ .x == 1)]),
    # Inventor output metrics
    out_pp = mean(outside_citations),
    cit_pp = mean(forward_citations),
    num_pat = n_distinct(doc_id),
    # Affiliations
    univ_aff = any(university == 1)
  )

# Re-merge with patent data to get BT city/year/class and
# also merge with inventor-year location panel
# (Probs want to take inventor characteristics pre-breakthrough, 
# but then don't know what to do for those without.)

merged_inventor_dta <- inventor_cov %>%
  #Many-to-many, as we have multiple inventors on patents on multiple
  #classes per patent
  left_join(top_patents %>% 
              pivot_longer(cols = starts_with("top1_predict50_",),
                           names_to = "bt_class",
                           names_prefix = "top1_predict50_",
                           values_to = "in_bt_class") %>%
              filter(in_bt_class) %>% 
              left_join(inventor_location_dta  %>% select(inventor_id, patent_id),
                        by = c("doc_id" = "patent_id")),
            by = c("inventor_id")
            ) %>%
  #Inventors making many patents to inventors living across years
  left_join(inventor_year %>% select(inventor_id, year, city),
            by = "inventor_id")

# 8/27 --------------------------------------------------------------------

#Assignee time!!!
# Curious about: breakthrough, type of firm, entrant or not
# On locations, only consider where patent location and assignee location converge
# Or...just use inventor location.

# Type of firm is all set in merge_master (university, VC-backed, founding year,
# publicly listed)

#Comparison 1:
# Breakthrough firm: would like to see a comparison between firms that got a
# breakthrough (in a city) vs. those that didn't. (Marx gets the original assignee.)

merge_master %>%
  select()
  

#Comparison 2:
# See, within a city, who contributes to patents--entrants, incumbents, breakthrough firm.
# Also note responsiveness across types of firms