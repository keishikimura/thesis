library(tidyverse)

msa2013_dta <- read_csv("AI/data/raw/nber/cbsatocountycrosswalk.csv") %>%
  filter(!is.na(y2013)) %>%
  select(-ssacounty) %>%
  unique()

csa2013_dta <- read_excel("AI/data/raw/census/qcew-county-msa-csa-crosswalk.xlsx",
                          sheet = 3)

regions_dta <- read_csv("AI/data/raw/census/regions.csv")
names(regions_dta) <- make.names(names(regions_dta),unique = TRUE)

names(csa2013_dta)<-make.names(names(csa2013_dta),unique = TRUE)

csa_dta <- msa2013_dta %>%
  mutate(
    fipscounty = as.numeric(fipscounty)
  ) %>%
  left_join(csa2013_dta, by = c("fipscounty"="County.Code")) %>%
  rename(
    CBSA.Code = cbsa,
    CBSA.Title = cbsaname
  ) %>%
  left_join(regions_dta, by = c("state" = "State.Code"))

regions_cleaned <- csa_dta %>%
  select(MSA.Code, CBSA.Code, CSA.Code, State, Region, Division)

write_csv(regions_cleaned, "AI/data/generated/region_crosswalk.csv")
