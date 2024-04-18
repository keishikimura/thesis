library(tidyverse)
library(readxl)

population <- read_csv("AI/data/raw/census/tab05b.csv", 
                       skip = 10) %>%
  slice_head(n = 396) %>%
  slice_tail(n = 391)

names(population)<-make.names(names(population),unique = TRUE)

pop_cleaned <- population %>%
  filter(is.na(Metro.Div..Code)) %>%
  rename(msa = Metro..Micro.Area.Code,
         msaname = Metropolitan.Statistical.Area.Metropolitan.Division.Micropolitan.Statistical.Area,
         pop2000 = Population,
         pop1990 = ...7
  ) %>%
  select(msa, msaname, pop2000, pop1990)

write_csv(pop_cleaned, "AI/data/generated/populationMSA.csv")  
