library(tidyverse)
library(lubridate)

dta_ai <- read.csv("AI/data/generated/AIpatentCBSA.csv") %>%
  mutate(
    pub_dt = ymd(pub_dt),
    year = year(pub_dt),
    trend = year - 1987
  ) %>%
  filter(
      predict50_any_ai == 1
    )

by_msa <- dta_ai %>%
  group_by(cbsa_inventor, cbsaname_inventor, year, trend) %>%
  summarize(count = n()) %>%
  filter(cbsaname_inventor != "")

msa_index <- dta_ai %>%
  filter(univ == 1) %>%
  filter(year < 1987 & year > 1977) %>%
  group_by(cbsa_inventor, cbsaname_inventor) %>%
  summarize(count = n()) %>%
  filter(cbsaname_inventor != "") %>%
  rename(index = count)

msa_index_all <- by_msa %>%
  left_join(msa_index, by = "cbsa_inventor") %>%
  mutate(
    index = if_else(is.na(index), 0 , index)
  ) %>%
  group_by(cbsa_inventor) %>%
  summarize(mean_index = mean(index)) %>%
  mutate(std_index = scale(mean_index))

fit_dta <- by_msa %>%
  left_join(msa_index_all, by = "cbsa_inventor") %>%
  mutate(
    post1987 = year >= 1987
  )

fit_dta_1997 <- by_msa %>%
  left_join(msa_index_all, by = "cbsa_inventor") %>%
  mutate(
    post1987 = year >= 1987
  ) %>%
  filter(year <= 1997)

fit <- lm(log(1+count) ~ std_index*trend*post1987 + factor(cbsa_inventor), 
          data = fit_dta)

fit_basic <- lm(log(1+count) ~ std_index*post1987 + factor(cbsa_inventor) + factor(year), 
                data = fit_dta)

fit_1997 <- lm(log(1+count) ~ std_index*trend*post1987 + factor(cbsa_inventor), 
          data = fit_dta_1997)

fit_basic_1997 <- lm(log(1+count) ~ std_index*post1987 + factor(cbsa_inventor) + factor(year), 
                data = fit_dta_1997)

# next steps:
# (0) summary statistics: plot time series, Ellison-Glaeser, etc.
# (a) current model:
# (a).-i. use d(log(count)); get address of university contributor; 
#         perhaps measure industry-university connection by # patents co-created
#         construct index of citation-measured local knowledge spillovers?
# (a).i. add controls: population, all patents, software patents (why does kerr use log population? log patents?)
# (a).ii. why are coefficients not negative for time trend? where's the "shock"?
# (a).iii. get rid of university patents in outcome variable
# (a).iv. use only expert systems-related inventions
# (b) robustness:
# (b).i. use paper counts for index, weighted by citations
# (b).ii. use citation-weighted patents
# (b).iii. time width for index
# (b).iv. long panel vs. short panel
# (b).v. use CSAs instead of CBSAs
# (b).vi. pre-trend graph
# (b).vii. RD on 1987?
# (c) mechanisms
# (c).i. show universities decline *less* than industry
# (c).ii. measure any increases in c(inventor, productivity, h-index, citations,
#                                    funding, expenditures), opposite for failed
# (d) theory: Jaffe knowledge production function
# (d).i. with universities and industry with a shock on industry

#Akcigit:
#Variation in university AI research funding as a shock
#Modeling: basic research
