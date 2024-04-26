library(tidyverse)
library(readr)
library(lubridate)
library(sandwich)
library(lmtest)
library(dfadjust)
library(knitr)
library(kableExtra)
library(scales)
library(stargazer)

# Merge datasets ------------------------------------------------------
citation_dta <- read_csv("AI/data/generated/citation_outside.csv")

cum_citations <- function(lag){
  cit_varname <- paste0("count_", as.character(lag))
  out_varname <- paste0("out_", as.character(lag))
  
  df <- read_csv("AI/data/generated/cum_citations_outside.csv") %>%
    filter(citation_lag == lag) %>%
    select(-citation_lag) %>%
    rename(!!cit_varname := cum_count,
           !!out_varname := cum_outside)
}

citation_cum_4 <- cum_citations(4)
citation_cum_9 <- cum_citations(9)
citation_cum_14 <- cum_citations(14)
citation_cum_18 <- cum_citations(18)
citation_cum_24 <- cum_citations(24)
citation_cum_34 <- cum_citations(34)

dta_csa <- read_csv("AI/data/generated/AIpatentCSA.csv") %>%
  rename(csa_inventor = city_inventor,
         csaname_inventor = cityname_inventor,
         csa_assignee = city_assignee,
         csaname_assignee = cityname_assignee,
         csa_univ = univcity,
         csaname_univ = univcityname,
         in.csa = in.city.x)

dta_cbsa <- read_csv("AI/data/generated/AIpatentCBSA.csv") %>%
  rename(cbsa_inventor = city_inventor,
         cbsaname_inventor = cityname_inventor,
         cbsa_assignee = city_assignee,
         cbsaname_assignee = cityname_assignee,
         cbsa_univ = univcity,
         cbsaname_univ = univcityname,
         in.cbsa = in.city.x) %>%
  select(doc_id, cbsa_inventor, cbsaname_inventor, cbsa_assignee, cbsaname_assignee,
         cbsa_univ, cbsaname_univ, in.cbsa)

dta_msa <- read_csv("AI/data/generated/AIpatentMSA.csv") %>%
  rename(msa_inventor = city_inventor,
         msaname_inventor = cityname_inventor,
         msa_assignee = city_assignee,
         msaname_assignee = cityname_assignee,
         msa_univ = univcity,
         msaname_univ = univcityname,
         in.msa = in.city.x) %>%
  select(doc_id, msa_inventor, msaname_inventor, msa_assignee, msaname_assignee,
         msa_univ, msaname_univ, in.msa)

dta_all <- dta_csa %>%
  left_join(dta_cbsa, by = "doc_id") %>%
  left_join(dta_msa, by = "doc_id")

dta_all_merged <- dta_all %>%
  #Filter out evo
  filter(rowSums(select(., predict50_ml, predict50_nlp, predict50_planning,
                        predict50_kr, predict50_hardware, predict50_speech,
                        predict50_vision) == 0) != 7) %>%
  left_join(citation_dta, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_4, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_9, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_14, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_18, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_24, by = c("doc_id"="citation_patent_id")) %>%
  left_join(citation_cum_34, by = c("doc_id"="citation_patent_id")) %>%
  rename(forward_citations = count,
         outside_citations = sum_outside) %>%
  mutate(
    pub_dt = ymd(pub_dt),
    filing_date = ymd(filing_date),
    #Based on application year for when invention occurs
    year = year(filing_date)
  ) %>%
  filter(
    predict50_any_ai == 1
  ) %>%
  mutate(
    forward_citations = replace_na(forward_citations, 0),
    outside_citations = replace_na(outside_citations, 0),
    count_4 = replace_na(count_4, 0),
    count_9 = replace_na(count_9, 0),
    count_14 = replace_na(count_14, 0),
    count_18 = replace_na(count_18, 0),
    count_24 = replace_na(count_24, 0),
    count_34 = replace_na(count_34, 0),
    out_4 = replace_na(out_4, 0),
    out_14 = replace_na(out_14, 0),
    out_18 = replace_na(out_18, 0),
    out_24 = replace_na(out_24, 0),
    out_34 = replace_na(out_34, 0)
  ) %>%
  #Cut-off years
  filter(
    year >= 1976 & year <= 2015
  )

#Make components mutually exclusive by taking highest p()
dta_all_merged_excl <- dta_all_merged %>%
  rowwise() %>%
  mutate(max_val = max(c(ai_score_ml, ai_score_evo, ai_score_nlp, ai_score_speech,
                         ai_score_vision, ai_score_kr, ai_score_planning, 
                         ai_score_hardware)),
         ml_indic = as.integer(ai_score_ml == max_val),
         evo_indic = as.integer(ai_score_evo == max_val),
         nlp_indic = as.integer(ai_score_nlp == max_val),
         speech_indic = as.integer(ai_score_speech == max_val),
         vision_indic = as.integer(ai_score_vision == max_val),
         kr_indic = as.integer(ai_score_kr == max_val),
         planning_indic = as.integer(ai_score_planning == max_val),
         hardware_indic = as.integer(ai_score_hardware == max_val),
         any_ai_indic = predict50_any_ai
  ) %>%
  ungroup() %>%
  select(-max_val) %>%
  mutate(
    #6 tech clusters; 5 big cities
    tech_cluster = (csa_inventor %in% c("CS488", "CS148", "CS500", "CS216") |  
            msa_inventor %in% c("C4174", "C1242")),
    big_city = (csa_inventor %in% c("CS408", "CS348", "CS176", "CS428",
                                    "CS220")) & !tech_cluster,
    other = !(tech_cluster | big_city),
    key_csa = ifelse((tech_cluster|big_city), 
                      ifelse(msa_inventor %in% c("C4174", "C1242"), 
                             msa_inventor, csa_inventor), 
                      NA)
  )

# University --------------------------------------------------------------
univ_index <- function(treatment_year, pre_periods, class_var, city_var, univcity_var){
  msa_index <- dta_all_merged_excl %>%
    filter(.data[[class_var]] == 1) %>% 
    filter(univ == 1) %>%
    separate_rows(.data[[univcity_var]], sep = ",\\s*") %>%
    filter(grepl("^C[0-9]+$", .data[[univcity_var]])) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    group_by(.data[[univcity_var]]) %>%
    summarize(univ_index = n()) %>%
    rename(!!city_var := univcity_var)
   
  return(msa_index)
}

# Government --------------------------------------------------------------
gov_interest <- read_tsv("AI/data/raw/uspto/g_gov_interest_org.tsv")

gov_index <- function(treatment_year, pre_periods, class_var, city_var){
  
  gov_index <- dta_all_merged_excl %>%
    filter(.data[[class_var]] == 1) %>%
    left_join(gov_interest, by = c("doc_id" = "patent_id")) %>%
    mutate(
      gov = !is.na(fedagency_name),
      darpa = ifelse(is.na(level_two), FALSE, 
                     level_two == "Defense Advanced Research Projects Agency"),
      dod = ifelse(is.na(level_one), FALSE, 
                   level_one == "Department of Defense")
    ) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    group_by(.data[[city_var]]) %>%
    summarize(gov_index = sum(gov),
              darpa_index = sum(darpa),
              dod_index = sum(dod))
  
  return(gov_index)
}

# Summary
# Population --------------------------------------------------------------
pop <- read_csv("AI/data/generated/populationMSA.csv") %>%
  #Manually recode Los Angeles MSA and others
  mutate(msa = paste0("C", as.character(msa/10)),
         msa = ifelse(msa == "C3110", "C3108", msa),
         msa = ifelse(msa == "C4206", "C4220", msa),
         msa = ifelse(msa == "C4694", "C4268", msa))
# Regions -----------------------------------------------------------------
msa_to_regions <- read_csv("AI/data/generated/region_crosswalk.csv") %>%
  select(MSA.Code, Division, Region) %>%
  group_by(MSA.Code) %>%
  slice_head(n=1) %>%
  ungroup()

# Sampling ----------------------------------------------------------------
#Helper function to create samples

by_class_helper <- function(df, class_var, treatment_year, pre_periods, 
                            city_var,cityname_var, method, all_cities = FALSE,
                            num_cities = 10, citation_var = "forward_citations"){
  
  all_citation <- df %>%
    filter(.data[[class_var]] == 1) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    mutate(
      top_1_all = .data[[citation_var]] > quantile(.data[[citation_var]], 0.99)
    ) %>%
    filter(grepl("^C[0-9]+$", .data[[city_var]])) %>%
    group_by(.data[[city_var]], .data[[cityname_var]], tech_cluster, big_city, other,
             key_csa) %>%
    summarize(count = n(),
              count_top_1 = sum(top_1_all)
    ) %>%
    ungroup() %>%
    mutate(
      total_patents = sum(count),
      total_top_1 = sum(count_top_1),
      bt_ratio = (count_top_1/total_top_1)/(count/total_patents),
      bt_share = count_top_1/total_top_1
    )
  
  top_10 <- function(data, all_cities) {
    if (!all_cities) {
      data <- data %>% slice_head(n= num_cities)
    }
    return(data)
  }
  
  rank_group <- function(data, all_cities){
    if(!all_cities){
      data <- data %>%
        mutate(rank_group = c(rep(1, n()/2), rep(0, n()/2)))
    } else{
      data <- data %>%
        mutate(rank_group = 1)
    }
    return(data)
  }
  
  all_citation_arranged <- if(method == 1){
    all_citation %>% arrange(desc(count)) %>%
      #Sample size: 10
      top_10(all_cities) %>%
      arrange(desc(bt_ratio)) %>%
      rank_group(all_cities)
  } else if(method == 2){
    all_citation %>% arrange(desc(bt_ratio)) %>%
      ##Must have at least 10 patents over pre-period
      filter(count >= 10) %>%
      #(Warning): This is MSA-specific
      filter(!grepl("MicroSA", msaname_inventor)) %>%
      #Sample size: 10
      top_10(all_cities) %>%
      rank_group(all_cities)
  } else{
    stop("method not 1 or 2")
  }
  
  return(all_citation_arranged)
}


#Helper function to get yearly shares for rank groups
by_year_helper <- function(main_data, bt_data, class_var, city_var, cityname_var){
  by_year <- main_data %>%
    filter(.data[[class_var]] == 1) %>%
    group_by(.data[[city_var]], .data[[cityname_var]], year) %>%
    summarize(year_count = n()) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(year_sum = sum(year_count),
           year_share = year_count/year_sum) %>%
    left_join(bt_data, by = city_var) %>%
    filter(!is.na(rank_group)) %>%
    group_by(rank_group, year) %>%
    reframe(
      group_year_sum = sum(year_count),
      group_year_share = sum(year_share)) %>%
    mutate(
      rank_group = as.character(rank_group)
    )
  
  return(by_year)
}

#Produces data and plots
by_class <- function(class, treatment_year, pre_periods, city_type, method,
                     plot_graphs = TRUE, all_cities = FALSE, excl = FALSE,
                     num_cities = 10, citation_var = "forward_citations"){
  
  city_var <- paste0(city_type, "_inventor")
  cityname_var <- paste0(city_type, "name_inventor")
  univcity_var <- paste0(city_type, "_univ")
  class_var <- ifelse(excl, paste0(class, "_indic"), 
                      paste0("predict50_", class))
  
  all_citation_arranged <- by_class_helper(dta_all_merged_excl, class_var, treatment_year, pre_periods, 
                                           city_var, cityname_var, method, all_cities, num_cities, citation_var) %>%
    left_join(univ_index(treatment_year, pre_periods, class_var, city_var, univcity_var),
              by = city_var) %>%
    left_join(gov_index(treatment_year, pre_periods, class_var, city_var),
              by = city_var) %>%
    mutate(
      class = class,
      univ_index = if_else(is.na(univ_index), 0 , univ_index),
      gov_index = if_else(is.na(gov_index), 0 , gov_index),
      darpa_index = if_else(is.na(darpa_index), 0 , darpa_index),
      dod_index = if_else(is.na(dod_index), 0 , dod_index)
    ) %>%
    mutate(
      across(.cols = c(univ_index, gov_index, darpa_index, dod_index), .fns = scale, .names = "{.col}_scale")
    )
  
  if(plot_graphs){
    by_year <- by_year_helper(dta_all_merged_excl, all_citation_arranged, class_var,
                              city_var, cityname_var)
    
    n_btpos <- all_citation_arranged %>%
      filter(bt_ratio != 0) %>%
      nrow()
    
    p <- by_year %>% ggplot(aes(x = year, y = group_year_share, color = rank_group)) +
      geom_line() +
      geom_vline(xintercept = treatment_year) +
      geom_vline(xintercept = treatment_year - pre_periods) +
      labs(
        title = paste(class, city_type),
        subtitle = paste("method", as.character(method), "n=", as.character(n_btpos))
      )
    
    print(p)
  }

  return(all_citation_arranged)
}



#Patent count data + graphs
any_ai <- by_class("any_ai",1999, 4, "msa",1)
# nlp <- by_class("nlp", 1984, 5, "msa", 1)
# kr <- by_class("kr",1984, 5, "msa", 1)
# planning <- by_class("planning",1984, 5, "msa", 1)
# hardware <- by_class("hardware",1984, 5, "msa", 1)
# vision <- by_class("vision",1984, 5, "msa", 1)
# speech <- by_class("speech",1984, 5, "msa", 1)
# ml <- by_class("ml",1984, 5, "msa", 1)

# Graphical ---------------------------------------------------------------
year_totals_excl <- dta_all_merged_excl %>%
  group_by(year) %>%
  summarize(count = n())

graphical <- function(treatment_year, pre_periods, city_type, method, citation_var){
  class_vec <- c("nlp","kr","planning","hardware","vision", "speech", "ml")
  class_var_vec <- paste0(class_vec, "_indic")
  city_var <- paste0(city_type, "_inventor")
  cityname_var <- paste0(city_type, "name_inventor")
  
  combined_helper <- function(df, class_var, treatment_year, pre_periods, 
                              city_var, cityname_var, method){
    
    bt_dta <- by_class_helper(df, class_var, treatment_year, pre_periods, 
                              city_var, cityname_var, method, 
                              citation_var = citation_var)
    out <- by_year_helper(df, bt_dta, class_var, city_var, cityname_var)
    
    return(out)
  }
  
  dta_excl <- lapply(class_var_vec, function(x) combined_helper(dta_all_merged_excl,
                                              x, treatment_year, pre_periods, 
                                              city_var, cityname_var, method)) %>%
    Map(function(df, class){
      df %>% 
        select(-group_year_share) %>%
        rename_with(.fn = ~paste0(., "_", class),
                         .cols = -c("rank_group", "year"))
        }, ., class_vec) %>%
    reduce(full_join, by = c("rank_group", "year"))
  
  dta_excl_plot <- dta_excl %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    pivot_longer(
      cols = -c(rank_group, year),
      names_to = c(".value", "class"),
      names_pattern = "^(.+)_(.+)$"
    ) %>%
    group_by(rank_group, year) %>%
    summarize(
      year_sum = sum(group_year_sum)
    ) %>%
    left_join(year_totals_excl, by = "year") %>%
    mutate(
      year_share = year_sum/count
    )
  
  df_dotted <- dta_excl_plot[dta_excl_plot$year <= treatment_year - pre_periods , ]
  df_solid <- dta_excl_plot[dta_excl_plot$year >= treatment_year - pre_periods, ]
  
  p <- ggplot() +
    geom_line(data = df_dotted, aes(x = year, y = year_share, color = rank_group), linetype= "dotted") + 
    geom_line(data = df_solid, aes(x = year, y = year_share, color = rank_group), linetype = "solid") + 
    geom_vline(xintercept = treatment_year) +
    geom_vline(xintercept = treatment_year - pre_periods) +
    labs(
      x = "Year",
      y = "Share of AI patents",
      color = "Top 5?",
    ) +
    xlim(c(1974, 2017)) +
    ylim(c(0, 0.45))
  
  print(p)
  
  return(dta_excl)
}

graphical(1999, 9, "msa", 1, citation_var = "count_24")

graphical(1994, 9, "msa", 1, citation_var = "count_14")
graphical(1999, 9, "msa", 1, citation_var = "count_14")
graphical(2004, 9, "msa", 1, citation_var = "count_14")
graphical(2009, 9, "msa", 1, citation_var = "count_14")

graphical(1994, 9, "msa", 1, citation_var = "count_9")
graphical(1999, 9, "msa", 1, citation_var = "count_9")
graphical(2004, 9, "msa", 1, citation_var = "count_9")
graphical(2009, 9, "msa", 1, citation_var = "count_9")

graphical(1994, 9, "msa", 1, citation_var = "count_4")
graphical(1999, 9, "msa", 1, citation_var = "count_4")
graphical(2004, 9, "msa", 1, citation_var = "count_4")
graphical(2009, 9, "msa", 1, citation_var = "count_4")

# Main Model -------------------------------------------------------------------
modeldta_maker <- function(pre_year_vec, post_year_vec, city_type,
                           treatment_year, pre_periods, plot_graphs = TRUE,
                           all_cities = FALSE, excl = FALSE, num_cities = 10,
                           method = 1, citation_var = "forward_citations"){
  
  city_var <- paste0(city_type, "_inventor")
  cityname_var <- paste0(city_type, "name_inventor")
  class_vec <- c("any_ai","nlp","kr","planning","hardware","vision", "speech", "ml")
  
  count <- function(year_vec){
    
    start_year <- year_vec[1]
    end_year <- year_vec[2]
    
    count_helper <- function(start_year, end_year, class){
      class_var <- ifelse(excl, paste0(class, "_indic"), 
                          paste0("predict50_", class))
      count_var <- paste0("count_", class)
      
      count_dta <- dta_all_merged_excl %>%
        filter(.data[[class_var]] == 1) %>%
        filter(year <= end_year & year >= start_year) %>%
        group_by(.data[[city_var]]) %>%
        summarize(!!count_var := n())
      
      return(count_dta)
    }
    
    df <- lapply(class_vec, function(x) count_helper(start_year, end_year, x)) %>%
      reduce(full_join, by = city_var)
    
    return(df)
  }
  
  pre <- count(pre_year_vec) %>%
    pivot_longer(cols = starts_with("count_"), names_to = "class", values_to = "count", 
                 names_prefix = "count_") %>%
    rename(pre_count = count)
  
  post <- count(post_year_vec) %>%
    pivot_longer(cols = starts_with("count_"), names_to = "class", values_to = "count", 
                 names_prefix = "count_") %>%
    rename(post_count = count)
  
  sample <- lapply(class_vec, function(x) by_class(x, treatment_year, pre_periods, city_type,
                                                   method, plot_graphs, all_cities, excl,
                                                   num_cities = num_cities, citation_var)) %>%
    bind_rows()
  
  merged <- sample %>%
    left_join(pre, by = c("msa_inventor", "class")) %>%
    left_join(post, by = c("msa_inventor", "class")) %>%
    left_join(pop, by = c("msa_inventor" = "msa")) %>%
    left_join(msa_to_regions, by = c("msa_inventor" = "MSA.Code")) %>%
    mutate(
      logdiff = log(post_count) - log(pre_count),
      logpre = log(pre_count),
      #Can change years for these
      logpop = log(pop2000),
      logpopdiff = log(pop2000) - log(pop1990)
    ) %>%
    group_by(class) %>%
    mutate(
      bt_ratio_std = scale(bt_ratio)
    ) %>%
    ungroup()
  
  return(merged)
}

executor <- function(pre_year_vec, post_year_vec, city_type = "msa",
                     treatment_year, pre_periods, extra_reg = "",
                     plot_graphs = TRUE, all_cities = FALSE,
                     share = FALSE, excl = FALSE, num_cities = 10,
                     nopop = FALSE, method = 1, citation_var = "forward_citations"){
  dta <- modeldta_maker(pre_year_vec, post_year_vec, city_type,
                        treatment_year, pre_periods, plot_graphs,
                        all_cities, excl, num_cities, method, citation_var) %>%
    filter(class != "any_ai")
  
  bt_reg <- ifelse(!share, "bt_ratio_std", "bt_share")
  
  default_reg <- if(!nopop){
    c(bt_reg, "logpop", "logpopdiff", "logpre")
  } else {
    bt_reg
  }
  
  if(all(extra_reg != "")) {
    f <- paste("logdiff ~", paste(c(default_reg, extra_reg), collapse = " + "))
  } else {
    f <- paste("logdiff ~", paste(c(default_reg), collapse = " + "))
  }
  
  fit <- lm(as.formula(f), data = dta)
  return(fit)
}

# Model Execution ---------------------------------------------------------
#Main (20)
fit_20_1999 <- executor(pre_year_vec = c(1990, 1999),
                        post_year_vec = c(2000, 2009),
                        city_type = "msa",
                        treatment_year = 1999,
                        pre_periods = 9,
                        extra_reg = c("class", "Division"))
robust_se <- vcovHC(fit_20_1999, type = "HC1")
coeftest(fit_20_1999, vcov. = robust_se)

#Main (30)
fit_30_1999 <- executor(pre_year_vec = c(1985, 1999),
                        post_year_vec = c(2000, 2014),
                        city_type = "msa",
                        treatment_year = 1999,
                        pre_periods = 9,
                        extra_reg =  c("class", "Division"))
summary(fit_30_1999)
dfadjustSE(fit_30_1999)

fit_30_1999_data <- modeldta_maker(pre_year_vec = c(1985, 1999),
                                            post_year_vec = c(2000, 2014),
                                            city_type = "msa",
                                            treatment_year = 1999,
                                            pre_periods = 9) %>%
  filter(class != "any_ai")

fit_30_1999_data$msaname_inventor %>% unique()
fit_30_1999_data %>% 
  group_by(msaname_inventor) %>%
  summarize(count= n(),
            sum_top_1 = sum(count_top_1))

#Main (30; Share)
fit_30_1999_share <- executor(pre_year_vec = c(1985, 1999),
                              post_year_vec = c(2000, 2014),
                              city_type = "msa",
                              treatment_year = 1999,
                              pre_periods = 9,
                              extra_reg = c("class", "Division"),
                              share = TRUE)
summary(fit_30_1999_share)
dfadjustSE(fit_30_1999_share)

#Univ + gov
fit_30_1999_add <- executor(pre_year_vec = c(1985, 1999),
                           post_year_vec = c(2000, 2014),
                           city_type = "msa",
                           treatment_year = 1999,
                           pre_periods = 9,
                           extra_reg = c("class", "Division", "univ_index_scale", "gov_index_scale", "darpa_index_scale"))
summary(fit_30_1999_add)
dfadjustSE(fit_30_1999_add)

fit_30_1999_add_share <- executor(pre_year_vec = c(1985, 1999),
                                  post_year_vec = c(2000, 2014),
                                  city_type = "msa",
                                  treatment_year = 1999,
                                  pre_periods = 9,
                                  extra_reg = c("class", "Division", "univ_index_scale", 
                                                "gov_index_scale", "darpa_index_scale"),
                                  share = TRUE)
summary(fit_30_1999_add_share)
dfadjustSE(fit_30_1999_add_share)


# Robustness checks -------------------------------------------------------
#All cities
model_dta_all <- modeldta_maker(pre_year_vec = c(1985, 1999),
                                post_year_vec = c(2000, 2014),
                                city_type = "msa",
                                treatment_year = 1999,
                                pre_periods = 9,
                                all_cities = TRUE,
                                citation_var = "count_24") %>%
  filter(!grepl("MicroSA", msaname_inventor)) %>%
  #Filtering out small cities without matching population
  #and cities without post period patenting (in interest of time)
  filter(!is.na(logpop) & !is.na(logdiff)) %>%
  #Must have at least 5 patents
  filter(count >= 10) %>%
  #Re-standardize
  group_by(class) %>%
  mutate(bt_ratio_std = scale(bt_ratio)) %>%
  ungroup()

fit_30_1999_all <- lm(logdiff ~ bt_ratio_std + logpop + logpopdiff + logpre + class + Division + univ_index_scale + gov_index_scale + darpa_index_scale,
                      data = model_dta_all %>% filter(class != "any_ai"))
summary(fit_30_1999_all)
dfadjustSE(fit_30_1999_all)

#Mutually exclusive
fit_30_1999_excl <- executor(pre_year_vec = c(1985, 1999),
                                 post_year_vec = c(2000, 2014),
                                 city_type = "msa",
                                 treatment_year = 1999,
                                 pre_periods = 9,
                                 extra_reg = c("class", "Division", "univ_index_scale", 
                                               "gov_index_scale", "darpa_index_scale"),
                                 excl = TRUE)
summary(fit_30_1999_excl)
dfadjustSE(fit_30_1999_excl)

#Region by technology
fit_30_1999_rxt <- executor(pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          extra_reg = c("class", "Division", "Division:class",
                                        "univ_index_scale", "gov_index_scale", "darpa_index_scale"),
                          num_cities = 26)
rxt_dta <- modeldta_maker(pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          num_cities = 26)
dfadjustSE(fit_30_1999_rxt)

#Effects wiped out with city FE; some city FE give NAs
fit_city_fe <- executor(pre_year_vec = c(1985, 1999),
                     post_year_vec = c(2000, 2014),
                     city_type = "msa",
                     treatment_year = 1999,
                     pre_periods = 9,
                     extra_reg = c("logpre", "class", "msa_inventor",
                                   "univ_index_scale", "gov_index_scale", "darpa_index_scale"),
                     nopop = TRUE,
                     num_cities = 10
                     )
dfadjustSE(fit_city_fe)

#Outside
fit_outside <- executor(pre_year_vec = c(1985, 1999),
                    post_year_vec = c(2000, 2014),
                    city_type = "msa",
                    treatment_year = 1999,
                    pre_periods = 9,
                    extra_reg = c("class", "Division", "univ_index_scale", 
                                  "gov_index_scale", "darpa_index_scale"),
                    num_cities = 10,
                    citation_var = "out_24"
                    )
dfadjustSE(fit_outside)

#Time window
fit_24 <- executor(pre_year_vec = c(1985, 1999),
                   post_year_vec = c(2000, 2014),
                   city_type = "msa",
                   treatment_year = 1999,
                   pre_periods = 9,
                   extra_reg = c("class", "Division", "univ_index_scale", 
                                "gov_index_scale", "darpa_index_scale"),
                   num_cities = 10,
                   citation_var = "count_24"
                     )
dfadjustSE(fit_24)

stargazer(fit_30_1999_add, fit_24, type = "latex",
          title = "Main Regression Results",
          label = "table:main",
          covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                               "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
          omit = c("Constant", "^class", "^Division"),
          omit.stat = c("LL", "ser", "f", "rsq"),
          add.lines = list(c("Class FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Division FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Class x Division FE", "No", "No", "No", "No"),
                           c("City FE", "No", "No", "No", "No")),
          column.labels = c("(1)", "(2)"),
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          intercept.bottom = FALSE,
          digits = 5)

# Second Sampling Method --------------------------------------------------
fit_30_1999_2 <- executor(pre_year_vec = c(1985, 1999),
                        post_year_vec = c(2000, 2014),
                        city_type = "msa",
                        treatment_year = 1999,
                        pre_periods = 9,
                        method = 2,
                        extra_reg =  c("class", "Division"))
fit_30_1999_2_dta <- modeldta_maker(pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          method = 2)
summary(fit_30_1999_2)
dfadjustSE(fit_30_1999_2)

fit_30_1999_2_add <- executor(pre_year_vec = c(1985, 1999),
                        post_year_vec = c(2000, 2014),
                        city_type = "msa",
                        treatment_year = 1999,
                        pre_periods = 9,
                        method = 2,
                        extra_reg =  c("class", "Division", "univ_index", "gov_index", "darpa_index"))
summary(fit_30_1999_2_add)
dfadjustSE(fit_30_1999_2_add)

# LOO ---------------------------------------------------------------------
executor_empty <- function(dta, extra_reg = "", nopop = FALSE, share = FALSE){
  bt_reg <- ifelse(!share, "bt_ratio_std", "bt_share")
  
  default_reg <- if(!nopop){
    c(bt_reg, "logpop", "logpopdiff", "logpre")
  } else {
    bt_reg
  }
  
  if(all(extra_reg != "")) {
    f <- paste("logdiff ~", paste(c(default_reg, extra_reg), collapse = " + "))
  } else {
    f <- paste("logdiff ~", paste(c(default_reg), collapse = " + "))
  }
  
  fit <- lm(as.formula(f), data = dta)
  return(fit)
}

loo_test <- function(pre_year_vec = c(1985, 1999),
                     post_year_vec = c(2000, 2014),
                     city_type = "msa",
                     treatment_year = 1999,
                     pre_periods = 9,
                     extra_reg = c("class", "Division")){
  
  dta <- modeldta_maker(pre_year_vec, post_year_vec, city_type,
                 treatment_year, pre_periods, plot_graphs = TRUE,
                 all_cities = FALSE, excl = FALSE, num_cities = 10,
                 method = 1)
  city_vec <- unique(dta$msaname_inventor)
  
  fits <- lapply(city_vec, function(x) {
    model_dta <- dta %>% 
      filter(class != "any_ai") %>%
      filter(msaname_inventor != x)
    
    model_fit <- executor_empty(model_dta, extra_reg = c("class", "Division",
                                                         "univ_index", "gov_index",
                                                         "darpa_index"))
    
    meta_data <- tibble(
      left_out = x
    )
    
    return(list(model_fit = model_fit, 
                meta_data = meta_data))
  }
  )
  
  results <- map_dfr(fits, function(model) {
    coefs <- summary(model[["model_fit"]])$coefficients
    
    tibble(
      coefficient = coefs["bt_ratio_std", "Estimate"],
      standard_error = coefs["bt_ratio_std", "Std. Error"],
      t_value = coefs["bt_ratio_std", "t value"],
      p_value = coefs["bt_ratio_std", "Pr(>|t|)"]
    ) %>%
      bind_cols(model[["meta_data"]])
    
  }, .id = "model_id")
  
  return(results)
}

loo <- loo_test()

# Summary stats (all) --------------------------------------------------------

##Averages
avg <- model_dta_all %>%
  filter(class != "any_ai") %>%
  group_by(msa_inventor) %>%
  summarize(
    bt_ratio_mean = mean(bt_ratio),
    bt_share_mean = mean(bt_share),
    univ_index_mean = mean(univ_index),
    gov_index_mean = mean(gov_index),
    darpa_index_mean = mean(darpa_index)
  )

#All cities
stats_by_growth <- model_dta_all %>%
  filter(class == "any_ai") %>%
  select(msa_inventor, msaname_inventor, logdiff) %>%
  right_join(avg, by = "msa_inventor") %>%
  mutate(
    quintile = cut(logdiff, 
                   breaks = quantile(logdiff, probs = seq(0, 1, by = 0.2)), 
                   include.lowest = TRUE, 
                   labels = FALSE),
    median = cut(logdiff, 
                 breaks = quantile(logdiff, probs = seq(0, 1, by = 0.5)), 
                 include.lowest = TRUE, 
                 labels = FALSE)
  )

stats_quintile <- stats_by_growth %>%
  select(-c(msaname_inventor, msa_inventor, median, logdiff)) %>%
  group_by(quintile) %>%
  summarise_all(mean)  %>%
  mutate(
    type = as.character(quintile)
  ) %>%
  select(-quintile)

stats_median <- stats_by_growth %>%
  select(-c(msaname_inventor, msa_inventor, quintile, logdiff)) %>%
  group_by(median) %>%
  summarise_all(mean) %>%
  mutate(
    type = as.character(median)
  ) %>%
  select(-median)

stats_all <- stats_by_growth %>%
  select(-c(msaname_inventor, msa_inventor, quintile, median, logdiff)) %>%
  summarise_all(mean) %>%
  mutate(
    type = "All cities"
  )

stats_merged <- rbind(stats_all, stats_median, stats_quintile)

stats_merged <- stats_merged[, c("type", "bt_ratio_mean",
                                 "univ_index_mean", "gov_index_mean", "darpa_index_mean")] %>% 
  mutate_if(is.numeric, round, 2)

kable(stats_merged, "latex", booktabs = TRUE, caption = "Descriptive statistics for all cities.",
      linesep = "") %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))

# Summary stats (top 25) --------------------------------------------------
pre_total <- dta_all_merged_excl %>%
  filter(year <= 1999 & year >= 1985) %>%
  nrow()

post_total <- dta_all_merged_excl %>%
  filter(year <= 2014 & year >= 2000) %>%
  nrow()

ranked <- model_dta_all %>%
  left_join(avg, by = "msa_inventor") %>%
  filter(class == "any_ai") %>%
  select(msaname_inventor, bt_ratio_mean, bt_share_mean, univ_index_mean,
         gov_index_mean, darpa_index_mean, pre_count, post_count) %>%
  mutate(
    pre_rank = rank(-pre_count),
    post_rank = rank(-post_count),
    rank_change = pre_rank - post_rank,
    pre_share = pre_count/pre_total,
    post_share = post_count/post_total
  ) %>%
  arrange(post_rank) %>%
  slice_head(n=25)

top_avg <- ranked %>%
  select(-c(msaname_inventor, pre_rank, post_rank, rank_change, bt_share_mean,
            pre_count, post_count, pre_share, post_share)) %>% 
  summarise_all(mean)

top_avg_noks <- ranked %>%
  filter(msaname_inventor != "Kansas City, MO-KS MSA") %>%
  select(-c(msaname_inventor, pre_rank, post_rank, rank_change, bt_share_mean,
            pre_count, post_count, pre_share, post_share)) %>% 
  summarise_all(mean)

top_up_avg <- ranked %>%
  filter(rank_change > 0) %>%
  select(-c(msaname_inventor, pre_rank, post_rank, rank_change, bt_share_mean,
            pre_count, post_count, pre_share, post_share)) %>% 
  summarise_all(mean)

top_up_avg_noks <- ranked %>%
  filter(rank_change > 0) %>%
  filter(msaname_inventor != "Kansas City, MO-KS MSA") %>%
  select(-c(msaname_inventor, pre_rank, post_rank, rank_change, bt_share_mean,
            pre_count, post_count, pre_share, post_share)) %>% 
  summarise_all(mean)

top_down_avg <- ranked %>%
  filter(rank_change < 0) %>%
  select(-c(msaname_inventor, pre_rank, post_rank, rank_change, bt_share_mean,
            pre_count, post_count, pre_share, post_share)) %>% 
  summarise_all(mean)

top_avg_merged <- rbind(top_avg, top_up_avg, top_down_avg, top_avg_noks, top_up_avg_noks) %>%
  mutate_if(is.numeric, round, 2)

# ranked <- ranked %>%
#   mutate_if(is.numeric, round, 2) %>%
#   mutate(pre_rankshare = paste(pre_rank, " (", pre_share, ")", sep=""),
#          post_rankshare = paste(post_rank, " (", post_share, ")", sep="")) %>%
#   select(-c(pre_rank, pre_share, post_rank, post_share))

ranked <- ranked %>%
  mutate_if(is.numeric, round, 2) %>%
  select(-c(pre_share, post_share)) %>%
  mutate(rank_change = ifelse(rank_change > 0, 
                              paste0("+", as.character(rank_change)), rank_change))

ranked <- ranked[, c("msaname_inventor", "pre_rank", "post_rank", "rank_change", "bt_ratio_mean",
                     "univ_index_mean", "gov_index_mean", "darpa_index_mean")]
  

kable(ranked, "latex", booktabs = TRUE, caption = "Descriptive statistics for prominent patenting cities.",
      linesep = "") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  add_header_above(c(" " = 1, "City Rank" = 3, " " = 4))

kable(top_avg_merged, "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  add_footnote("Footnote 1", threeparttable = TRUE)


# Summary stats (tech) ----------------------------------------------------

# List of strings to detect
tech_csa <- c("CS488", "CS148", "CS500", "CS216")
tech_msa <- c("C4174", "C1242")
big_csa <- c("CS408", "CS348", "CS176", "CS428", "CS220")

# Function to check for matches using grepl
find_matches <- function(text, patterns) {
  # Check if any pattern is found in the text
  matches <- sapply(patterns, function(pattern) grepl(pattern, text))
  # Return only the matching patterns
  paste(patterns[matches], collapse = ", ")
}

univ_tech <- dta_all_merged_excl %>% 
  filter(univ == 1) %>%
  mutate(matches = pmap_chr(list(csa_univ, msa_univ), function(c1, c2) {
        # Find matches in each column with different search strings
        matches1 <- find_matches(c1, c(tech_csa, big_csa))
        matches2 <- find_matches(c2, tech_msa)
        # Combine and keep unique matches from both columns
        unique_matches <- unique(c(matches1, matches2))
        # Filter out empty strings and join with commas
        matches_string <- paste(unique_matches[unique_matches != ""], collapse = ", ")
        # Return an empty string if no matches are found
        ifelse(matches_string == "", NA, matches_string)
        })
        )
  
univ_index_tech <- function(treatment_year, pre_periods, class_var){
  msa_index <- univ_tech %>%
    filter(.data[[class_var]] == 1) %>%
    separate_rows(matches, sep = ",\\s*") %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    group_by(matches) %>%
    summarize(univ_index = n()) %>%
    rename(key_csa = matches)
  
  return(msa_index)
}

gov_index_tech <- function(treatment_year, pre_periods, class_var){
  
  gov_index <- dta_all_merged_excl %>%
    filter(.data[[class_var]] == 1) %>%
    left_join(gov_interest, by = c("doc_id" = "patent_id")) %>%
    mutate(
      gov = !is.na(fedagency_name),
      darpa = ifelse(is.na(level_two), FALSE, 
                     level_two == "Defense Advanced Research Projects Agency"),
      dod = ifelse(is.na(level_one), FALSE, 
                   level_one == "Department of Defense")
    ) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    filter(tech_cluster | big_city) %>%
    group_by(tech_cluster, big_city, key_csa) %>%
    summarize(gov_index = sum(gov),
              darpa_index = sum(darpa),
              dod_index = sum(dod)) %>%
    ungroup() %>%
    select(-c(tech_cluster, big_city))
  
  return(gov_index)
}

# Summary
by_class_helper_tech <- function(df, class, treatment_year, pre_periods, 
                                 citation_var = "forward_citations"){
  
  class_var <- paste0("predict50_", class)
  
  all_citation <- df %>%
    filter(.data[[class_var]] == 1) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    mutate(
      top_1_all = .data[[citation_var]] > quantile(.data[[citation_var]], 0.99)
    ) %>%
    filter(tech_cluster | big_city) %>%
    group_by(tech_cluster, big_city, key_csa) %>%
    summarize(count = n(),
              count_top_1 = sum(top_1_all)
    ) %>%
    ungroup() %>%
    mutate(
      total_patents = sum(count),
      total_top_1 = sum(count_top_1),
      bt_ratio = (count_top_1/total_top_1)/(count/total_patents),
      bt_share = count_top_1/total_top_1,
      class = class
    ) %>%
    left_join(univ_index_tech(treatment_year, pre_periods, class_var),
              by = "key_csa") %>%
    left_join(gov_index_tech(treatment_year, pre_periods, class_var),
              by = "key_csa") %>%
    mutate(
      univ_index = if_else(is.na(univ_index), 0 , univ_index),
      gov_index = if_else(is.na(gov_index), 0 , gov_index),
      darpa_index = if_else(is.na(darpa_index), 0 , darpa_index),
      dod_index = if_else(is.na(dod_index), 0 , dod_index)
    )
  
  return(all_citation)
}

by_class_helper_test <- by_class_helper_tech(dta_all_merged_excl, "any_ai", 
                                             treatment_year = 1989, pre_periods = 9)

modeldta_maker_tech <- function(pre_year_vec, post_year_vec, city_type,
                           treatment_year, pre_periods,
                           citation_var = "forward_citations"){
  
  
  class_vec <- c("any_ai","nlp","kr","planning","hardware","vision", "speech", "ml")
  
  count <- function(year_vec){
    
    start_year <- year_vec[1]
    end_year <- year_vec[2]
    
    count_helper <- function(start_year, end_year, class){
      class_var <- ifelse(excl, paste0(class, "_indic"), 
                          paste0("predict50_", class))
      count_var <- paste0("count_", class)
      
      count_dta <- dta_all_merged_excl %>%
        filter(tech_cluster | big_city)
        filter(.data[[class_var]] == 1) %>%
        filter(year <= end_year & year >= start_year) %>%
        group_by(key_csa) %>%
        summarize(!!count_var := n())
      
      return(count_dta)
    }
    
    df <- lapply(class_vec, function(x) count_helper(start_year, end_year, x)) %>%
      reduce(full_join, by = city_var)
    
    return(df)
  }
  
  sample <- lapply(class_vec, function(x) by_class_helper_tech(dta_all_merged_excl, x, 
                                                               treatment_year, pre_periods,
                                                               citation_var)) %>%
    bind_rows()
  
  return(sample)
}

tech_dta <-modeldta_maker_tech(pre_year_vec = c(1985, 1999),
                                post_year_vec = c(2000, 2014),
                                treatment_year = 1999,
                                pre_periods = 9,
                                citation_var = "count_24")

tech_dta_summary <- tech_dta %>%
  filter(class != "any_ai") %>%
  group_by(tech_cluster) %>%
  summarize(bt_ratio_mean = mean(bt_ratio),
            univ_index_mean = mean(univ_index),
            gov_index_mean = mean(gov_index),
            darpa_index_mean = mean(darpa_index)
            ) %>% 
  mutate_if(is.numeric, round, 2)

kable(tech_dta_summary, "latex", booktabs = TRUE, caption = "Descriptive statistics for tech cities.",
      linesep = "") %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))

# Co-occurrence ------------------------------------------------------------


# class_vec <- c("nlp","kr","planning","hardware","vision", "speech", "ml")
# class_var_vec <- paste0("predict50_", class_vec)
# 
# co_occurrence_matrix <- dta_all_merged_excl %>% 
#   select(all_of(class_var_vec)) %>%
#   mutate(id = row_number()) %>% 
#   pivot_longer(cols = -id, names_to = "category", values_to = "value") %>% 
#   filter(value == 1) %>% 
#   select(-value) %>% 
#   inner_join(., ., by = "id") %>% 
#   count(name.x, name.y) %>%
#   pivot_wider(names_from = name.y, values_from = n, values_fill = list(n = 0))
# 
# # Adjusting the matrix to be symmetrical
# co_occurrence_matrix <- co_occurrence_matrix %>%
#   select(-name.x) %>%
#   as.matrix() %>%
#   { . + t(.) } %>%
#   replace(1:7, diag(.) / 2)  # Fixing diagonal to avoid double count
# 
# # Plotting the matrix as a heatmap
# ggplot(melt(co_occurrence_matrix), aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "blue") +
#   labs(x = "Category", y = "Category", fill = "Number of Patents") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Shifting sample --------------------------------------------------------
shift_sample <- function(sample_length, pre_periods, citation_var = "forward_citations"){
  init_year <- c(1976:(2015-sample_length+1))
  
  fits <- lapply(init_year, function(x) {
    pre_start <- x
    pre_end <- x + sample_length/2 - 1
    post_start <- x+sample_length/2
    post_end <- x + sample_length - 1
    treat_start <- x + sample_length/2 - 1 - pre_periods
    treat_end <- x + sample_length/2 - 1
    
    model_fit <- executor(pre_year_vec = c(pre_start, pre_end),
                          post_year_vec = c(post_start, post_end),
                          treatment_year = treat_end,
                          pre_periods = pre_periods,
                          extra_reg = c("class", "Division"),
                          plot_graphs = FALSE,
                          citation_var = citation_var
                          )
    
    year_data <- tibble(
      pre_start = pre_start,
      pre_end = pre_end,
      post_start = post_start,
      post_end = post_end,
      treat_start = treat_start,
      treat_end = treat_end,
      pre_periods = pre_periods,
      sample_length = sample_length
    )
    
    return(list(model_fit = model_fit, 
                year_data = year_data))
  }
  )
  
  results <- map_dfr(fits, function(model) {
    coefs <- dfadjustSE(model[["model_fit"]])[["coefficients"]]
    
    tibble(
      coefficient = coefs["bt_ratio_std", "Estimate"],
      standard_error = coefs["bt_ratio_std", "Adj. se"]
    ) %>%
      bind_cols(model[["year_data"]])
    
  }, .id = "model_id")
  
  return(results)
}
results_30_24 <- shift_sample(30, 9, "count_24")
results_20_18 <- shift_sample(20, 9, "count_18")
results_30 <- shift_sample(30, 9)
results_20 <- shift_sample(20, 9)

results_shift_merged <- rbind(results_30, results_20)

ggplot(results_20, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient") + 
  scale_x_continuous(breaks= pretty_breaks())

ggplot(results_30, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")  + 
  scale_x_continuous(breaks= pretty_breaks())

ggplot(results_20_18, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient") + 
  scale_x_continuous(breaks= pretty_breaks())

ggplot(results_30_24, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")  + 
  scale_x_continuous(breaks= pretty_breaks())

ggplot(results_shift_merged, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  facet_grid(vars(pre_periods)) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")

# Shifting post-period ----------------------------------------------------
shift_post <- function(treatment_year, increment, pre_periods, 
                       citation_var = "forward_citations"){
  treat_start <- treatment_year - pre_periods
  treat_end <- treatment_year 

  post_start_vec <- c((treatment_year+1):(2015-increment/2+1))
  
  fits <- lapply(post_start_vec, function(x) {
    pre_start <- x - increment/2
    pre_end <- x - 1
    post_start <- x
    post_end <- x + increment/2 - 1
    
    model_fit <- executor(pre_year_vec = c(pre_start, pre_end),
                          post_year_vec = c(post_start, post_end),
                          treatment_year = treat_end,
                          pre_periods = pre_periods,
                          plot_graphs = FALSE,
                          extra_reg = c("class", "Division"),
                          citation_var = citation_var)
    
    year_data <- tibble(
      pre_start = pre_start,
      pre_end = pre_end,
      post_start = post_start,
      post_end = post_end,
      treat_start = treat_start,
      treat_end = treat_end,
      pre_periods = pre_periods,
      increment = increment
    )
    
    return(list(model_fit = model_fit, 
                year_data = year_data))
    }
  )
  
  results <- map_dfr(fits, function(model) {
    coefs <- dfadjustSE(model[["model_fit"]])[["coefficients"]]
    
    tibble(
      coefficient = coefs["bt_ratio_std", "Estimate"],
      standard_error = coefs["bt_ratio_std", "Adj. se"]
    ) %>%
      bind_cols(model[["year_data"]])
    
  }, .id = "model_id")
  
  return(results)
}

shift_20_1990 <- shift_post(treatment_year = 1989,
                         increment = 20,
                         pre_periods = 9)
shift_30_1990 <- shift_post(treatment_year = 1989,
                            increment = 30,
                            pre_periods = 9)

shift_20_1990_34 <- shift_post(treatment_year = 1989,
                            increment = 20,
                            pre_periods = 9,
                            citation_var = "count_34")
shift_30_1990_34 <- shift_post(treatment_year = 1989,
                            increment = 30,
                            pre_periods = 9,
                            citation_var = "count_34")

shift_post_merged <- rbind(shift_20_1990, shift_30_1990)

ggplot(shift_20_1990, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")

ggplot(shift_30_1990, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")

ggplot(shift_20_1990_34, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")

ggplot(shift_30_1990_34, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  labs(x = "Post-period start year",
       y = "Coefficient")

ggplot(shift_post_merged, aes(x = post_start, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                    ymax = coefficient + 1.96* standard_error), width = 0.2) +
  theme_minimal() +
  facet_grid(vars(increment)) + 
  labs(x = "Post-period start year",
       y = "Coefficient")
