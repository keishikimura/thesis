library(tidyverse)

# First differentiate by trends of incumbent patents v.
# trends of entrant patents.

# Merge patent pre-post to code that produces time series.
merge_master_plus <- merge_master %>%
  left_join(patent_pre_post, by = c("doc_id" = "patent_id"))

city_type <- "msa"
method = 1
treatment_year = 1999 
dta_all_merged_excl = merge_master_plus
pre_periods = 9
citation_var = "count_24"

# Function begins here ----------------------------------------------------
  
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
      year_sum = sum(group_year_sum),
      inc_sum = sum(group_year_inc),
      ent_sum = sum(group_year_ent),
      mig_sum = sum(group_year_mig)
    ) %>%
    left_join(dta_all_merged_excl %>%
                group_by(year) %>%
                summarize(count = n()), by = "year") %>%
    mutate(
      year_share = year_sum/count,
      inc_share = inc_sum/count,
      ent_share = ent_sum/count,
      mig_share = mig_sum/count
    )
  
  dta_excl_plot_long <- dta_excl_plot %>%
    select(rank_group, year, inc_share, ent_share, mig_share,
           inc_sum, ent_sum, mig_sum) %>%
    pivot_longer(
      cols = c(inc_share, ent_share, mig_share,
               inc_sum, ent_sum, mig_sum),
      names_to = c("type", "stat"),
      values_to = "value",
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(
      period = if_else(year <= 1999, "pre", "post")
    ) %>%
    mutate(
      type = recode(type,
                    "ent" = "Entrant",
                    "inc" = "Incumbent",
                    "mig" = "Migrant"),
      rank_group = recode(as.character(rank_group),
                          "0" = "Bottom 5",
                          "1" = "Top 5")
    )
  
  p <-  dta_excl_plot_long %>%
    filter(stat == "share") %>%
    ggplot(aes(x = year, y = value, color = rank_group, linetype = type)) +
    geom_line() +
    geom_vline(xintercept = treatment_year) +
    geom_vline(xintercept = treatment_year - pre_periods) +
    labs(
      x = "Year",
      y = "Share of AI patents",
      color = "City Sample",
      linetype = "Inventor Type"
    ) +
    scale_color_manual(values = c("Top 5" = "#1f77b4", "Bottom 5" = "#d62728"),
                       breaks = c("Top 5", "Bottom 5")) +
    scale_x_continuous(limits = c(1974, 2017),
                       expand = expansion(mult = 0, add = 0)) +
    theme_classic(base_size = 14)
  
  p_sum <-  dta_excl_plot_long %>%
    filter(stat == "sum") %>%
    ggplot(aes(x = year, y = value, color = rank_group, linetype = type)) +
    geom_line() +
    geom_vline(xintercept = treatment_year) +
    geom_vline(xintercept = treatment_year - pre_periods) +
    labs(
      x = "Year",
      y = "AI Patent Count",
      color = "City Sample",
      linetype = "Inventor Type"
    ) +
    scale_color_manual(values = c("Top 5" = "#1f77b4", "Bottom 5" = "#d62728"),
                       breaks = c("Top 5", "Bottom 5")) +
    scale_x_continuous(limits = c(1974, 2017),
                       expand = expansion(mult = 0, add = 0)) +
    theme_classic(base_size = 14)

# Function ends here ------------------------------------------------------


# inventor_prep excerpt begins here ---------------------------------------
  
  city_type <- "MSA"
  city_var <- paste0(city_type, ".Code")
  title_var <- paste0(city_type, ".Title")
  
  inventor_MSA_cw <- inventor_location_dta %>%
    ##Link with MSAs
    ##(NAs on many Japanese inventors registered in Ebeye island???)
    mutate(
      fips = as.numeric(paste0(state_fips, county_fips))
    ) %>%
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
    )

# unique inventor time series ---------------------------------------------
  city_type <- "msa"
  class_vec <- c("nlp","kr","planning","hardware","vision", "speech", "ml")
  class_var_vec <- paste0(class_vec, "_indic")
  city_var <- paste0(city_type, "_inventor")
  cityname_var <- paste0(city_type, "name_inventor")
  
  merge_master_inventor <- merge_master %>%
    left_join(inventor_pre_post, by = c("doc_id" = "patent_id"))
  
  combined_helper_inventor <- function(df, class_var, treatment_year, pre_periods, 
                              city_var, cityname_var, method){
    bt_dta <- by_class_helper(df, class_var, treatment_year, pre_periods, 
                              city_var, cityname_var, method, 
                              citation_var = citation_var)
    out <- by_year_helper_inventor(df, bt_dta, class_var, city_var, cityname_var)
    
    return(out)
  }
  
  dta_excl <- lapply(class_var_vec, function(x) combined_helper_inventor(merge_master_inventor,
                                                                x, treatment_year, pre_periods, 
                                                                city_var, cityname_var, method)) %>%
    Map(function(df, class){
      df %>% 
        rename_with(.fn = ~paste0(., "_", class),
                    .cols = -c("rank_group", "year5"))
    }, ., class_vec) %>%
    reduce(full_join, by = c("rank_group", "year5"))
  
  dta_excl_plot <- dta_excl %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    pivot_longer(
      cols = -c(rank_group, year5),
      names_to = c(".value", "class"),
      names_pattern = "^(.+)_(.+)$"
    ) %>%
    group_by(rank_group, year5) %>%
    summarize(
      year_sum = sum(group_year_inv),
      inc_sum = sum(group_year_inc_inv),
      ent_sum = sum(group_year_ent_inv),
      mig_sum = sum(group_year_mig_inv)
    ) %>%
    left_join(merge_master_inventor %>%
                mutate(year5 = floor((year - 2000)/5)*5 + 2000) %>%
                group_by(year5) %>%
                summarize(count = n_distinct(inventor_id), by = "year5")) %>%
    mutate(
      year_share = year_sum/count,
      inc_share = inc_sum/count,
      ent_share = ent_sum/count,
      mig_share = mig_sum/count
    )
  
  dta_excl_plot_long <- dta_excl_plot %>%
    select(rank_group, year5, inc_share, ent_share, mig_share,
           inc_sum, ent_sum, mig_sum) %>%
    pivot_longer(
      cols = c(inc_share, ent_share, mig_share,
               inc_sum, ent_sum, mig_sum),
      names_to = c("type", "stat"),
      values_to = "value",
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(
      period = if_else(year5 <= 1999, "pre", "post")
    ) %>%
    mutate(
      type = recode(type,
                    "ent" = "Entrant",
                    "inc" = "Incumbent",
                    "mig" = "Migrant"),
      rank_group = recode(as.character(rank_group),
                          "0" = "Bottom 5",
                          "1" = "Top 5")
    )
  
  p <-  dta_excl_plot_long %>%
    filter(stat == "share") %>%
    ggplot(aes(x = year5, y = value, color = rank_group, linetype = type)) +
    geom_line() +
    geom_vline(xintercept = treatment_year) +
    geom_vline(xintercept = treatment_year - pre_periods) +
    labs(
      x = "Year",
      y = "Share of Unique AI Inventors",
      color = "City Sample",
      linetype = "Inventor Type"
    ) +
    scale_color_manual(values = c("Top 5" = "#1f77b4", "Bottom 5" = "#d62728"),
                       breaks = c("Top 5", "Bottom 5")) +
    scale_x_continuous(limits = c(1974, 2017),
                       expand = expansion(mult = 0, add = 0)) +
    theme_classic(base_size = 14)
  
  p_inv_count <-  dta_excl_plot_long %>%
    filter(stat == "sum") %>%
    ggplot(aes(x = year5, y = value, color = rank_group, linetype = type)) +
    geom_line() +
    geom_vline(xintercept = treatment_year) +
    geom_vline(xintercept = treatment_year - pre_periods) +
    labs(
      x = "Year",
      y = "AI Inventor-Class Count",
      color = "City Sample",
      linetype = "Inventor Type"
    ) +
    scale_color_manual(values = c("Top 5" = "#1f77b4", "Bottom 5" = "#d62728"),
                       breaks = c("Top 5", "Bottom 5")) +
    scale_x_continuous(limits = c(1974, 2017),
                       expand = expansion(mult = 0, add = 0)) +
    theme_classic(base_size = 14)
  