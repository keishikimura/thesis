gen_avg <- function(data_all){
  data_all %>%
    filter(class != "any_ai") %>%
    group_by(msa_inventor) %>%
    summarize(
      bt_ratio_mean = mean(bt_ratio),
      bt_share_mean = mean(bt_share),
      univ_index_mean = mean(univ_index),
      gov_index_mean = mean(gov_index),
      darpa_index_mean = mean(darpa_index)
    ) %>%
    return()
}

# Summary stats (all) --------------------------------------------------------
gen_summary_all <- function(data_all){
  ##Averages
  avg <- gen_avg(data_all)
  
  #All cities
  stats_by_growth <- data_all %>%
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
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    save_kable("AI/tables/tab2a.tex")
  
  return("AI/tables/tab2a.tex")
}

# Summary stats (top 25) --------------------------------------------------
gen_summary_top <- function(data, data_all, panel){
  
  avg <- gen_avg(data_all)
  
  pre_total <- data %>%
    filter(year <= 1999 & year >= 1985) %>%
    nrow()
  
  post_total <- data %>%
    filter(year <= 2014 & year >= 2000) %>%
    nrow()
  
  ranked <- data_all %>%
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
  
  ranked <- ranked %>%
    mutate_if(is.numeric, round, 2) %>%
    select(-c(pre_share, post_share)) %>%
    mutate(rank_change = ifelse(rank_change > 0, 
                                paste0("+", as.character(rank_change)), rank_change))
  
  ranked <- ranked[, c("msaname_inventor", "pre_rank", "post_rank", "rank_change", "bt_ratio_mean",
                       "univ_index_mean", "gov_index_mean", "darpa_index_mean")]
  
  if(panel == "A"){
    
    kable(ranked, "latex", booktabs = TRUE, caption = "Descriptive statistics for prominent patenting cities.",
          linesep = "") %>%
      kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
      add_header_above(c(" " = 1, "City Rank" = 3, " " = 4)) %>%
      save_kable("AI/tables/tab1a.tex")
    
    return("AI/tables/tab1a.tex")
    
  } else if(panel == "B"){
    
    kable(top_avg_merged, "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
      add_footnote("Footnote 1", threeparttable = TRUE) %>%
      save_kable("AI/tables/tab1b.tex")
    
    return("AI/tables/tab1b.tex")
  }

}

# Summary stats (tech) ----------------------------------------------------
gen_summary_tech <- function(data, gov_data){
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
  
  univ_tech <- data %>% 
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
    
    gov_index <- data %>%
      filter(.data[[class_var]] == 1) %>%
      left_join(gov_data, by = c("doc_id" = "patent_id")) %>%
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
  
  by_class_helper_test <- by_class_helper_tech(data, "any_ai", 
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
        
        count_dta <- data %>%
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
    
    sample <- lapply(class_vec, function(x) by_class_helper_tech(data, x, 
                                                                 treatment_year, pre_periods,
                                                                 citation_var)) %>%
      bind_rows()
    
    return(sample)
  }
  
  tech_dta <- modeldta_maker_tech(pre_year_vec = c(1985, 1999),
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
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    save_kable("AI/tables/tab2b.tex")
  
  return("AI/tables/tab2b.tex")
}
