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
             key_csa, Division) %>%
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
by_class <- function(data, gov_data, class, treatment_year, pre_periods, city_type, method,
                     plot_graphs = TRUE, all_cities = FALSE, excl = FALSE,
                     num_cities = 10, citation_var = "forward_citations"){
  
  city_var <- paste0(city_type, "_inventor")
  cityname_var <- paste0(city_type, "name_inventor")
  univcity_var <- paste0(city_type, "_univ")
  class_var <- ifelse(excl, paste0(class, "_indic"), 
                      paste0("predict50_", class))
  
  all_citation_arranged <- by_class_helper(data, class_var, treatment_year, pre_periods, 
                                           city_var, cityname_var, method, all_cities, num_cities, citation_var) %>%
    left_join(univ_index(data, treatment_year, pre_periods, class_var, city_var, univcity_var),
              by = city_var) %>%
    left_join(gov_index(data, gov_data, treatment_year, pre_periods, class_var, city_var),
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
    by_year <- by_year_helper(data, all_citation_arranged, class_var,
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