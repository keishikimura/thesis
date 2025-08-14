modeldta_maker <- function(data, pop_data, gov_data, pre_year_vec, post_year_vec, city_type,
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
      
      count_dta <- data %>%
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
  
  sample <- lapply(class_vec, function(x) by_class(data, gov_data, x, treatment_year, pre_periods, city_type,
                                                   method, plot_graphs, all_cities, excl,
                                                   num_cities = num_cities, citation_var)) %>%
    bind_rows()
  
  merged <- sample %>%
    left_join(pre, by = c("msa_inventor", "class")) %>%
    left_join(post, by = c("msa_inventor", "class")) %>%
    left_join(pop_data, by = c("msa_inventor" = "msa")) %>%
    #left_join(region_data, by = c("msa_inventor" = "MSA.Code")) %>%
    mutate(
      logdiff = log(post_count) - log(pre_count),
      logpre = log(pre_count),
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

executor <- function(data, pop_data, gov_data, pre_year_vec, post_year_vec, city_type = "msa",
                     treatment_year, pre_periods, extra_reg = "",
                     plot_graphs = TRUE, all_cities = FALSE,
                     share = FALSE, excl = FALSE, num_cities = 10,
                     nopop = FALSE, method = 1, citation_var = "forward_citations"){
  dta <- modeldta_maker(data, pop_data, gov_data, pre_year_vec, post_year_vec, city_type,
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

#All cities
gen_model_dta_all <- function(data, pop_data, gov_data){
  modeldta_maker(data,
                 pop_data,
                 gov_data,
                 pre_year_vec = c(1985, 1999),
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
    ungroup() %>%
    return()
}
