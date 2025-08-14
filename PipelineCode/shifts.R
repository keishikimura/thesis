# Shifting sample --------------------------------------------------------
shift_sample_specs <- tribble(
  ~sample_length, ~pre_periods, ~citation_var,  ~fig_path,
  30,            9,           "count_24",     "AI/figures/fig3a.png",
  20,          9,             "count_18",     "AI/figures/fig3b.png"
)


shift_sample <- function(data, pop_data, gov_data,sample_length, pre_periods, citation_var = "forward_citations",
                         filepath){
  init_year <- c(1976:(2015-sample_length+1))
  
  fits <- lapply(init_year, function(x) {
    pre_start <- x
    pre_end <- x + sample_length/2 - 1
    post_start <- x+sample_length/2
    post_end <- x + sample_length - 1
    treat_start <- x + sample_length/2 - 1 - pre_periods
    treat_end <- x + sample_length/2 - 1
    
    model_fit <- executor(data,
                          pop_data, gov_data,
                          pre_year_vec = c(pre_start, pre_end),
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
  
  p <- ggplot(results, aes(x = post_start, y = coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                      ymax = coefficient + 1.96* standard_error), width = 0.2) +
    theme_minimal() +
    labs(x = "Post-period start year",
         y = "Coefficient")  + 
    scale_x_continuous(breaks= pretty_breaks()) +
    ylim(-0.22, 0.3) 
  
  ggsave(filepath, p)
  
  return(filepath)
}

# Shifting post-period ----------------------------------------------------
shift_post_specs <- tribble(
  ~treatment_year, ~increment, ~pre_periods, ~citation_var,  ~fig_path,
  1989,             20,         9,           "count_34",     "AI/figures/fig4a.png",
  1989,             30,         9,           "count_34",     "AI/figures/fig4b.png",
)

shift_post <- function(data, pop_data, gov_data, treatment_year, increment, pre_periods, 
                       citation_var = "forward_citations", filepath){
  treat_start <- treatment_year - pre_periods
  treat_end <- treatment_year 
  
  post_start_vec <- c((treatment_year+1):(2015-increment/2+1))
  
  fits <- lapply(post_start_vec, function(x) {
    pre_start <- x - increment/2
    pre_end <- x - 1
    post_start <- x
    post_end <- x + increment/2 - 1
    
    model_fit <- executor(data,
                          pop_data, 
                          gov_data,
                          pre_year_vec = c(pre_start, pre_end),
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
  
  p <- ggplot(results, aes(x = post_start, y = coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - 1.96* standard_error, 
                      ymax = coefficient + 1.96* standard_error), width = 0.2) +
    theme_minimal() +
    labs(x = "Post-period start year",
         y = "Coefficient") +
    ylim(-0.4, 0.3)
  
  ggsave(filepath, p)
  
  return(filepath)
}