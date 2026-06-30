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

run_loo <- function(data,
                    pop_data,
                    gov_data,
                    pre_year_vec = c(1985, 1999),
                    post_year_vec = c(2000, 2014),
                    city_type = "msa",
                    treatment_year = 1999,
                    pre_periods = 9,
                    extra_reg = c("class", "Division")){
  
  dta <- modeldta_maker(data, pop_data, gov_data, pre_year_vec, post_year_vec, city_type,
                        treatment_year, pre_periods, plot_graphs = TRUE,
                        all_cities = FALSE, excl = FALSE, num_cities = 10,
                        method = 1, citation_var = "count_24")
  city_vec <- unique(dta$msaname_inventor)
  
  fits <- lapply(city_vec, function(x) {
    model_dta <- dta %>% 
      filter(class != "any_ai") %>%
      filter(msaname_inventor != x)
    
    model_fit <- executor_empty(model_dta, extra_reg = c("class", "Division",
                                                         "univ_index_scale", "gov_index_scale",
                                                         "darpa_index_scale"))
    
    meta_data <- tibble(
      left_out = x
    )
    
    return(list(model_fit = model_fit, 
                meta_data = meta_data))
  }
  )
  
  results <- map_dfr(fits, function(model) {
    coefs <- dfadjustSE(model[["model_fit"]])[["coefficients"]]
    tibble(
      coefficient = coefs["bt_ratio_std", "Estimate"],
      standard_error = coefs["bt_ratio_std", "Adj. se"]
    ) %>%
      bind_cols(model[["meta_data"]])
    
  }, .id = "model_id")
  
  return(results)
}

gen_loo_fig <- function(data, pop_data, gov_data){
  loo <- run_loo(data, pop_data, gov_data) %>%
    mutate(
      ci_lb = coefficient - 1.96*standard_error,
      ci_ub = coefficient + 1.96*standard_error
    )
  
  p <- ggplot(loo, aes(x=left_out, y=coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin=ci_lb, ymax=ci_ub), width=0.2) +
    coord_flip() +  # Flip coordinates to have studies on the y-axis
    ylab("Effect Size with 95% CI") +
    xlab("Omitted MSA") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave("Results/figures/figC1.png", p)
  
  return("Results/figures/figC1.png")
}

