#Main (30)
gen_main_table <- function(data, pop_data, gov_data){
  fit_30_1999 <- executor(data,
                          pop_data,
                          gov_data,
                          pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          extra_reg =  c("class", "Division"),
                          citation_var = "count_24")
  df_main1 <- dfadjustSE(fit_30_1999)[["coefficients"]][, "Adj. se"]
  
  fit_30_1999_data <- modeldta_maker(data,
                                     pop_data,
                                     gov_data,
                                     pre_year_vec = c(1985, 1999),
                                     post_year_vec = c(2000, 2014),
                                     city_type = "msa",
                                     treatment_year = 1999,
                                     pre_periods = 9,
                                     citation_var = "count_24") %>%
    filter(class != "any_ai")
  
  fit_30_1999_data$msaname_inventor %>% unique()
  fit_30_1999_data %>% 
    group_by(msaname_inventor) %>%
    summarize(count= n(),
              sum_top_1 = sum(count_top_1))
  
  #Univ + gov
  fit_30_1999_add <- executor(data,
                              pop_data,
                              gov_data,
                              pre_year_vec = c(1985, 1999),
                              post_year_vec = c(2000, 2014),
                              city_type = "msa",
                              treatment_year = 1999,
                              pre_periods = 9,
                              extra_reg = c("class", "Division", "univ_index_scale", 
                                            "gov_index_scale", "darpa_index_scale"),
                              citation_var = "count_24")
  df_main2 <- dfadjustSE(fit_30_1999_add)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_30_1999, fit_30_1999_add, type = "latex",
            title = "Main Regression Results",
            label = "table:main",
            covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
            se = list(df_main1, df_main2),
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
            digits = 4,
            out = "AI/tables/tab3.tex")
  
  return("AI/tables/tab3.tex")
}

#Main (30; Share)
gen_share_table <- function(data, pop_data, gov_data){
  
  
  fit_30_1999_share <- executor(data,
                                pop_data,
                                gov_data,
                                pre_year_vec = c(1985, 1999),
                                post_year_vec = c(2000, 2014),
                                city_type = "msa",
                                treatment_year = 1999,
                                pre_periods = 9,
                                extra_reg = c("class", "Division"),
                                citation_var = "count_24",
                                share = TRUE)
  dfshare1 <- dfadjustSE(fit_30_1999_share)[["coefficients"]][, "Adj. se"]
  
  fit_30_1999_add_share <- executor(data,
                                    pop_data,
                                    gov_data,
                                    pre_year_vec = c(1985, 1999),
                                    post_year_vec = c(2000, 2014),
                                    city_type = "msa",
                                    treatment_year = 1999,
                                    pre_periods = 9,
                                    extra_reg = c("class", "Division", "univ_index_scale", 
                                                  "gov_index_scale", "darpa_index_scale"),
                                    citation_var = "count_24",
                                    share = TRUE)
  dfshare2 <- dfadjustSE(fit_30_1999_add_share)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_30_1999_share, fit_30_1999_add_share, type = "latex",
            title = "Breakthrough Shares",
            label = "table:main",
            covariate.labels = c("Breakthrough share", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
            se = list(dfshare1, dfshare2),
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
            digits = 4,
            out = "AI/tables/tabC1.tex")
  
  return("AI/tables/tabC1.tex")
}

#Diff citation windows
gen_window_table <- function(data, pop_data, gov_data){
  
  msa_counter <- function(citation_var){
    data <- modeldta_maker(data,
                           pop_data, gov_data,
                           pre_year_vec = c(1985, 1999),
                           post_year_vec = c(2000, 2014),
                           city_type = "msa",
                           treatment_year = 1999,
                           pre_periods = 9,
                           citation_var = citation_var) %>%
      filter(class != "any_ai")
    
    return(data$msaname_inventor %>% unique() %>% length())
  }
  
  num_msas_cit <- lapply(c("forward_citations",
                           "count_14",
                           "count_9",
                           "count_4"), msa_counter)
  
  fit_citall <- executor(data,
                         pop_data, gov_data,
                         pre_year_vec = c(1985, 1999),
                         post_year_vec = c(2000, 2014),
                         city_type = "msa",
                         treatment_year = 1999,
                         pre_periods = 9,
                         extra_reg =  c("class", "Division", "univ_index_scale", 
                                        "gov_index_scale", "darpa_index_scale"),
                         citation_var = "forward_citations")
  df_citall <- dfadjustSE(fit_citall)[["coefficients"]][, "Adj. se"]
  
  
  fit_cit4 <- executor(data,
                       pop_data, gov_data,
                       pre_year_vec = c(1985, 1999),
                       post_year_vec = c(2000, 2014),
                       city_type = "msa",
                       treatment_year = 1999,
                       pre_periods = 9,
                       extra_reg =  c("class", "Division", "univ_index_scale", 
                                      "gov_index_scale", "darpa_index_scale"),
                       citation_var = "count_4")
  df_cit4 <- dfadjustSE(fit_cit4)[["coefficients"]][, "Adj. se"]
  
  fit_cit9 <- executor(data,
                       pop_data, gov_data,
                       pre_year_vec = c(1985, 1999),
                       post_year_vec = c(2000, 2014),
                       city_type = "msa",
                       treatment_year = 1999,
                       pre_periods = 9,
                       extra_reg =  c("class", "Division", "univ_index_scale", 
                                      "gov_index_scale", "darpa_index_scale"),
                       citation_var = "count_9")
  df_cit9 <- dfadjustSE(fit_cit9)[["coefficients"]][, "Adj. se"]
  
  fit_cit14 <- executor(data,
                        pop_data, gov_data,
                        pre_year_vec = c(1985, 1999),
                        post_year_vec = c(2000, 2014),
                        city_type = "msa",
                        treatment_year = 1999,
                        pre_periods = 9,
                        extra_reg =  c("class", "Division", "univ_index_scale", 
                                       "gov_index_scale", "darpa_index_scale"),
                        citation_var = "count_14")
  df_cit14 <- dfadjustSE(fit_cit14)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_citall, fit_cit14, fit_cit9, fit_cit4,
            type = "latex",
            title = "Varying Citation Time Windows",
            label = "table:main",
            covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
            se = list(df_citall, df_cit14, df_cit9, df_cit4),
            omit = c("Constant", "^class", "^Division"),
            omit.stat = c("LL", "ser", "f", "rsq"),
            add.lines = list(c("Class FE", "Yes", "Yes", "Yes", "Yes"),
                             c("Division FE", "Yes", "Yes", "Yes", "Yes"),
                             c("Class x Division FE", "No", "No", "No", "No"),
                             c("City FE", "No", "No", "No", "No")),
            column.labels = c("Present", "15 years", "10 years", "5 years"),
            dep.var.caption = "",
            dep.var.labels.include = FALSE,
            intercept.bottom = FALSE,
            digits = 4,
            out = "AI/tables/tabC2.tex")
  
  return("AI/tables/tabC2.tex")
}

#Robustness checks
gen_robustness_table <- function(data, pop_data, gov_data){
  #Mutually exclusive
  fit_30_1999_excl <- executor(data,
                               pop_data, gov_data,
                               pre_year_vec = c(1985, 1999),
                               post_year_vec = c(2000, 2014),
                               city_type = "msa",
                               treatment_year = 1999,
                               pre_periods = 9,
                               extra_reg = c("class", "Division", "univ_index_scale", 
                                             "gov_index_scale", "darpa_index_scale"),
                               excl = TRUE,
                               citation_var = "count_24"
  )
  df_excl <- dfadjustSE(fit_30_1999_excl)[["coefficients"]][, "Adj. se"]
  excl_dta <- modeldta_maker(data,
                             pop_data, gov_data,
                             pre_year_vec = c(1985, 1999),
                             post_year_vec = c(2000, 2014),
                             city_type = "msa",
                             treatment_year = 1999,
                             pre_periods = 9,
                             num_cities = 10,
                             excl = TRUE,
                             citation_var = "count_24")
  
  #Region by technology
  fit_30_1999_rxt <- executor(data,
                              pop_data, gov_data,
                              pre_year_vec = c(1985, 1999),
                              post_year_vec = c(2000, 2014),
                              city_type = "msa",
                              treatment_year = 1999,
                              pre_periods = 9,
                              extra_reg = c("class", "Division", "Division:class",
                                            "univ_index_scale", "gov_index_scale", "darpa_index_scale"),
                              num_cities = 26,
                              citation_var = "count_24")
  rxt_dta <- modeldta_maker(data,
                            pop_data, gov_data,
                            pre_year_vec = c(1985, 1999),
                            post_year_vec = c(2000, 2014),
                            city_type = "msa",
                            treatment_year = 1999,
                            pre_periods = 9,
                            num_cities = 26,
                            citation_var = "count_24")
  df_rxt <- dfadjustSE(fit_30_1999_rxt)[["coefficients"]][, "Adj. se"]
  
  #Effects wiped out with city FE
  fit_city_fe <- executor(data,
                          pop_data, gov_data,
                          pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          extra_reg = c("logpre", "class", "msa_inventor",
                                        "univ_index_scale", "gov_index_scale", "darpa_index_scale"),
                          nopop = TRUE,
                          num_cities = 10,
                          citation_var = "count_24"
  )
  df_cityfe <- dfadjustSE(fit_city_fe)[["coefficients"]][, "Adj. se"]
  
  
  #Outside
  fit_outside <- executor(data,
                          pop_data, gov_data,
                          pre_year_vec = c(1985, 1999),
                          post_year_vec = c(2000, 2014),
                          city_type = "msa",
                          treatment_year = 1999,
                          pre_periods = 9,
                          extra_reg = c("class", "Division", "univ_index_scale", 
                                        "gov_index_scale", "darpa_index_scale"),
                          num_cities = 10,
                          citation_var = "out_24"
  )
  df_outside <- dfadjustSE(fit_outside)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_30_1999_excl, fit_30_1999_rxt, fit_city_fe, fit_outside,
            type = "latex",
            title = "Robustness Checks",
            label = "table:main",
            covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
            se = list(df_excl, df_rxt, df_cityfe, df_outside),
            omit = c("Constant", "^class", "^Division", "^msa"),
            omit.stat = c("LL", "ser", "f", "rsq"),
            add.lines = list(c("Class FE", "Yes", "Yes", "Yes", "Yes"),
                             c("Division FE", "Yes", "Yes", "Yes", "Yes"),
                             c("Class x Division FE", "No", "No", "No", "No"),
                             c("City FE", "No", "No", "No", "No")),
            column.labels = c("Mut. excl.", "CxD", "City FE", "Outside Citations"),
            dep.var.caption = "",
            dep.var.labels.include = FALSE,
            intercept.bottom = FALSE,
            digits = 4,
            out = "AI/tables/tab4.tex")
  
  return("AI/tables/tab4.tex")
}

#Second Sampling Method
gen_altsample_table <- function(data, data_all, pop_data, gov_data){
  
  fit_30_1999_all <- lm(logdiff ~ bt_ratio_std + logpop + logpopdiff + logpre + class + 
                          Division + univ_index_scale + gov_index_scale + darpa_index_scale,
                        data = data_all %>% filter(class != "any_ai"))
  df_all <- dfadjustSE(fit_30_1999_all)[["coefficients"]][, "Adj. se"]
  
  #Second sampling method
  fit_30_1999_2 <- executor(data,
                            pop_data,
                            gov_data,
                            pre_year_vec = c(1985, 1999),
                            post_year_vec = c(2000, 2014),
                            city_type = "msa",
                            treatment_year = 1999,
                            pre_periods = 9,
                            method = 2,
                            extra_reg =  c("class", "Division"),
                            citation_var = "count_24")
  fit_30_1999_2_dta <- modeldta_maker(data,
                                      pop_data,
                                      gov_data,
                                      pre_year_vec = c(1985, 1999),
                                      post_year_vec = c(2000, 2014),
                                      city_type = "msa",
                                      treatment_year = 1999,
                                      pre_periods = 9,
                                      method = 2,
                                      citation_var = "count_24")
  df_2 <- dfadjustSE(fit_30_1999_2)[["coefficients"]][, "Adj. se"]
  
  #Second sampling method w additional
  fit_30_1999_2_add <- executor(data,
                                pop_data,
                                gov_data,
                                pre_year_vec = c(1985, 1999),
                                post_year_vec = c(2000, 2014),
                                city_type = "msa",
                                treatment_year = 1999,
                                pre_periods = 9,
                                method = 2,
                                extra_reg =  c("class", "Division", "univ_index_scale", "gov_index_scale", 
                                               "darpa_index_scale"),
                                citation_var = "count_24")
  df_2add <- dfadjustSE(fit_30_1999_2_add)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_30_1999_2, fit_30_1999_2_add, fit_30_1999_all,
            type = "latex",
            title = "Other Sampling Methods",
            label = "table:main",
            covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest"),
            se = list(df_2, df_2add, df_all),
            omit = c("Constant", "^class", "^Division", "^msa"),
            omit.stat = c("LL", "ser", "f", "rsq"),
            add.lines = list(c("Class FE", "Yes", "Yes", "Yes"),
                             c("Division FE", "Yes", "Yes", "Yes"),
                             c("Class x Division FE", "No", "No", "No"),
                             c("City FE", "No", "No", "No")),
            dep.var.caption = "",
            dep.var.labels.include = FALSE,
            intercept.bottom = FALSE,
            digits = 4,
            out = "AI/tables/tab5.tex")
  
  return("AI/tables/tab5.tex")
}

#Add tech cluster FE
gen_tech_table <- function(data, pop_data, gov_data){
  
  fit_30_1999_tech <- executor(data,
                               pop_data, gov_data,
                               pre_year_vec = c(1985, 1999),
                               post_year_vec = c(2000, 2014),
                               city_type = "msa",
                               treatment_year = 1999,
                               pre_periods = 9,
                               method = 1,
                               extra_reg =  c("class", "Division", "univ_index_scale", 
                                              "gov_index_scale", "darpa_index_scale", "tech_cluster"),
                               citation_var = "count_24")
  df_tech <- dfadjustSE(fit_30_1999_tech)[["coefficients"]][, "Adj. se"]
  
  stargazer(fit_30_1999_tech,
            type = "latex",
            title = "Tech Cluster Fixed Effect",
            label = "table:main",
            covariate.labels = c("Breakthrough ratio", "Log population", "Log pop. growth", 
                                 "Log count", "Univ. Strength", "Gov. Interest", "DARPA Interest",
                                 "Tech Cluster FE"),
            se = list(df_tech),
            omit = c("Constant", "^class", "^Division", "^msa"),
            omit.stat = c("LL", "ser", "f", "rsq"),
            add.lines = list(c("Class FE", "Yes"),
                             c("Division FE", "Yes"),
                             c("Class x Division FE", "No"),
                             c("City FE", "No")),
            dep.var.caption = "",
            dep.var.labels.include = FALSE,
            intercept.bottom = FALSE,
            digits = 4,
            out = "AI/tables/tabC3.tex")
  
  return("AI/tables/tabC3.tex")
}
