library(tibble)

graphical_specs <- tribble(
  ~treatment_year, ~citation_var,        ~fig_path,
  1999,            "count_24",           "AI/figures/fig2.png",
  1994,            "forward_citations",  "AI/figures/graphical/figB1a.png",
  1999,            "forward_citations",  "AI/figures/graphical/figB1b.png",
  2004,            "forward_citations",  "AI/figures/graphical/figB1c.png",
  2009,            "forward_citations",  "AI/figures/graphical/figB1d.png",
  1994,            "count_14",           "AI/figures/graphical/figB2a.png",
  1999,            "count_14",           "AI/figures/graphical/figB2b.png",
  2004,            "count_14",           "AI/figures/graphical/figB2c.png",
  2009,            "count_14",           "AI/figures/graphical/figB2d.png",
  1994,            "count_9",            "AI/figures/graphical/figB3a.png",
  1999,            "count_9",            "AI/figures/graphical/figB3b.png",
  2004,            "count_9",            "AI/figures/graphical/figB3c.png",
  2009,            "count_9",            "AI/figures/graphical/figB3d.png",
  1994,            "count_4",            "AI/figures/graphical/figB4a.png",
  1999,            "count_4",            "AI/figures/graphical/figB4b.png",
  2004,            "count_4",            "AI/figures/graphical/figB4c.png",
  2009,            "count_4",            "AI/figures/graphical/figB4d.png"
)

graphical <- function(dta_all_merged_excl, treatment_year, pre_periods, city_type, 
                      method, citation_var, fig_path){
  
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
    left_join(dta_all_merged_excl %>%
                group_by(year) %>%
                summarize(count = n()), by = "year") %>%
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
  
  ggsave(
    filename = fig_path,
    plot = p,
    width = 8,
    height = 5
  )
  
  return(fig_path)
}