gen_timeseries <- function(data){

  count_by_year <- function(df){
    class_vec <- c("any_ai","nlp","kr","planning","hardware","vision", "speech", "ml")
    
    count_helper <- function(class){
      class_var <- paste0("predict50_", class)
      
      count_dta <- df %>%
        filter(.data[[class_var]] == 1) %>%
        group_by(app_year) %>%
        summarize(count = n()) %>%
        mutate(type = class)
      
      return(count_dta)
    }
    
    result <- lapply(class_vec, function(x) count_helper(x)) %>%
      bind_rows()
    
    return(result)
  }
  
  df <- data %>%
    mutate(
      pub_dt = ymd(pub_dt),
      year = year(pub_dt),
      filing_date = ymd(filing_date),
      app_year = year(filing_date)
    ) %>%
    filter(
      app_year >= 1976 & app_year <= 2015
    )
  
  by_year_counts <- count_by_year(df) %>%
    mutate(
      type = case_when(
        type == "any_ai" ~ "All",
        type == "hardware" ~ "AI Hardware",
        type == "kr" ~ "Knowledge Processing",
        type == "ml" ~ "ML",
        type == "nlp" ~ "NLP",
        type == "planning" ~ "Planning/Control",
        type == "speech" ~ "Speech",
        type == "vision" ~ "Computer Vision"
      ),
      type = factor(type, levels = c(
        "All","AI Hardware","Knowledge Processing","ML","NLP","Planning/Control","Speech","Computer Vision"
      ))
    )
  
  pal <- c(
    "All" = "black",
    "Planning/Control" = "#e6ab02",
    "Knowledge Processing" = "#7570b3",
    "AI Hardware" = "#1b9e77",
    "Computer Vision" = "#d95f02",
    "ML" = "#e7298a",
    "NLP" = "#66a61e",
    "Speech" = "#a6761d"
  )
  
  ggplot(by_year_counts, aes(app_year, count, color = type,
                             linewidth = type == "All")) +
    geom_line() +
    scale_color_manual(values = pal, breaks = names(pal)) +  # "All" will be first
    scale_linewidth_manual(values = c("TRUE" = 0.8, "FALSE" = 0.6), guide = "none") +
    labs(y = "Patent Count", x = "Application Year", color = "AI Component") +
    scale_y_continuous(labels = label_comma()) +
    theme_classic(base_size = 14)
  
  ggsave("Results/figures/fig1.png", width = 10, height = 6, units = "in")
  
  return("Results/figures/fig1.png")
}
