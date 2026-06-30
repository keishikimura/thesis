#Helper function to get yearly shares for rank groups
by_year_helper <- function(main_data, bt_data, class_var, city_var, cityname_var){
  by_year <- main_data %>%
    filter(.data[[class_var]] == 1) %>%
    group_by(.data[[city_var]], .data[[cityname_var]], year) %>%
    summarize(year_count = n(),
              incumbent_count = sum(migrant_status == "incumbent"),
              entrant_count = sum(migrant_status == "entrant"),
              migrant_count = sum(migrant_status == "migrant")
    ) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(year_sum = sum(year_count),
           year_share = year_count/year_sum) %>%
    left_join(bt_data, by = city_var) %>%
    filter(!is.na(rank_group)) %>%
    group_by(rank_group, year) %>%
    reframe(
      group_year_sum = sum(year_count),
      group_year_share = sum(year_share),
      group_year_inc = sum(incumbent_count),
      group_year_ent = sum(entrant_count),
      group_year_mig = sum(migrant_count)) %>%
    mutate(
      rank_group = as.character(rank_group)
    )
  
  return(by_year)
}

#Helper function to get yearly shares for rank groups
by_year_helper_inventor <- function(main_data, bt_data, class_var, city_var, cityname_var){
  by_year <- main_data %>%
    filter(.data[[class_var]] == 1) %>%
    #Five year bins as inventors don't invent *every year*
    mutate(
      year5 = floor((year - 2000)/5)*5 + 2000
    ) %>%
    group_by(.data[[city_var]], .data[[cityname_var]], year5) %>%
    summarize(
      #inventor counts
      inv_count = n_distinct(inventor_id),
      inc_inv_count = n_distinct(inventor_id[migrant_status == "incumbent"]),
      ent_inv_count = n_distinct(inventor_id[migrant_status == "entrant"]),
      mig_inv_count = n_distinct(inventor_id[migrant_status == "migrant"])
    ) %>%
    ungroup() %>%
    left_join(bt_data, by = city_var) %>%
    filter(!is.na(rank_group)) %>%
    group_by(rank_group, year5) %>%
    reframe(
      group_year_inv = sum(inv_count),
      group_year_inc_inv = sum(inc_inv_count),
      group_year_ent_inv = sum(ent_inv_count),
      group_year_mig_inv = sum(mig_inv_count)) %>%
    mutate(
      rank_group = as.character(rank_group)
    )
  
  return(by_year)
}