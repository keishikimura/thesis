#Load USPTO AI patent data
load_ai_dta <- function(path){
  read_tsv(path) %>%
    filter(flag_patent == 1) %>%
    select(-c(appl_id, flag_patent, flag_train_any, flag_train_ml,
              flag_train_evo, flag_train_nlp, flag_train_speech,
              flag_train_vision, flag_train_kr, flag_train_planning,
              flag_train_hardware)
    )
}

# Load USPTO patent application data
load_app_dta <- function(path){
  read_tsv(path) %>%
    select(c(patent_id, filing_date))
}

#Load USPTO disambiguated location data
load_location_dta <- function(path){
  read_tsv(path) %>%
    select(-c(latitude, longitude))
}

#Load CSA-CBSA-MSA crosswalks
load_csa_dta <- function(msa_path, csa_path, region_path){
  
  msa2013_dta <- read_csv(msa_path) %>%
    filter(!is.na(y2013)) %>%
    select(-ssacounty) %>%
    unique()
  
  csa2013_dta <- read_excel(csa_path,
                            sheet = "Feb. 2013 Crosswalk") %>%
    setNames(make.names(names(.), unique = TRUE))
  
  regions_dta <- read_csv(region_path) %>%
    setNames(make.names(names(.),unique = TRUE))
  
  msa2013_dta %>%
    mutate(fipscounty = as.numeric(fipscounty)) %>%
    left_join(csa2013_dta %>%
                mutate(County.Code = as.numeric(County.Code)), 
              by = c("fipscounty"="County.Code")) %>%
    left_join(regions_dta, by = c("state" = "State.Code")) %>%
    rename(
      CBSA.Code = cbsa,
      CBSA.Title = cbsaname
    )
}

#Load inventor data with disambiguated location
load_inventor_dta <- function(inventor_path, location_dta){
  read_tsv(inventor_path) %>%
    left_join(location_dta, by = "location_id") %>%
    ##Only want US-based inventors
    filter(disambig_country == "US")
}

#Load assignee data with disambiguated location
load_assignee_dta <- function(assignee_path, location_dta){
  read_csv(assignee_path) %>%
    left_join(location_dta, by = "location_id")  %>%
    ##Only want US-based assignees
    filter(disambig_country == "US")
}

#Helper function to assign patents to inventor location
inventor_prep <- function(city_type, inventor_location_dta, csa_dta,
                          migration_flags){
  
  city_var <- paste0(city_type, ".Code")
  title_var <- paste0(city_type, ".Title")
  
  inventor_MSA_dta <- inventor_location_dta %>%
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
    ) %>%
    select(c(patent_id, inventor_sequence, city, cityname, in.city, in.msa, Division)) %>%
    ##Get most frequent CITY_TYPE
    ##If tie, select CITY_TYPE of highest ranked inventor
    group_by(patent_id, city, cityname, in.city, in.msa, Division) %>%
    summarise(
      freq = n(),
      min_inventor_sequence = min(inventor_sequence)
    ) %>%
    arrange(desc(freq), min_inventor_sequence) %>%
    group_by(patent_id) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(inventor_MSA_dta)
}

#Helper function to assign patents to assignee location
assignee_prep <- function(city_type, assignee_location_dta, csa_dta){
  
  city_var <- paste0(city_type, ".Code")
  title_var <- paste0(city_type, ".Title")
  
  assignee_MSA_dta <- assignee_location_dta %>%
    ##Link with MSAs
    ##Again, many NAs in Ebeye
    mutate(
      fips = as.numeric(paste0(state_fips, county_fips))
    ) %>%
    filter(!is.na(fips)) %>%
    left_join(csa_dta, by = c("fips" = "fipscounty")) %>%
    mutate(
      in.city = !is.na(.data[[city_var]]),
      in.msa = !is.na(MSA.Code),
      city = ifelse(in.city, .data[[city_var]], paste0(countyname, state.x)),
      cityname = ifelse(in.city, .data[[title_var]], paste0(countyname, state.x))
    ) %>%
    select(c(patent_id, assignee_sequence, founding_year, assignee_sequence, 
             assignee_type, organization, university, UO_DISCERN, SUB_DISCERN,
             first_year_publicly_listed, VC_backed_assignee, city, cityname,
             in.city, in.msa)) %>%
    ##Since only 284 patents have multiple assignees and all have 2,
    ##we just choose the highest ranked assignee for all patents
    group_by(patent_id) %>%
    #Keep university indicator and location irrespective of assignee order
    mutate(univ = if_else(any(university == 1), 1, 0),
           univcity = paste(unique(city[university == 1]), collapse = ", "),
           univcityname = paste(unique(cityname[university == 1]), collapse = ", ")) %>%
    arrange(assignee_sequence) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(assignee_MSA_dta)
}

#Function to prep master data for given city type (MSA, CSA, CBSA)
master_prep <- function(city_type, inventor_location_dta, assignee_location_dta,
                        csa_dta, ai_dta, app_dta){
  
  inventor_MSA_dta <- inventor_prep(city_type, inventor_location_dta, csa_dta)
  print("Inventor Data Prepared.")
  assignee_MSA_dta <- assignee_prep(city_type, assignee_location_dta, csa_dta)
  print("Assignee Data Prepared.")
  
  inventor_MSA_dta_merge <- inventor_MSA_dta %>%
    rename(city_inventor = city, cityname_inventor = cityname) %>%
    select(-c(min_inventor_sequence, freq))
  
  assignee_MSA_dta_merge <- assignee_MSA_dta %>%
    rename(city_assignee = city, cityname_assignee = cityname) %>%
    select(-assignee_sequence)
  
  merged_dta <- ai_dta %>%
    left_join(inventor_MSA_dta_merge, by = c("doc_id" = "patent_id")) %>%
    left_join(assignee_MSA_dta_merge, by = c("doc_id" = "patent_id")) %>%
    left_join(app_dta, by = c("doc_id" = "patent_id")) %>%
    filter(!is.na(city_inventor))
  print("Data merged.")
  
  return(merged_dta)
}

#Read in MSA-level population data
load_population_dta <- function(path){
  population <- read_csv(path, skip = 10) %>%
    slice_head(n = 396) %>%
    slice_tail(n = 391)
  
  names(population)<-make.names(names(population),unique = TRUE)
  
  population %>%
    filter(is.na(Metro.Div..Code)) %>%
    rename(msa = Metro..Micro.Area.Code,
           msaname = Metropolitan.Statistical.Area.Metropolitan.Division.Micropolitan.Statistical.Area,
           pop2000 = Population,
           pop1990 = ...7
    ) %>%
    select(msa, msaname, pop2000, pop1990) %>%
    #Manually recode Los Angeles MSA and others
    mutate(msa = paste0("C", as.character(as.numeric(msa)/10)),
           msa = ifelse(msa == "C3110", "C3108", msa),
           msa = ifelse(msa == "C4206", "C4220", msa),
           msa = ifelse(msa == "C4694", "C4268", msa),
           pop2000 = as.numeric(gsub(",", "", pop2000)),
           pop1990 = as.numeric(gsub(",", "", pop1990))
           )
}

#Find counts of patent citations from inventors outside MSA
load_citation_dta <- function(citation_path,
                              patent_path,
                              AIpatent_MSA,
                              inventor_MSA_map,
                              cum_citations){
  
  AIpatent_dta <- AIpatent_MSA %>%
    filter(predict50_any_ai == 1) %>%
    select(doc_id, city_inventor, cityname_inventor, filing_date,
           predict50_ml, predict50_nlp, predict50_planning,
           predict50_kr, predict50_hardware, predict50_speech,
           predict50_vision)
  
  citation_dta <- read_tsv(citation_path) %>%
    select(patent_id, citation_patent_id) %>%
    right_join(AIpatent_dta, by = c("citation_patent_id" = "doc_id")) %>%
    filter(!is.na(patent_id))
  
  ai_citations_loc <- citation_dta %>%
    left_join(inventor_MSA_map, 
              by = "patent_id") %>%
    #Some inventor data are missing! If missing, select outside as TRUE.
    mutate(
      outside = (city_inventor != city),
      outside = ifelse(is.na(outside), TRUE, outside)
    )
  
  if(cum_citations){
    #General patent info
    patent_dta <- read_tsv(patent_path) %>%
      select(patent_id, patent_date)
    
    ai_citations_loc %>%
      left_join(patent_dta, by = "patent_id") %>%
      #Set patent application year, citation year, and years of lag
      mutate(
        app_year = year(filing_date),
        citation_year = year(patent_date),
        citation_lag = citation_year - app_year
      ) %>%
      #Find sum citations on patent per lag year
      group_by(citation_lag, citation_patent_id) %>%
      mutate(sum_lag_citation = n(),
             sum_lag_outside = sum(outside)) %>%
      ungroup() %>%
      #Filter for AI patents (7 classifications) and apps between 1976 and 2015
      filter(app_year >= 1976 & app_year <= 2015) %>%
      filter(rowSums(select(., predict50_ml, predict50_nlp, predict50_planning,
                            predict50_kr, predict50_hardware, predict50_speech,
                            predict50_vision) == 0) != 7) %>%
      #Take one observation per cited patent
      group_by(citation_patent_id, citation_lag) %>%
      slice_head(n=1) %>%
      select(-c(patent_id, patent_date)) %>%
      ungroup() %>%
      # Generate a complete sequence of years for each patent up to the max year found
      select(citation_patent_id, citation_lag, sum_lag_citation, sum_lag_outside) %>%
      complete(citation_patent_id, citation_lag = full_seq(citation_lag, 1),
               fill = list(sum_lag_citation = 0, sum_lag_outside = 0)) %>%
      # Calculate the cumulative sum of citations per patent
      arrange(citation_patent_id, citation_lag) %>%
      group_by(citation_patent_id) %>%
      mutate(cum_count = cumsum(sum_lag_citation),
             cum_outside = cumsum(sum_lag_outside)) %>%
      select(citation_patent_id, citation_lag, cum_count, cum_outside)
  
  } else{
    ai_citations_loc %>%
      group_by(citation_patent_id) %>%
      summarize(
        count = n(),
        sum_outside = sum(outside)
      )
  }
}

#Helper to extract cumulative citations of x year lag
citation_extractor <- function(data, lag){
  cit_varname <- paste0("count_", lag)
  out_varname <- paste0("out_", lag)
  
  data %>%
    filter(citation_lag == lag) %>%
    select(-citation_lag) %>%
    rename(!!cit_varname := cum_count,
           !!out_varname := cum_outside)
}

#Helper to rename city variables
rename_city_vars <- function(data, prefix) {
  df <- data %>%
    rename(
      !!paste0(prefix, "_inventor") := city_inventor,
      !!paste0(prefix, "name_inventor") := cityname_inventor,
      !!paste0(prefix, "_assignee") := city_assignee,
      !!paste0(prefix, "name_assignee") := cityname_assignee,
      !!paste0(prefix, "_univ") := univcity,
      !!paste0(prefix, "name_univ") := univcityname,
      !!paste0("in.", prefix) := in.city.x
    ) 
  
  #keep all other columns in at least one dataset
  if(prefix == "msa"){
    df
  } else {
    df %>%
      select(doc_id,
             matches(paste0("^", prefix, "_")),
             matches(paste0("^", prefix, "name_")),
             matches(paste0("^in\\.", prefix))
             )
  }
}

load_master_dta <- function(AIpatent_CSA,
                            AIpatent_CBSA,
                            AIpatent_MSA){
  
  dta_csa <- rename_city_vars(AIpatent_CSA, "csa")
  dta_cbsa <- rename_city_vars(AIpatent_CBSA, "cbsa")
  dta_msa <- rename_city_vars(AIpatent_MSA, "msa")
  
  dta_msa %>%
    left_join(dta_csa, by = "doc_id") %>%
    left_join(dta_cbsa, by = "doc_id")
}

load_sel_cities <- function(csa_dta, csa_vec){
  csa_dta %>%
    filter(CSA.Code %in% csa_vec) %>%
    select(MSA.Code, CSA.Code, CSA.Title) %>%
    group_by(MSA.Code) %>%
    slice_head(n=1)
}

cum_citations <- function(lag){
  cit_varname <- paste0("count_", as.character(lag))
  out_varname <- paste0("out_", as.character(lag))
  
  df <- read_csv("Data/generated/cum_citations_outside.csv") %>%
    filter(citation_lag == lag) %>%
    select(-citation_lag) %>%
    rename(!!cit_varname := cum_count,
           !!out_varname := cum_outside)
}

merge_master_dta <- function(master_dta, csa_dta, citation_dta, cum_citation_dta, 
                             lag_vec, msa_tech_vec, msa_big_vec){
  msa_tech <- load_sel_cities(
      csa_dta = csa_dta,
      csa_vec = msa_tech_vec
  )
  
  msa_big <- load_sel_cities(
      csa_dta = csa_dta,
      csa_vec = msa_big_vec
  )
  
  cum_citations_merged <- lag_vec %>%
    map(~ citation_extractor(cum_citation_dta, .x)) %>%
    reduce(left_join, by = "citation_patent_id") %>%
    right_join(citation_dta, by = c("citation_patent_id"))

  master_dta %>%
    #Filter out evo
    filter(
      if_any(
        c(starts_with("predict50_"), -starts_with("predict50_evo")),
        ~ . != 0
      )
    ) %>%
    left_join(cum_citations_merged, by = c("doc_id" = "citation_patent_id")) %>%
    rename(forward_citations = count,
           outside_citations = sum_outside) %>%
    mutate(
      across(c(pub_dt, filing_date), ymd),
      #Based on application year for when invention occurs
      year = year(filing_date)
    ) %>%
    filter(
      predict50_any_ai == 1,
      between(year, 1976, 2015)
    ) %>%
    mutate(
      across(
        matches("^(forward_citations|outside_citations|count_\\d+|out_\\d+)$"),
        ~ replace_na(.x, 0)
      )
    ) %>%
    #Make components mutually exclusive by taking highest p()
    rowwise() %>%
    mutate(
      max_val = max(c_across(starts_with("ai_score_"))),
      across(
        starts_with("ai_score_"),
        ~ as.integer(.x == max_val),
        .names = "{sub('^ai_score_','', .col)}_indic"
      ),
      any_ai_indic = predict50_any_ai
    ) %>%
    ungroup() %>%
    select(-max_val) %>%
    mutate(
      #6 tech clusters; 5 big cities
      tech_cluster = msa_inventor %in% c(msa_tech$MSA.Code, "C4174", "C1242"),
      big_city = msa_inventor %in% msa_big$MSA.Code,
      other = !(tech_cluster | big_city)
    ) %>%
    left_join(msa_tech, by = c("msa_inventor" = "MSA.Code")) %>%
    left_join(msa_big, by = c("msa_inventor" = "MSA.Code")) %>%
    mutate(
      key_csa = case_when(
        msa_inventor %in% c("C4174", "C1242") ~ msa_inventor,
        !is.na(CSA.Code.x) ~ CSA.Code.x,
        .default = CSA.Code.y
      ),
      key_csaname = case_when(
        msa_inventor %in% c("C4174", "C1242") ~ msaname_inventor,
        !is.na(CSA.Code.x) ~ CSA.Title.x,
        .default = CSA.Title.y
      )
    ) %>%
    select(-c(CSA.Code.x, CSA.Code.y, CSA.Title.x, CSA.Title.y)) %>%
    return()
    
}

gov_index <- function(data, data_gov, treatment_year, pre_periods, class_var, city_var){
  
  gov_index <- data %>%
    filter(.data[[class_var]] == 1) %>%
    left_join(data_gov, by = c("doc_id" = "patent_id")) %>%
    mutate(
      gov = !is.na(fedagency_name),
      darpa = ifelse(is.na(level_two), FALSE, 
                     level_two == "Defense Advanced Research Projects Agency"),
      dod = ifelse(is.na(level_one), FALSE, 
                   level_one == "Department of Defense")
    ) %>%
    filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
    group_by(.data[[city_var]]) %>%
    summarize(gov_index = sum(gov),
              darpa_index = sum(darpa),
              dod_index = sum(dod))
  
  return(gov_index)
}

univ_index <- function(data, treatment_year, pre_periods, class_var, city_var, univcity_var){
    msa_index <- data %>%
      filter(.data[[class_var]] == 1) %>% 
      filter(univ == 1) %>%
      separate_rows(.data[[univcity_var]], sep = ",\\s*") %>%
      filter(grepl("^C[0-9]+$", .data[[univcity_var]])) %>%
      filter(year <= treatment_year & year >= treatment_year - pre_periods) %>%
      group_by(.data[[univcity_var]]) %>%
      summarize(univ_index = n()) %>%
      rename(!!city_var := univcity_var)
    
    return(msa_index)
}