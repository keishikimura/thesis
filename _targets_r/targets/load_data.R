lapply(
  c(
    "PipelineCode/data_funs.R"
  ),
  tar_source
)

list(
  tar_target(
    ai_dta,
    load_ai_dta("Data/raw/uspto/ai_model_predictions.tsv")
  ),
  tar_target(
    app_dta,
    load_app_dta("Data/raw/uspto/g_application.tsv")
  ),
  tar_target(
    location_dta,
    load_location_dta("Data/raw/uspto/g_location_disambiguated.tsv")
  ),
  tar_target(
    csa_dta,
    load_csa_dta(
      msa_path = "Data/raw/nber/cbsatocountycrosswalk.csv",
      csa_path = "Data/raw/census/qcew-county-msa-csa-crosswalk.xlsx",
      region_path = "Data/raw/census/regions.csv"
    )
  ),
  tar_target(
    gov_interest,
    read_tsv("Data/raw/uspto/g_gov_interest_org.tsv")
  ),
  tar_target(
    inventor_location_dta,
    load_inventor_dta(
      inventor_path = "Data/raw/uspto/g_inventor_disambiguated.tsv",
      location_dta = location_dta
    )
  ),
  tar_target(
    assignee_location_dta,
    load_assignee_dta(
      assignee_path = "Data/raw/uspto/_patent_ocpb_augmented.csv",
      location_dta = location_dta
    )
  ),
  tar_target(
    inventor_MSA_map,
    inventor_prep(
      city_type = "MSA",
      inventor_location_dta = inventor_location_dta,
      csa_dta = csa_dta
    )
  ),
  tar_map(
    values = list(city_type = city_types),
    names = city_type,
    tar_target(
      AIpatent,
      master_prep(
        city_type = city_type,
        inventor_location_dta = inventor_location_dta,
        assignee_location_dta = assignee_location_dta,
        csa_dta = csa_dta,
        ai_dta = ai_dta,
        app_dta = app_dta
      )
    )
  ),
  tar_target(
    population_dta,
    load_population_dta("Data/raw/census/tab05b.csv")
  ),
  tar_map(
    values = list(cum_citations = c(T,F)),
    names = cum_citations,
    tar_target(
      outside_citation_dta,
      load_citation_dta(
        citation_path = "Data/raw/uspto/g_us_patent_citation.tsv",
        patent_path = "Data/raw/uspto/g_patent.tsv",
        AIpatent_MSA = AIpatent_MSA,
        inventor_MSA_map = inventor_MSA_map,
        cum_citations = cum_citations
      )
    )
  ),
  tar_target(
    master_dta,
    load_master_dta(
      AIpatent_CSA = AIpatent_CSA,
      AIpatent_CBSA = AIpatent_CBSA,
      AIpatent_MSA = AIpatent_MSA
    )
  ),
  tar_target(
    merge_master,
    merge_master_dta(
      master_dta = master_dta,
      csa_dta = csa_dta,
      citation_dta = outside_citation_dta_FALSE,
      cum_citation_dta = outside_citation_dta_TRUE,
      lag_vec = c(4, 9, 14, 18, 24, 34),
      msa_tech_vec = c("CS488", "CS148", "CS500", "CS216"),
      msa_big_vec = c("CS408", "CS348", "CS176", "CS428", "CS220")
    )
  )
)
