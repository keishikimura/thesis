lapply(
  c(
    "PipelineCode/sample_funs.R",
    "PipelineCode/graph_funs.R"
  ),
  tar_source
)

list(
  tar_target(
    specs_graphical,
    graphical_specs
  ),
  tar_target(
    graphical_analysis,
    graphical(
      dta_all_merged_excl = merge_master,
      treatment_year = specs_graphical$treatment_year,
      pre_periods = 9,
      city_type = "msa",
      method = 1,
      citation_var = specs_graphical$citation_var,
      fig_path = specs_graphical$fig_path
    ),
    pattern = map(specs_graphical),
    format  = "file"
  )
)
