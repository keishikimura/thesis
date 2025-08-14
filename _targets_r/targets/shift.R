lapply(
  c(
    "PipelineCode/sample_funs.R",
    "PipelineCode/model_funs.R",
    "PipelineCode/shifts.R"
  ),
  tar_source
)

list(
  tar_target(
    specs_shift_sample,
    shift_sample_specs
  ),
  tar_target(
    shift_sample_est,
    shift_sample(
      data = merge_master,
      pop_data = population_dta,
      gov_data = gov_interest,
      sample_length = specs_shift_sample$sample_length,
      pre_periods = specs_shift_sample$pre_periods, 
      citation_var = specs_shift_sample$citation_var,
      filepath = specs_shift_sample$fig_path
    ),
    pattern = map(specs_shift_sample),
    format  = "file"
  ),
  tar_target(
    specs_shift_post,
    shift_post_specs
  ),
  tar_target(
    shift_post_est,
    shift_post(
      data = merge_master,
      pop_data = population_dta,
      gov_data = gov_interest,
      treatment_year = specs_shift_post$treatment_year,
      increment = specs_shift_post$increment,
      pre_periods = specs_shift_post$pre_periods, 
      citation_var = specs_shift_post$citation_var,
      filepath = specs_shift_post$fig_path
    ),
    pattern = map(specs_shift_post),
    format  = "file"
  )
)
