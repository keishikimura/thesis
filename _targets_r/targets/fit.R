lapply(
  c(
    "PipelineCode/sample_funs.R",
    "PipelineCode/model_funs.R",
    "PipelineCode/model_specs.R",
    "PipelineCode/loo.R"
  ),
  tar_source
)

list(
  tar_target(
    model_dta_all,
    gen_model_dta_all(data = merge_master,
                      pop_data = population_dta,
                      gov_data = gov_interest)
  ),
  tar_target(
    main_table,
    gen_main_table(data = merge_master,
                   pop_data = population_dta,
                   gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    share_table,
    gen_share_table(data = merge_master,
                    pop_data = population_dta,
                    gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    window_table,
    gen_window_table(data = merge_master,
                     pop_data = population_dta,
                     gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    robustness_table,
    gen_robustness_table(data = merge_master,
                         pop_data = population_dta,
                         gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    altsample_table,
    gen_altsample_table(data = merge_master,
                        data_all = model_dta_all,
                        pop_data = population_dta,
                        gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    tech_table,
    gen_tech_table(data = merge_master,
                   pop_data = population_dta,
                   gov_data = gov_interest),
    format = "file"
  ),
  tar_target(
    loo,
    gen_loo_fig(data = merge_master,
                pop_data = population_dta,
                gov_data = gov_interest),
    format = "file"
  )
)
