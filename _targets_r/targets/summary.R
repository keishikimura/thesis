lapply(
  c(
    "PipelineCode/summary.R"
  ),
  tar_source
)

list(
  tar_target(
    summary_all,
    gen_summary_all(data_all = model_dta_all),
    format = "file"
  ),
  tar_map(
    values = list(panels = c("A","B")),
    names = panel,
    tar_target(
      summary_top,
      gen_summary_top(data = merge_master,
                      data_all = model_dta_all,
                      panel = panels),
      format = "file"
    )
  ),
  tar_target(
    summary_tech,
    gen_summary_tech(data = merge_master,
                     gov_data = gov_interest),
    format = "file"
  )
)
