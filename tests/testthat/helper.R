.get_good_project_path <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  invisible(pm_create_project(dir))
  
  # Create a valid project.yaml with two inputs: one csv, one tsv
  inputs_yaml <- list(
    inputs = list(
      test_input = list(),
      test_tsv = list()
    )
  )
  yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))
  
  # Create a valid temp file for the csv input
  test_file_csv <- file.path(dir, "test_input.csv")
  writeLines(c("col1,col2", "a,1", "b,2"), test_file_csv)
  
  # Create a valid temp file for the tsv input
  test_file_tsv <- file.path(dir, "test_tsv.tsv")
  writeLines(c("col1\tcol2", "a\t1", "b\t2"), test_file_tsv)
  
  # Create a valid inputs.local.yaml pointing to the temp files
  local_inputs_yaml <- list(
    paths = list(
      test_input = "test_input.csv",
      test_tsv   = "test_tsv.tsv"
    )
  )
  yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))
  
  dir
}