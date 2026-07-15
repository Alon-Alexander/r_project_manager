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

.compare_paths <- function(actual, expected) {
  actual <- gsub("\\", "/", normalizePath(actual, mustWork = FALSE), fixed = TRUE)
  expected <- gsub("\\", "/", normalizePath(expected, mustWork = FALSE), fixed = TRUE)

  expect_equal(actual, expected)
}

.pm_r_script_path <- function(path) {
  normalizePath(path, mustWork = FALSE, winslash = "/")
}

.pm_rscript_path <- function() {
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  if (!file.exists(rscript_path)) {
    alt <- Sys.which("Rscript")
    if (nzchar(alt) && grepl("[/\\\\]", alt)) {
      rscript_path <- alt
    }
  }
  rscript_path
}

.pm_rscript_load_cmds <- function() {
  pkg_root <- .pm_r_script_path(testthat::test_path("../.."))
  r_dir <- file.path(pkg_root, "R")
  has_r_source <- dir.exists(r_dir) &&
    length(list.files(r_dir, pattern = "[.]R$")) > 0L

  if (has_r_source && requireNamespace("pkgload", quietly = TRUE)) {
    return(sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(pkg_root)))
  }

  pm_lib <- .pm_r_script_path(dirname(find.package("pm")))
  c(
    sprintf(".libPaths(c(%s, .libPaths()))", shQuote(pm_lib)),
    "library(pm)"
  )
}

.pm_run_rscript <- function(script_path, stdout = FALSE, stderr = FALSE) {
  rscript_path <- .pm_rscript_path()
  script_path <- .pm_r_script_path(script_path)
  result <- system2(
    rscript_path,
    args = script_path,
    stdout = stdout,
    stderr = stderr,
    wait = TRUE
  )
  status <- if (is.null(result)) attr(result, "status") else result
  if (is.null(status)) 0L else as.integer(status)
}