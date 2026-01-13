.get_good_project_path <- function(env = parent.frame()) {
  good_project_path <- "./data/good_project"
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(good_project_path, dir, recursive = TRUE)

  file.path(dir, basename(good_project_path))
}