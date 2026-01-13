.get_good_project_path <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  invisible(pm_create_project(dir))
  dir
}