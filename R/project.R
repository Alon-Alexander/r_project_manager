#' @title Project class to manage a project folder
#'
#' @description
#' This object can be used to manage a project folder.
#' Use it to create analyses, freeze input files, etc.
#' 
#' @field path Full path to the project's folder
#' 
#' @examples
#' # Create a valid project to load
#' folder <- withr::local_tempdir()
#' invisible(pm_create_project(folder))
#'
#' # Load the project
#' pm <- PMProject$new(folder)
#' pm
#'
#' @importFrom R6 R6Class
#' @export
PMProject <- R6Class("PMProject",
  public = list(
    path = NULL,

    #' @description
    #' Create a PMProject object in an existing project's folder
    #'
    #' @param path Path to the project
    initialize = function(path) {
      chk::check_dirs(path)
      path <- normalizePath(path)
      self$path <- path

      self$validate()
    },

    #' @description
    #' Validate the project folder.
    #' Makes sure all expected files and folder exist and are valid.
    validate = function() {
      chk::check_files(
        private$at(constants$INPUTS_FILENAME),
        x_name = "Inputs definition file"
      )
      chk::check_files(
        private$at(constants$LOCAL_INPUTS_FILENAME),
        x_name = "Local inputs mapping file"
      )
      chk::check_files(
        private$at(constants$README_FILENAME),
        x_name = "Readme file"
      )
      chk::check_dirs(
        private$at(constants$ANALYSES_DIR),
        x_name = "Analyses folder"
      )
    }
  ),
  private = list(
    at = function(...) {
      file.path(self$path, ...)
    }
  )
)

#' @title Create a PMProject object for a given project folder
#'
#' @param path Path to the project
#'
#' @return \code{PMProject} object representing the given project folder
#'
#' @examples
#' # Create a valid project to load
#' folder <- withr::local_tempdir()
#' invisible(pm_create_project(folder))
#'
#' # Load the project
#' pm <- pm_project(folder)
#' pm
#'
#' @export
pm_project <- function(path) {
  PMProject$new(path)
}

#' @title Create a new project from template in the given folder
#'
#' @description
#' Creates a new clean project in the given folder and returns a
#' PMProject object.
#' Also works on an empty folder, but fails if the folder contains
#' irrelevant content.
#'
#' @param path Path to the folder in which to create the project
#'
#' @return \code{PMProject} object representing the newly created project
#'
#' @note If the folder already exists and has contents, does not override it,
#' but instead returns the existing project from that folder.
#'
#' @examples
#' empty_folder <- withr::local_tempdir()
#' pm <- pm_create_project(empty_folder)
#' pm
#'
#' @export
pm_create_project <- function(path) {
  if (dir.exists(path)) {
    # Check if valid project and if so return it
    proj <- tryCatch(pm_project(path), error = function(e) NULL)
    if (!is.null(proj)) {
      return(proj)
    }

    # Check if empty folder (and then we'll populate it)
    files <- list.files(path)
    if (length(files) > 0) {
      # Folder exists but invalid, raise error
      stop(sprintf(
        "Trying to create a new project in folder \"%s\" which exists but contains an invalid project",
        path
      ))
    }
  } else {
    dir.create(path)
  }

  # Copy default folder structure to output folder
  # Not using `file.copy` because then we'll get a folder named
  # "template_project" inside our destination instead of copying the files
  template_dir <- system.file("extdata", constants$TEMPLATE_PROJECT_DIR, package = "pm")
  .recursive_copy(template_dir, path)

  # Manually copy gitignore (otherwise I would be missing files in the package :)
  template_gitignore <- system.file("extdata", constants$TEMPLATE_GITIGNORE_FILENAME, package = "pm")
  file.copy(
    template_gitignore,
    file.path(path, ".gitignore")
  )

  pm_project(path)
}


.recursive_copy <- function(from_dir, to_dir) {
  chk::check_dirs(
    from_dir,
    x_name = "Source of copy"
  )
  chk::check_dirs(
    to_dir,
    x_name = "Destination of copy"
  )

  for (file in list.files(from_dir)) {
    file.copy(file.path(from_dir, file), to_dir, overwrite = TRUE)
  }

  for (inner_source_dir in list.dirs(from_dir)) {
    if (identical(inner_source_dir, from_dir)) {
      next
    }

    inner_dest_dir <- file.path(to_dir, basename(inner_source_dir))
    dir.create(inner_dest_dir, showWarnings = FALSE)

    .recursive_copy(inner_source_dir, inner_dest_dir)
  }
}
