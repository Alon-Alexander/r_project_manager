#' @title Analysis class to manage an analysis folder
#'
#' @description
#' This object can be used to manage an analysis folder within a project.
#' Each analysis contains code, outputs, intermediate results, and logs.
#' 
#' @field path Full path to the analysis's folder
#' @field name Name of the analysis (folder name within analyses/)
#' @field project_path Full path to the project's folder (if created from project)
#' 
#' @examples
#' # Create a project and analysis
#' folder <- withr::local_tempdir()
#' pm <- pm_create_project(folder)
#' 
#' # Create a new analysis
#' analysis <- pm$create_analysis("data_preparation")
#' analysis
#'
#' # Load an existing analysis from project
#' analysis <- pm$get_analysis("data_preparation")
#' 
#' # Load an existing analysis from path
#' analysis <- PMAnalysis$new(path = file.path(folder, "analyses", "data_preparation"))
#'
#' @importFrom R6 R6Class
#' @export
PMAnalysis <- R6Class("PMAnalysis",
  public = list(
    path = NULL,
    name = NULL,
    project_path = NULL,

    #' @description
    #' Create a PMAnalysis object
    #'
    #' @param project PMProject object. If provided, creates analysis within that project.
    #' @param name Character. Name of the analysis (folder name). Required if project is provided.
    #' @param path Character. Full path to the analysis folder. Required if project is not provided.
    initialize = function(project = NULL, name = NULL, path = NULL) {
      if (!is.null(project)) {
        # Construct from project object
        chk::chk_s3_class(project, "PMProject")
        chk::chk_scalar(name)
        chk::chk_character(name)
        
        self$project_path <- project$path
        self$name <- name
        self$path <- file.path(project$path, constants$ANALYSES_DIR, name)
        self$path <- normalizePath(self$path, mustWork = FALSE)
      } else if (!is.null(path)) {
        # Construct from path
        chk::chk_scalar(path)
        chk::chk_character(path)
        
        self$path <- normalizePath(path, mustWork = FALSE)
        self$name <- basename(path)
        
        # Try to infer project path (parent of analyses directory)
        parent <- dirname(path)
        if (basename(parent) == constants$ANALYSES_DIR) {
          self$project_path <- dirname(parent)
        }
      } else {
        stop("Must provide either 'project' and 'name', or 'path'")
      }
      
      self$validate()
    },

    #' @description
    #' Validate the analysis folder.
    #' Makes sure all expected files and folders exist.
    validate = function() {
      chk::check_dirs(self$path, x_name = "Analysis folder")
      chk::check_files(
        file.path(self$path, constants$README_FILENAME),
        x_name = "Analysis README file"
      )
      chk::check_dirs(
        file.path(self$path, "code"),
        x_name = "Code folder"
      )
      chk::check_dirs(
        file.path(self$path, "outputs"),
        x_name = "Outputs folder"
      )
      chk::check_dirs(
        file.path(self$path, "intermediate"),
        x_name = "Intermediate folder"
      )
      chk::check_dirs(
        file.path(self$path, "logs"),
        x_name = "Logs folder"
      )
    },

    #' @description
    #' Print method for PMAnalysis
    print = function() {
      cat("PMAnalysis:\n")
      cat("  Name: ", self$name, "\n", sep = "")
      cat("  Path: ", self$path, "\n", sep = "")
      if (!is.null(self$project_path)) {
        cat("  Project: ", self$project_path, "\n", sep = "")
      }
      invisible(self)
    }
  )
)

