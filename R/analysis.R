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
          self$project_path <- normalizePath(dirname(parent), mustWork = FALSE)
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
    },

    #' @description
    #' Get output path for a file, returning a PMData object.
    #'
    #' @param name Character. Name of the output file (with or without extension).
    #' @param type Character. Optional type of output (table, object, image, figure, parquet, csv).
    #'   If provided and name has no extension, an appropriate extension will be added.
    #'   If provided and name has an extension, the extension will be validated against the type.
    #' @param intermediate Logical. If TRUE, file goes in intermediate/ folder; if FALSE, in outputs/ folder.
    #'
    #' @return A \code{PMData} object with:
    #'   - \code{id}: The file name without extension
    #'   - \code{path}: The full absolute path to the output file
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis <- pm$create_analysis("my_analysis")
    #'
    #' # Get output path for a CSV file
    #' output <- analysis$get_output_path("results.csv", type = "table")
    #' output$id    # "results"
    #' output$path  # full path to results.csv in outputs/
    #'
    #' # Get intermediate path without extension (will add .parquet for table type)
    #' intermediate <- analysis$get_output_path("temp_data", type = "table", intermediate = TRUE)
    #' intermediate$id    # "temp_data"
    #' intermediate$path  # full path to temp_data.parquet in intermediate/
    get_output_path = function(name, type = NULL, intermediate = FALSE) {
      # Store original name for ID (without extension)
      original_name <- name
      original_ext <- tolower(tools::file_ext(original_name))
      id <- if (identical(original_ext, "")) {
        original_name
      } else {
        tools::file_path_sans_ext(original_name)
      }

      ext <- tolower(tools::file_ext(name))

      if (is.null(type)) {
        # Assume rds object if no extension given
        # Otherwise, just propagate given extension
        if (identical(ext, "")) {
          name <- paste0(name, ".rds")
        }
      } else {
        if (identical(ext, "")) {
          # Add extension based on type
          # Special names are converted but the rest are
          # just assumed to be the extension.
          new_ext <- switch(tolower(type),
            table = "parquet",
            object = "rdata",
            image = ,
            figure = "png",
            tolower(type)
          )

          name <- paste0(name, ".", new_ext)
        } else {
          # Validate extension and type match
          possible_extensions <- switch(tolower(type),
            parquet = ,
            pqt = c("parquet", "pqt"),
            table = c("parquet", "pqt", "tsv", "csv", "rds", "rdata", "rda"),
            object = c("rdata", "rda", "rds"),
            image = ,
            figure = c("png", "jpeg", "jpg", "svg", "gif", "tiff", "bmp"),
            c(tolower(type))
          )

          if (!(ext %in% possible_extensions)) {
            stop(sprintf(
              "Got type = %s and file with extension %s, and expected the extension to be one of %s",
              type,
              ext,
              possible_extensions
            ))
          }
        }
      }

      folder <- if (intermediate) constants$ANALYSIS_INTERMEDIATE_DIR else constants$ANALYSIS_OUTPUT_DIR

      full_path <- normalizePath(file.path(self$path, folder, name), mustWork = FALSE)
      
      PMData$new(id = id, path = full_path)
    }
  )
)
