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
    #' Get an artifact (output file) from another analysis by ID.
    #' Searches for files with the given ID (filename without extension) in analysis output directories.
    #'
    #' @param id Character. The artifact ID (filename without extension).
    #' @param analysis_name Character. Optional name of the analysis to search in.
    #'   If not provided, uses the current analysis's name. If explicitly set to \code{NULL},
    #'   searches all analyses and fails if not exactly one match is found.
    #'
    #' @return A \code{PMData} object with the artifact's ID and path.
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis1 <- pm$create_analysis("data_preparation")
    #' analysis2 <- pm$create_analysis("modeling")
    #'
    #' # Create a test output file in analysis1
    #' output <- analysis1$get_output_path("results.csv", type = "table")
    #' write.csv(data.frame(x = 1:5), output$path)
    #'
    #' # Get artifact from analysis2 (which gets it from analysis1)
    #' artifact <- analysis2$get_artifact("results", analysis_name = "data_preparation")
    #'
    #' # Get artifact from current analysis (default behavior)
    #' artifact <- analysis1$get_artifact("results")
    #'
    #' # Get artifact without specifying analysis (if unique across all analyses)
    #' artifact <- analysis2$get_artifact("results", analysis_name = NULL)
    get_artifact = function(id, analysis_name = NULL) {
      # Need project for getting artifacts
      if (is.null(self$project_path)) {
        stop("Cannot get artifact: analysis is not associated with a project")
      }

      # Determine analysis_name: if not provided (missing), use self.name
      # If explicitly NULL, pass NULL to project
      call_obj <- match.call()
      analysis_name_provided <- "analysis_name" %in% names(call_obj)
      if (!analysis_name_provided) {
        analysis_name <- self$name
      }

      project <- PMProject$new(self$project_path)
      project$get_artifact(id = id, analysis_name = analysis_name)
    },

    #' @description
    #' Get an intermediate artifact from the current analysis's intermediate folder.
    #' Searches for existing files with the given ID (filename without extension) in the intermediate directory.
    #' If an existing file is found, returns it. If no existing file is found, returns the path
    #' via \code{get_output_path()} (the file may not exist yet).
    #'
    #' @param id Character. The artifact ID (filename without extension).
    #'
    #' @return A \code{PMData} object with the artifact's ID and path.
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis <- pm$create_analysis("data_preparation")
    #'
    #' # Get artifact from current analysis's intermediate folder
    #' # If file exists, returns it; otherwise returns path for new file
    #' artifact <- analysis$get_intermediate_artifact("temp_data")
    #' if (artifact$exists()) {
    #'   data <- artifact$read()
    #' } else {
    #'   # File doesn't exist yet, can write to it
    #'   artifact$write(data.frame(x = 1:5))
    #' }
    get_intermediate_artifact = function(id) {
      # Search for existing intermediate files
      intermediates <- self$list_outputs(intermediate = TRUE)

      # Filter outputs matching the ID
      matching_outputs <- Filter(function(output) identical(output$id, id), intermediates)

      # Handle results
      if (length(matching_outputs) == 0) {
        # No existing file found, return output path (file may not exist yet)
        return(self$get_output_path(id, intermediate = TRUE))
      }

      if (length(matching_outputs) > 1) {
        # Multiple files with same ID
        file_names <- vapply(matching_outputs, function(x) basename(x$path), character(1))
        stop(sprintf(
          "Multiple artifacts with ID '%s' found in intermediate folder: %s",
          id, paste(file_names, collapse = ", ")
        ))
      }

      # Return the single matching file
      matching_outputs[[1]]
    },

    #' @description
    #' List all output files in the analysis.
    #' Returns a list of PMData objects for all files in the outputs or intermediate directory.
    #'
    #' @param intermediate Logical. If TRUE, lists files in intermediate/ folder; if FALSE, in outputs/ folder.
    #'
    #' @return A list of \code{PMData} objects, one for each file found.
    #'   Each object has:
    #'   - \code{id}: The file name without extension
    #'   - \code{path}: The full absolute path to the file
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis <- pm$create_analysis("my_analysis")
    #'
    #' # Create some output files
    #' output1 <- analysis$get_output_path("results.csv", type = "table")
    #' output2 <- analysis$get_output_path("plot.png", type = "figure")
    #'
    #' # List all outputs
    #' outputs <- analysis$list_outputs()
    #' length(outputs)  # Number of output files
    #'
    #' # List intermediate files
    #' intermediates <- analysis$list_outputs(intermediate = TRUE)
    list_outputs = function(intermediate = FALSE) {
      folder <- if (intermediate) constants$ANALYSIS_INTERMEDIATE_DIR else constants$ANALYSIS_OUTPUT_DIR
      outputs_dir <- file.path(self$path, folder)

      if (!dir.exists(outputs_dir)) {
        return(list())
      }

      # Get all files in the directory
      files <- list.files(outputs_dir, full.names = TRUE)
      if (length(files) == 0) {
        return(list())
      }

      # Filter to only files (not directories) using vectorized approach
      file_info <- file.info(files)
      is_file <- !is.na(file_info$isdir) & !file_info$isdir
      files_only <- files[is_file]

      if (length(files_only) == 0) {
        return(list())
      }

      # Create PMData objects using lapply
      file_ids <- tools::file_path_sans_ext(basename(files_only))
      file_paths <- normalizePath(files_only, mustWork = FALSE)

      lapply(seq_along(files_only), function(i) {
        PMData$new(id = file_ids[i], path = file_paths[i])
      })
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
        type_lower <- tolower(type)
        if (identical(ext, "")) {
          # Add extension based on type
          new_ext <- constants$TYPE_MAPPINGS$defaults[[type_lower]] %||% type_lower
          name <- paste0(name, ".", new_ext)
        } else {
          # Validate extension and type match
          possible_extensions <- constants$TYPE_MAPPINGS$allowed[[type_lower]] %||% c(type_lower)

          if (!(ext %in% possible_extensions)) {
            stop(sprintf(
              "Got type = %s and file with extension %s, and expected the extension to be one of %s",
              type,
              ext,
              paste(possible_extensions, collapse = ", ")
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
