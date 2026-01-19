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
    #' Print method for PMProject
    print = function() {
      cat("PMProject:\n")
      cat("  Path: ", self$path, "\n", sep = "")
      
      # Get analyses
      analyses <- tryCatch(
        self$list_analyses(),
        error = function(e) character(0)
      )
      n_analyses <- length(analyses)
      cat("  Analyses: ", n_analyses, "\n", sep = "")
      if (n_analyses > 0) {
        # Show analysis names, truncate if too many
        if (n_analyses <= 5) {
          for (analysis in analyses) {
            cat("    - ", analysis, "\n", sep = "")
          }
        } else {
          for (analysis in analyses[1:4]) {
            cat("    - ", analysis, "\n", sep = "")
          }
          cat("    ... and ", n_analyses - 4, " more\n", sep = "")
        }
      }
      
      # Get inputs (may fail if not configured)
      n_inputs <- tryCatch(
        length(self$parse_inputs()),
        error = function(e) NULL
      )
      if (!is.null(n_inputs)) {
        cat("  Inputs: ", n_inputs, "\n", sep = "")
      }
      
      invisible(self)
    },

    #' @description
    #' Validate the project folder.
    #' Makes sure all expected files and folder exist and are valid.
    #' Also validates that all input files referenced in inputs.local.yaml exist.
    validate = function() {
      chk::check_files(
        private$at(constants$CONFIGURATION_FILENAME),
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

      # Validate input files exist
      .validate_input_files(
        project_path = self$path,
        configuration_file = private$at(constants$CONFIGURATION_FILENAME),
        local_inputs_file = private$at(constants$LOCAL_INPUTS_FILENAME)
      )
    },

    #' @description
    #' Parse project.yaml and inputs.local.yaml into PMData objects.
    #' Reads the inputs definition file and the local paths file
    #' and combines them to create a list of PMData objects.
    #'
    #' @return A list of PMData objects, one for each input defined in project.yaml
    #'   that has a corresponding path in inputs.local.yaml
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #'
    #' # After configuring project.yaml and inputs.local.yaml:
    #' # data_list <- pm$parse_inputs()
    parse_inputs = function() {
      # Read project.yaml (portable definitions)
      configuration_file <- private$at(constants$CONFIGURATION_FILENAME)
      chk::check_files(configuration_file, x_name = "Project configuration file")
      inputs_def <- tryCatch(
        yaml::read_yaml(configuration_file),
        error = function(e) {
          stop("project.yaml must be a YAML object (key-value pairs): ", conditionMessage(e))
        }
      )

      # Validate project.yaml schema
      .validate_inputs_schema(inputs_def)

      # Read inputs.local.yaml (local paths)
      local_inputs_file <- private$at(constants$LOCAL_INPUTS_FILENAME)
      chk::check_files(local_inputs_file, x_name = "Local inputs mapping file")
      local_inputs <- tryCatch(
        yaml::read_yaml(local_inputs_file),
        error = function(e) {
          stop("inputs.local.yaml must be a YAML object (key-value pairs): ", conditionMessage(e))
        }
      )

      # Validate inputs.local.yaml schema
      .validate_local_inputs_schema(local_inputs)

      # Extract input IDs from project.yaml
      # Inputs are defined under "inputs" key as a named list
      input_ids <- .extract_input_ids(inputs_def)

      # Create PMData objects by combining IDs with paths from local file
      data_list <- list()
      for (id in input_ids) {
        if (!id %in% names(local_inputs$paths)) {
          stop(sprintf(
            "Input ID '%s' is defined in project.yaml but missing from inputs.local.yaml paths",
            id
          ))
        }

        path <- local_inputs$paths[[id]]
        chk::chk_scalar(path)
        chk::chk_character(path, x_name = sprintf("Path for input '%s'", id))

        # Convert to absolute path if relative
        if (!fs::is_absolute_path(path)) {
          path <- normalizePath(file.path(self$path, path), mustWork = FALSE)
        } else {
          path <- normalizePath(path, mustWork = FALSE)
        }

        data_list[[id]] <- PMData$new(id = id, path = path)
      }

      data_list
    },

    #' @description
    #' Get all analysis names from the project.
    #' Returns the names of all valid analysis folders in the analyses directory.
    #'
    #' @return Character vector of analysis names
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' invisible(pm$create_analysis("data_preparation"))
    #' invisible(pm$create_analysis("modeling"))
    #' pm$list_analyses()
    list_analyses = function() {
      analyses_dir <- private$at(constants$ANALYSES_DIR)
      if (!dir.exists(analyses_dir)) {
        return(character(0))
      }

      # Get all subdirectories in analyses folder
      all_dirs <- list.dirs(analyses_dir, full.names = FALSE, recursive = FALSE)

      # Filter to only valid analyses
      valid_analyses <- character(0)
      for (dir_name in all_dirs) {
        analysis_path <- file.path(analyses_dir, dir_name)
        # Check if it's a valid analysis by trying to create it (validation happens in initialize)
        analysis <- tryCatch(
          PMAnalysis$new(path = analysis_path),
          error = function(e) NULL
        )
        if (!is.null(analysis)) {
          valid_analyses <- c(valid_analyses, dir_name)
        }
      }

      valid_analyses
    },

    #' @description
    #' Get an analysis object by name.
    #' Returns a PMAnalysis object for the specified analysis name.
    #'
    #' @param name Character. Name of the analysis (folder name within analyses/).
    #'
    #' @return \code{PMAnalysis} object for the specified analysis
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' invisible(pm$create_analysis("data_preparation"))
    #' analysis <- pm$get_analysis("data_preparation")
    #' analysis
    get_analysis = function(name) {
      chk::chk_scalar(name)
      chk::chk_character(name)

      analysis_path <- private$at(constants$ANALYSES_DIR, name)

      if (!dir.exists(analysis_path)) {
        stop(sprintf("Analysis '%s' does not exist in project", name))
      }

      # Try to create the analysis (validation happens in initialize)
      analysis <- tryCatch(
        PMAnalysis$new(project = self, name = name),
        error = function(e) {
          stop(sprintf("Analysis '%s' exists but is not valid: %s", name, conditionMessage(e)))
        }
      )

      analysis
    },

    #' @description
    #' Get an artifact (output file) from an analysis by ID.
    #' Searches for files with the given ID (filename without extension) in analysis output directories.
    #'
    #' @param id Character. The artifact ID (filename without extension).
    #' @param analysis_name Character. Optional name of the analysis to search in.
    #'   If not provided, searches all analyses and fails if not exactly one match is found.
    #'
    #' @return A \code{PMData} object with the artifact's ID and path.
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis <- pm$create_analysis("data_preparation")
    #'
    #' # Create a test output file
    #' output <- analysis$get_output_path("results.csv", type = "table")
    #' output$write(data.frame(x = 1:5))
    #'
    #' # Get artifact from specific analysis
    #' artifact <- pm$get_artifact("results", analysis_name = "data_preparation")
    #'
    #' # Get artifact without specifying analysis (if unique across all analyses)
    #' artifact <- pm$get_artifact("results")
    get_artifact = function(id, analysis_name = NULL) {
      chk::chk_scalar(id)
      chk::chk_character(id)

      # Determine which analyses to search
      if (!is.null(analysis_name)) {
        chk::chk_scalar(analysis_name)
        chk::chk_character(analysis_name)
        analysis_names <- analysis_name
      } else {
        analysis_names <- self$list_analyses()
        if (length(analysis_names) == 0) {
          stop("No analyses found in project")
        }
      }

      # Search through all specified analyses
      matching_artifacts <- lapply(analysis_names, function(aname) {
        analysis <- self$get_analysis(aname)
        outputs <- analysis$list_outputs(intermediate = FALSE)

        # Filter outputs matching the ID
        matching_outputs <- Filter(function(output) identical(output$id, id), outputs)

        # Create list entries for each matching output
        lapply(matching_outputs, function(output) list(analysis = aname, output = output))
      })

      # Flatten the nested list structure
      matching_artifacts <- unlist(matching_artifacts, recursive = FALSE)

      # Handle results
      if (length(matching_artifacts) == 0) {
        if (!is.null(analysis_name)) {
          stop(sprintf("No artifact with ID '%s' found in analysis '%s'", id, analysis_name))
        } else {
          stop(sprintf("No artifact with ID '%s' found in any analysis", id))
        }
      }

      if (length(matching_artifacts) > 1) {
        if (!is.null(analysis_name)) {
          # Multiple files with same ID in single analysis
          file_names <- vapply(matching_artifacts, function(x) basename(x$output$path), character(1))
          stop(sprintf(
            "Multiple artifacts with ID '%s' found in analysis '%s': %s",
            id, analysis_name, paste(file_names, collapse = ", ")
          ))
        } else {
          # Multiple matches across different analyses
          analysis_names_found <- vapply(matching_artifacts, function(x) x$analysis, character(1))
          stop(sprintf(
            "Multiple artifacts with ID '%s' found in analyses: %s. Please specify analysis_name.",
            id, paste(analysis_names_found, collapse = ", ")
          ))
        }
      }

      matching_artifacts[[1]]$output
    },

    #' @description
    #' Create a new analysis from template.
    #' Creates a new analysis folder with template structure including
    #' README.md, directories (code/, outputs/, intermediate/, logs/),
    #' and .gitignore file.
    #'
    #' @param name Character. Name of the analysis (will be the folder name).
    #'
    #' @return \code{PMAnalysis} object representing the newly created analysis
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #' analysis <- pm$create_analysis("data_preparation")
    #' analysis
    create_analysis = function(name) {
      chk::chk_scalar(name)
      chk::chk_character(name)

      # Check if analysis already exists
      analysis_path <- private$at(constants$ANALYSES_DIR, name)
      if (dir.exists(analysis_path)) {
        # Check if it's a valid analysis
        analysis <- tryCatch(
          PMAnalysis$new(project = self, name = name),
          error = function(e) NULL
        )
        if (!is.null(analysis)) {
          return(analysis)
        }

        # Folder exists but invalid, raise error
        stop(sprintf(
          "Analysis folder '%s' already exists but is not a valid analysis",
          name
        ))
      }

      # Create analysis directory
      dir.create(analysis_path, showWarnings = FALSE, recursive = TRUE)

      # Copy template structure
      template_dir <- system.file("extdata", constants$TEMPLATE_ANALYSIS_DIR, package = "pm")
      .recursive_copy(template_dir, analysis_path)

      # Replace {{ANALYSIS_NAME}} placeholder in README.md
      readme_path <- file.path(analysis_path, constants$README_FILENAME)
      if (file.exists(readme_path)) {
        readme_content <- readLines(readme_path)
        readme_content <- gsub("{{ANALYSIS_NAME}}", name, readme_content, fixed = TRUE)
        writeLines(readme_content, readme_path)
      }

      # Return PMAnalysis object
      PMAnalysis$new(project = self, name = name)
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
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
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
  success <- file.copy(
    template_gitignore,
    file.path(path, ".gitignore")
  )
  if (!success) {
    stop("Failed to copy .gitignore template file")
  }

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

  for (file in list.files(from_dir, all.files = TRUE, no.. = TRUE)) {
    if (identical(basename(file), "dont_copy")) {
      next
    }

    file_path <- file.path(from_dir, file)
    # Skip directories - they are handled recursively below
    if (dir.exists(file_path)) {
      next
    }

    # Handle files starting with "dot_" - rename to "." prefix
    file_basename <- basename(file)
    if (startsWith(file_basename, "dot_")) {
      dest_name <- sub("^dot_", ".", file_basename)
      dest_path <- file.path(to_dir, dest_name)
    } else {
      dest_path <- file.path(to_dir, file_basename)
    }

    file.copy(file_path, dest_path, overwrite = TRUE)
  }

  for (inner_source_dir in list.dirs(from_dir, recursive = FALSE)) {
    if (identical(inner_source_dir, from_dir)) {
      next
    }

    inner_dest_dir <- file.path(to_dir, basename(inner_source_dir))
    dir.create(inner_dest_dir, showWarnings = FALSE)

    .recursive_copy(inner_source_dir, inner_dest_dir)
  }
}
