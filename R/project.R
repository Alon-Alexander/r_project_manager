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
    #' Also validates that all input files referenced in inputs.local.yaml exist.
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

      # Validate input files exist
      .validate_input_files(
        project_path = self$path,
        inputs_file = private$at(constants$INPUTS_FILENAME),
        local_inputs_file = private$at(constants$LOCAL_INPUTS_FILENAME)
      )
    },

    #' @description
    #' Parse inputs.yaml and inputs.local.yaml into PMData objects.
    #' Reads the inputs definition file and the local paths file
    #' and combines them to create a list of PMData objects.
    #'
    #' @return A list of PMData objects, one for each input defined in inputs.yaml
    #'   that has a corresponding path in inputs.local.yaml
    #'
    #' @examples
    #' folder <- withr::local_tempdir()
    #' pm <- pm_create_project(folder)
    #'
    #' # After configuring inputs.yaml and inputs.local.yaml:
    #' # data_list <- pm$parse_inputs()
    parse_inputs = function() {
      # Read inputs.yaml (portable definitions)
      inputs_file <- private$at(constants$INPUTS_FILENAME)
      chk::check_files(inputs_file, x_name = "Inputs definition file")
      inputs_def <- tryCatch(
        yaml::read_yaml(inputs_file),
        error = function(e) {
          stop("inputs.yaml must be a YAML object (key-value pairs): ", conditionMessage(e))
        }
      )

      # Validate inputs.yaml schema
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

      # Extract input IDs from inputs.yaml
      # Inputs are defined under "inputs" key as a named list
      input_ids <- .extract_input_ids(inputs_def)

      # Create PMData objects by combining IDs with paths from local file
      data_list <- list()
      for (id in input_ids) {
        if (!id %in% names(local_inputs$paths)) {
          stop(sprintf(
            "Input ID '%s' is defined in inputs.yaml but missing from inputs.local.yaml paths",
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

  for (file in list.files(from_dir)) {
    if (identical(basename(file), "dont_copy")) {
      next
    }

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
