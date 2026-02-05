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
        self$name <- basename(self$path)

        # Try to infer project path (parent of analyses directory)
        parent <- dirname(self$path)
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

      self$project$get_artifact(id = id, analysis_name = analysis_name)
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
    },

    #' @description
    #' Run a function in SLURM.
    #' Submits the function as a SLURM job and returns a PMSlurmRun object
    #' that can be used to check status and retrieve results.
    #'
    #' @param fun Function. The function to run in SLURM.
    #' @param ... Additional arguments to pass to the function (can be positional or named).
    #' @param result_id Character. Optional ID for the result file (default: "slurm_result").
    #'   Must be provided by name if function arguments are passed positionally.
    #' @param config List. SLURM configuration with optional elements:
    #'   - `job_name`: Character. Name for the SLURM job (default: analysis name).
    #'   - `time_limit`: Character. SLURM time limit (default: "01:00:00").
    #'   - `memory`: Character. SLURM memory limit (default: "4G").
    #'   - `cpus`: Integer. Number of CPUs to request (default: 1).
    #'   - `slurm_flags`: Character. Additional SLURM flags (default: "").
    #'   - `modules`: Character vector. Modules to load (default: NULL).
    #'   - `store_image`: Logical. Whether to save and load R workspace image (default: TRUE).
    #'
    #' @return A \code{PMSlurmRun} object with the job ID and paths.
    #'
    #' @details
    #' This method:
    #' - Checks if SLURM is available
    #' - Creates an R script that runs the function and saves results
    #' - Creates a SLURM script from the template
    #' - Submits the job
    #' - Returns a PMSlurmRun object for monitoring
    #'
    #' Results are stored in the intermediate folder.
    #' All temporary files (function files, scripts) are stored in a temporary directory.
    #'
    #' Function arguments can be passed positionally or by name. If using positional
    #' arguments, `result_id` and `config` must be provided by name.
    #'
    #' @examples
    #' \dontrun{
    #' analysis <- pm$get_analysis("my_analysis")
    #'
    #' # With named arguments
    #' slurm_run <- analysis$run_in_slurm(function(x, y) {
    #'   result <- compute_something(x, y)
    #'   return(result)
    #' }, x = 10, y = 20)
    #'
    #' # With positional arguments
    #' slurm_run <- analysis$run_in_slurm(function(x) {
    #'   return(2 * x)
    #' }, 10, result_id = "my_result")
    #'
    #' # Check if done
    #' slurm_run$is_done()
    #'
    #' # Get results (with optional timeout to wait)
    #' results <- slurm_run$get_results(timeout = 60)
    #' }
    run_in_slurm = function(fun, ..., result_id = NULL, config = list()) {
      # Check if SLURM is available
      if (!is_slurm_available()) {
        stop("SLURM is not available on this system. Install SLURM or use mock mode for testing.")
      }

      # Validate inputs
      chk::chk_function(fun)

      # Extract function arguments from ...
      # Use match.call to separate function args from method args
      call_obj <- match.call(expand.dots = FALSE)
      dots <- call_obj$...

      # Get function arguments (everything in ...)
      fun_args <- if (is.null(dots)) {
        list()
      } else {
        # Evaluate the dots in the calling environment
        parent_env <- parent.frame()
        lapply(dots, function(x) eval(x, envir = parent_env))
      }

      # Validate method parameters
      if (is.null(result_id)) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        result_id <- paste0("slurm_result_", timestamp)
      }
      chk::chk_scalar(result_id)
      chk::chk_character(result_id)
      chk::chk_list(config)
      
      # Get store_image from config (default TRUE)
      store_image <- config$store_image
      if (is.null(store_image)) {
        store_image <- TRUE
      }
      chk::chk_flag(store_image)

      # Parse config with defaults
      job_name <- config$job_name %||% self$name
      time_limit <- config$time_limit %||% "01:00:00"
      memory <- config$memory %||% "32G"
      cpus <- config$cpus %||% 1L
      slurm_flags <- config$slurm_flags %||% ""
      modules <- config$modules
      
      # Ensure R is always in modules (needed for Rscript)
      if (is.null(modules) || length(modules) == 0) {
        modules <- c("R")
      } else {
        # Validate modules before processing
        chk::chk_character(modules)
        # Check if R or R/... is in modules
        has_r <- any(grepl("^R(/|$)", modules))
        if (!has_r) {
          modules <- c("R", modules)
        }
      }

      # Validate config values
      chk::chk_scalar(job_name)
      chk::chk_character(job_name)
      chk::chk_scalar(time_limit)
      chk::chk_character(time_limit)
      chk::chk_scalar(memory)
      chk::chk_character(memory)
      chk::chk_scalar(cpus)
      chk::chk_whole_number(cpus)
      chk::chk_scalar(slurm_flags)
      chk::chk_character(slurm_flags)

      # Get paths
      logs_dir <- file.path(self$path, "logs")
      intermediate_dir <- file.path(self$path, constants$ANALYSIS_INTERMEDIATE_DIR)

      # Ensure directories exist
      dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
      dir.create(intermediate_dir, showWarnings = FALSE, recursive = TRUE)

      # Create result path (use RDS format for direct loading)
      result_data <- self$get_output_path(result_id, type = "rds", intermediate = TRUE)
      result_path <- result_data$path

      # Create .slurm base directory in analysis folder (hidden)
      slurm_base_dir <- file.path(self$path, ".slurm")
      dir.create(slurm_base_dir, showWarnings = FALSE, recursive = TRUE)

      # Create job-specific subdirectory with timestamp and result_id
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (!is.null(result_id) && nzchar(result_id)) {
        # Sanitize result_id for use in directory name (remove invalid chars)
        safe_result_id <- gsub("[^A-Za-z0-9_-]", "_", result_id)
        job_dir_name <- paste0("job_", timestamp, "_", safe_result_id)
      } else {
        job_dir_name <- paste0("job_", timestamp)
      }
      slurm_dir <- file.path(slurm_base_dir, job_dir_name)
      dir.create(slurm_dir, showWarnings = FALSE, recursive = TRUE)

      # Save the function and its arguments to RDS files
      fun_rds_path <- file.path(slurm_dir, "fun.rds")
      args_rds_path <- file.path(slurm_dir, "args.rds")
      saveRDS(fun, fun_rds_path)
      saveRDS(fun_args, args_rds_path)
      
      # Extract and save loaded packages
      loaded_packages <- grep("^package:", search(), value = TRUE)
      loaded_packages <- sub("^package:", "", loaded_packages)
      # Filter out base packages that are always available
      base_packages <- c("base", "datasets", "graphics", "grDevices", "methods", "stats", "stats4", "tools", "utils")
      loaded_packages <- setdiff(loaded_packages, base_packages)
      packages_rds_path <- file.path(slurm_dir, "packages.rds")
      saveRDS(loaded_packages, packages_rds_path)
      
      # Save R workspace image if requested
      image_path <- NULL
      if (store_image) {
        image_path <- file.path(slurm_dir, "workspace.RData")
        save.image(file = image_path)
      }

      # Get paths to template files
      slurm_template_path <- system.file("extdata", "template_slurm.sh", package = "pm")
      r_template_path <- system.file("extdata", "template_slurm_r.R", package = "pm")

      if (!file.exists(slurm_template_path)) {
        stop("SLURM template file not found. Please reinstall the package.")
      }
      if (!file.exists(r_template_path)) {
        stop("SLURM R script template not found. Please reinstall the package.")
      }

      # Copy R script template to job directory
      r_script_path <- file.path(slurm_dir, "slurm_run.R")
      file.copy(r_template_path, r_script_path, overwrite = TRUE)
      Sys.chmod(r_script_path, mode = "0755")

      # Copy SLURM template to job directory
      slurm_script_path <- file.path(slurm_dir, "slurm_job.sh")
      file.copy(slurm_template_path, slurm_script_path, overwrite = TRUE)
      Sys.chmod(slurm_script_path, mode = "0755")

      # Handle extra SLURM flags
      if (nzchar(slurm_flags)) {
        # Split flags by newline and add #SBATCH prefix if needed
        flags_lines <- strsplit(slurm_flags, "\n")[[1]]
        flags_lines <- trimws(flags_lines)
        flags_lines <- flags_lines[nzchar(flags_lines)]
        flags_lines <- ifelse(
          startsWith(flags_lines, "#SBATCH"),
          flags_lines,
          paste0("#SBATCH ", flags_lines)
        )
        slurm_extra_flags <- paste(flags_lines, collapse = "\n")
      } else {
        slurm_extra_flags <- ""
      }

      # Set environment variables and submit job
      # Pass modules as space-separated string (will be parsed in bash script)
      modules_str <- paste(modules, collapse = " ")
      env_vars <- c(
        PM_JOB_NAME = job_name,
        PM_LOG_DIR = logs_dir,
        PM_TIME_LIMIT = time_limit,
        PM_MEMORY = memory,
        PM_CPUS = as.character(cpus),
        PM_WORK_DIR = self$path,
        PM_R_SCRIPT_PATH = r_script_path,
        PM_FUN_FILE = fun_rds_path,
        PM_ARGS_FILE = args_rds_path,
        PM_RESULT_FILE = result_path,
        PM_MODULES = modules_str,
        PM_SLURM_EXTRA_FLAGS = slurm_extra_flags,
        PM_IMAGE_FILE = if (!is.null(image_path)) image_path else ""
      )

      # Submit job with environment variables
      job_id <- .submit_slurm_job_with_env(slurm_script_path, env_vars)

      # Determine log path (SLURM will create it with job ID)
      log_path <- file.path(logs_dir, paste0("slurm-", job_id, ".out"))
      error_log_path <- file.path(logs_dir, paste0("slurm-", job_id, ".err"))

      # Return PMSlurmRun object
      PMSlurmRun$new(
        job_id = job_id,
        analysis_path = self$path,
        result_path = result_path,
        log_path = log_path,
        error_log_path = error_log_path
      )
    }
  ),
  active = list(
    #' @field project (`PMProject`)\cr
    #' Get the project containing this analysis
    project = function() {
      if (is.null(self$project_path)) {
        stop("This analysis is not associated with a project")
      }

      PMProject$new(self$project_path)
    }
  )
)

#' @title Infer an analysis object based on the current directory
#'
#' @description
#' Find the relevant analysis object based on the current directory.
#' Currently supported being called from an analysis folder inside
#' "analyses" folder, or from the "code" folder of an analysis.
#'
#' @return \code{PMAnalysis} object representing the inferred analysis
#'
#' @examples
#' empty_folder <- withr::local_tempdir()
#' pm <- pm_create_project(empty_folder)
#' pm$create_analysis("my_analysis")
#'
#' # Infer from analysis folder
#' withr::with_dir(file.path(empty_folder, "analyses", "my_analysis"), {
#'   analysis1 <- pm_infer_analysis()
#'   analysis1
#' })
#'
#' # Infer from code folder
#' withr::with_dir(file.path(empty_folder, "analyses", "my_analysis", "code"), {
#'   analysis2 <- pm_infer_analysis()
#'   analysis2
#' })
#'
#' @export
pm_infer_analysis <- function() {
  current_path <- normalizePath(getwd(), mustWork = FALSE)
  parent <- dirname(current_path)

  # Check if directly in analyses folder
  if (basename(parent) == constants$ANALYSES_DIR) {
    return(PMAnalysis$new(path = current_path))
  }

  # Check if in "code" folder in an analysis folder
  if (basename(current_path) == "code") {
    grandparent <- dirname(parent)
    if (basename(grandparent) == constants$ANALYSES_DIR) {
      return(PMAnalysis$new(path = parent))
    }
  }

  stop("Couldn't infer analysis path from current folder, please provide a direct path")
}
