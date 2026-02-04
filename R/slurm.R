#' @title SLURM Run class to manage SLURM job execution
#'
#' @description
#' This object represents a SLURM job that has been submitted.
#' It provides methods to check job status, get results, and cancel the job.
#'
#' @field job_id Character. The SLURM job ID
#' @field analysis_path Character. Path to the analysis folder
#' @field result_path Character. Path where results will be stored
#' @field log_path Character. Path to the SLURM log file
#'
#' @examples
#' \dontrun{
#' # Run a function in SLURM
#' analysis <- pm$get_analysis("my_analysis")
#' slurm_run <- analysis$run_in_slurm(function() {
#'   # Your analysis code here
#'   result <- compute_something()
#'   return(result)
#' })
#'
#' # Check if job is done
#' slurm_run$is_done()
#'
#' # Get results (errors if not done)
#' results <- slurm_run$get_results()
#'
#' # Cancel the job
#' slurm_run$cancel()
#' }
#'
#' @importFrom R6 R6Class
#' @export
PMSlurmRun <- R6Class("PMSlurmRun",
  public = list(
    job_id = NULL,
    analysis_path = NULL,
    result_path = NULL,
    log_path = NULL,

    #' @description
    #' Create a PMSlurmRun object
    #'
    #' @param job_id Character. The SLURM job ID
    #' @param analysis_path Character. Path to the analysis folder
    #' @param result_path Character. Path where results will be stored
    #' @param log_path Character. Path to the SLURM log file
    initialize = function(job_id, analysis_path, result_path, log_path) {
      chk::chk_scalar(job_id)
      chk::chk_character(job_id)
      chk::chk_scalar(analysis_path)
      chk::chk_character(analysis_path)
      chk::chk_scalar(result_path)
      chk::chk_character(result_path)
      chk::chk_scalar(log_path)
      chk::chk_character(log_path)

      self$job_id <- job_id
      self$analysis_path <- normalizePath(analysis_path, mustWork = FALSE)
      self$result_path <- normalizePath(result_path, mustWork = FALSE)
      self$log_path <- normalizePath(log_path, mustWork = FALSE)
    },

    #' @description
    #' Print method for PMSlurmRun
    print = function() {
      cat("PMSlurmRun:\n")
      cat("  Job ID: ", self$job_id, "\n", sep = "")
      cat("  Analysis Path: ", self$analysis_path, "\n", sep = "")
      cat("  Result Path: ", self$result_path, "\n", sep = "")
      if (self$is_done()) {
        status <- if (self$is_successful()) "Completed" else "Failed"
        cat("  Status: ", status, "\n", sep = "")
      } else {
        cat("  Status: Running\n", sep = "")
      }
      invisible(self)
    },

    #' @description
    #' Check if the SLURM job is done
    #'
    #' @return Logical. TRUE if the job is done, FALSE otherwise.
    is_done = function() {
      .check_slurm_job_done(self$job_id)
    },

    #' @description
    #' Check if the SLURM job completed successfully
    #'
    #' @return Logical. TRUE if the job completed successfully, FALSE if it failed or is still running.
    #'
    #' @details
    #' Returns FALSE if the job is still running or if it failed.
    #' Only returns TRUE if the job completed successfully.
    is_successful = function() {
      if (!self$is_done()) {
        return(FALSE)
      }
      .check_slurm_job_success(self$job_id, self$log_path, self$result_path)
    },

    #' @description
    #' Get the results from the SLURM job
    #'
    #' @param timeout Numeric. Optional timeout in seconds. If provided, will wait
    #'   for the job to complete (blocking) up to the timeout duration before failing.
    #'   If NULL (default), returns immediately if job is not done.
    #'
    #' @return The results object (loaded from the result file)
    #'
    #' @details
    #' If timeout is provided, this method will block and wait for the job to complete.
    #' If the job fails or times out, an error is raised with details.
    #' The results are loaded from the intermediate folder.
    get_results = function(timeout = NULL) {
      # If timeout is provided, wait for job to complete
      if (!is.null(timeout)) {
        chk::chk_scalar(timeout)
        chk::chk_numeric(timeout)
        chk::chk_gte(timeout, 0)
        
        start_time <- Sys.time()
        while (!self$is_done()) {
          elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          if (elapsed >= timeout) {
            stop(sprintf("Timeout waiting for job %s to complete (waited %.1f seconds)", 
                        self$job_id, elapsed))
          }
          Sys.sleep(0.5)  # Check every 0.5 seconds
        }

        check_count <- 6
      } else {
        # No timeout - check immediately
        if (!self$is_done()) {
          stop(sprintf("Job %s is not done yet. Use is_done() to check status or provide a timeout.", 
                      self$job_id))
        }

        check_count <- 1
      }

      for (i in 1:check_count - 1) {
        # Check if job was successful
        if (self$is_successful()) {
          break
        }
        Sys.sleep(0.5)  # Wait 0.5 seconds before checking again to allow system to flush files
      }

      # Check if job was successful
      if (!self$is_successful()) {
        error_msg <- .get_slurm_job_error(self$job_id, self$log_path)
        stop(sprintf("Job %s failed. %s", self$job_id, error_msg))
      }

      if (!file.exists(self$result_path)) {
        stop(sprintf("Result file not found at %s. Job may have failed.", self$result_path))
      }

      # Load the result file
      pm_read_file(self$result_path)
    },

    #' @description
    #' Cancel the SLURM job
    #'
    #' @return Invisibly returns TRUE if successful
    cancel = function() {
      .cancel_slurm_job(self$job_id)
      invisible(TRUE)
    }
  )
)

#' @title Check if SLURM is available on the system
#'
#' @description
#' Checks if SLURM commands (sbatch, squeue, scancel) are available.
#'
#' @return Logical. TRUE if SLURM is available, FALSE otherwise.
#'
#' @details
#' This function checks for the presence of sbatch, squeue, and scancel commands.
#'
#' @export
is_slurm_available <- function() {
  # Check for real SLURM commands
  has_sbatch <- nzchar(Sys.which("sbatch"))
  has_squeue <- nzchar(Sys.which("squeue"))
  has_scancel <- nzchar(Sys.which("scancel"))

  has_sbatch && has_squeue && has_scancel
}

#' @title Submit a SLURM job with environment variables
#'
#' @description
#' Submits a SLURM job using sbatch with environment variables and returns the job ID.
#'
#' @param slurm_script_path Character. Path to the SLURM script file
#' @param env_vars Named character vector. Environment variables to set
#'
#' @return Character. The SLURM job ID
#'
#' @keywords internal
.submit_slurm_job_with_env <- function(slurm_script_path, env_vars) {
  chk::check_files(slurm_script_path, x_name = "SLURM script file")
  chk::chk_named(env_vars)

  # Real SLURM submission with environment variables and SLURM directives
  # Extract SLURM-specific parameters for sbatch command-line arguments
  sbatch_args <- character(0)
  
  # Add SLURM directives as command-line arguments
  if ("PM_JOB_NAME" %in% names(env_vars)) {
    sbatch_args <- c(sbatch_args, paste0("--job-name=", env_vars[["PM_JOB_NAME"]]))
  }
  if ("PM_LOG_DIR" %in% names(env_vars)) {
    log_dir <- env_vars[["PM_LOG_DIR"]]
    sbatch_args <- c(sbatch_args, paste0("--output=", log_dir, "/slurm-%j.out"))
    sbatch_args <- c(sbatch_args, paste0("--error=", log_dir, "/slurm-%j.err"))
  }
  if ("PM_TIME_LIMIT" %in% names(env_vars)) {
    sbatch_args <- c(sbatch_args, paste0("--time=", env_vars[["PM_TIME_LIMIT"]]))
  }
  if ("PM_MEMORY" %in% names(env_vars)) {
    sbatch_args <- c(sbatch_args, paste0("--mem=", env_vars[["PM_MEMORY"]]))
  }
  if ("PM_CPUS" %in% names(env_vars)) {
    sbatch_args <- c(sbatch_args, paste0("--cpus-per-task=", env_vars[["PM_CPUS"]]))
  }
  
  # Handle extra SLURM flags (parse and add as arguments)
  if ("PM_SLURM_EXTRA_FLAGS" %in% names(env_vars) && nzchar(env_vars[["PM_SLURM_EXTRA_FLAGS"]])) {
    extra_flags <- env_vars[["PM_SLURM_EXTRA_FLAGS"]]
    # Split by newline and extract flags
    flag_lines <- strsplit(extra_flags, "\n")[[1]]
    flag_lines <- trimws(flag_lines)
    flag_lines <- flag_lines[nzchar(flag_lines)]
    for (flag_line in flag_lines) {
      # Remove #SBATCH prefix if present
      flag_line <- sub("^#SBATCH\\s+", "", flag_line)
      if (nzchar(flag_line)) {
        sbatch_args <- c(sbatch_args, flag_line)
      }
    }
  }
  
  # Filter out non-SLURM environment variables for export
  slurm_env_vars <- env_vars[!names(env_vars) %in% c("PM_JOB_NAME", "PM_LOG_DIR", "PM_TIME_LIMIT", 
                                                      "PM_MEMORY", "PM_CPUS", "PM_SLURM_EXTRA_FLAGS")]
  
  # Export environment variables before sbatch (they will be inherited)
  env_strings <- character(length(slurm_env_vars))
  for (i in seq_along(slurm_env_vars)) {
    name <- names(slurm_env_vars)[i]
    value <- slurm_env_vars[[i]]
    # Escape single quotes in values
    value_escaped <- gsub("'", "'\\''", value)
    env_strings[i] <- paste0(name, "='", value_escaped, "'")
  }
  env_cmd <- if (length(env_strings) > 0) {
    paste(env_strings, collapse = " ")
  } else {
    ""
  }
  
  # Build command: export env vars, then sbatch with arguments
  # We need to export environment variables in the shell, then call sbatch
  if (length(slurm_env_vars) > 0) {
    # Export environment variables and run sbatch
    full_cmd <- paste(env_cmd, "sbatch", paste(sbatch_args, collapse = " "), shQuote(slurm_script_path))
    result <- system(full_cmd, intern = TRUE)
  } else {
    # No environment variables to export, just call sbatch directly
    result <- system2("sbatch", args = c(sbatch_args, slurm_script_path), stdout = TRUE, stderr = TRUE)
  }
  
  if (attr(result, "status") != 0 && !is.null(attr(result, "status"))) {
    stop(sprintf("Failed to submit SLURM job: %s", paste(result, collapse = "\n")))
  }

  # Extract job ID from output (format: "Submitted batch job 12345")
  output <- paste(result, collapse = " ")
  job_id_match <- regmatches(output, regexpr("\\d+", output))
  
  if (length(job_id_match) == 0) {
    stop("Could not extract job ID from sbatch output")
  }

  job_id_match[1]
}

#' @title Check if a SLURM job is done
#'
#' @description
#' Checks if a SLURM job with the given job ID is done.
#'
#' @param job_id Character. The SLURM job ID
#'
#' @return Logical. TRUE if the job is done, FALSE otherwise.
#'
#' @keywords internal
.check_slurm_job_done <- function(job_id) {
  chk::chk_scalar(job_id)
  chk::chk_character(job_id)

  # Real SLURM check
  result <- system2("squeue", args = c("-j", job_id, "-h", "-o", "%T"), stdout = TRUE, stderr = TRUE)
  
  # If squeue returns nothing, the job is done
  if (length(result) == 0 || all(result == "")) {
    return(TRUE)
  }

  # Check if job status indicates it's done
  status <- paste(result, collapse = " ")
  done_statuses <- c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "NODE_FAIL")
  any(done_statuses %in% status)
}

#' @title Check if a SLURM job completed successfully
#'
#' @description
#' Checks if a SLURM job completed successfully (not just done, but succeeded).
#'
#' @param job_id Character. The SLURM job ID
#' @param log_path Character. Path to the log file
#' @param result_path Character. Path to the result file
#'
#' @return Logical. TRUE if the job completed successfully, FALSE otherwise.
#'
#' @keywords internal
.check_slurm_job_success <- function(job_id, log_path, result_path) {
  chk::chk_scalar(job_id)
  chk::chk_character(job_id)
  chk::chk_scalar(log_path)
  chk::chk_character(log_path)
  chk::chk_scalar(result_path)
  chk::chk_character(result_path)

  # Real SLURM: check job state
  result <- system2("squeue", args = c("-j", job_id, "-h", "-o", "%T"), stdout = TRUE, stderr = TRUE)
  
  # If squeue returns nothing, job is done - check completion status
  if (length(result) == 0 || all(result == "")) {
    # Check error log first for timeout or failure indicators
    error_log_path <- sub("\\.out$", ".err", log_path)
    if (file.exists(error_log_path)) {
      error_content <- readLines(error_log_path, warn = FALSE)
      # Check for timeout indicators (SLURM writes this to error log)
      if (any(grepl("DUE TO TIME LIMIT|TIME LIMIT|TIMEOUT|exceeded time limit", error_content, ignore.case = TRUE))) {
        return(FALSE)
      }
      if (any(grepl("error|Error|ERROR|failed|Failed|FAILED", error_content))) {
        return(FALSE)
      }
    }
    # Check output log for timeout
    if (file.exists(log_path)) {
      log_content <- readLines(log_path, warn = FALSE)
      if (any(grepl("DUE TO TIME LIMIT|TIME LIMIT|TIMEOUT|exceeded time limit", log_content, ignore.case = TRUE))) {
        return(FALSE)
      }
    }
    # If no timeout indicators and result file exists, consider successful
    if (file.exists(result_path)) {
      return(TRUE)
    }
    return(FALSE)
  }

  # Check job status from squeue
  status <- paste(result, collapse = " ")
  if (grepl("COMPLETED", status, ignore.case = TRUE)) {
    return(file.exists(result_path))
  }
  
  # Check for timeout or other failure statuses
  failure_statuses <- c("FAILED", "CANCELLED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY", "DEADLINE")
  if (any(sapply(failure_statuses, function(s) grepl(s, status, ignore.case = TRUE)))) {
    return(FALSE)
  }
  
  # If job is done but status unclear, check result file
  if (file.exists(result_path)) {
    return(TRUE)
  }
  
  # Any other status means failure
  FALSE
}

#' @title Get error message from SLURM job
#'
#' @description
#' Extracts error message from SLURM job logs.
#'
#' @param job_id Character. The SLURM job ID
#' @param log_path Character. Path to the log file
#'
#' @return Character. Error message or empty string if no error found.
#'
#' @keywords internal
.get_slurm_job_error <- function(job_id, log_path) {
  error_log_path <- sub("\\.out$", ".err", log_path)
  
  error_msg <- character(0)
  
  # Check error log
  if (file.exists(error_log_path)) {
    error_content <- readLines(error_log_path, warn = FALSE)
    if (length(error_content) > 0) {
      error_msg <- c(error_msg, paste("Error log:", paste(error_content, collapse = "\n")))
    }
  }
  
  # Check output log for errors
  if (file.exists(log_path)) {
    log_content <- readLines(log_path, warn = FALSE)
    error_lines <- grep("error|Error|ERROR|failed|Failed|FAILED", log_content, value = TRUE, ignore.case = TRUE)
    if (length(error_lines) > 0) {
      error_msg <- c(error_msg, paste("Output log errors:", paste(error_lines, collapse = "\n")))
    }
  }
  
  if (length(error_msg) == 0) {
    return("No error details found in logs.")
  }
  
  paste(error_msg, collapse = "\n")
}

#' @title Cancel a SLURM job
#'
#' @description
#' Cancels a SLURM job with the given job ID.
#'
#' @param job_id Character. The SLURM job ID
#'
#' @return Invisibly returns TRUE if successful
#'
#' @keywords internal
.cancel_slurm_job <- function(job_id) {
  chk::chk_scalar(job_id)
  chk::chk_character(job_id)

  # Real SLURM cancellation
  result <- system2("scancel", args = job_id, stdout = TRUE, stderr = TRUE)
  
  if (attr(result, "status") != 0 && !is.null(attr(result, "status"))) {
    stop(sprintf("Failed to cancel SLURM job %s: %s", job_id, paste(result, collapse = "\n")))
  }

  invisible(TRUE)
}
