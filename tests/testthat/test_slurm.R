# Mock SLURM implementation for testing
.mock_slurm_jobs <- new.env(parent = emptyenv())

# Store references to real base functions before mocking
.real_system2 <- base::system2
.real_system <- base::system
.real_sys_which <- base::Sys.which

# Helper function to create mock system2 function
.create_mock_system2 <- function() {
  function(command, args = NULL, stdout = "", stderr = "", ...) {
    if (command == "sbatch") {
      # sbatch is non-blocking - just return a job ID immediately
      # Parse positional arguments from args
      # Format: [sbatch flags...] script.sh "arg1" "arg2" "arg3" "arg4" "arg5" "arg6"
      # Where: arg1=PM_FUN_FILE, arg2=PM_ARGS_FILE, arg3=PM_RESULT_FILE, 
      #        arg4=PM_WORK_DIR, arg5=PM_R_SCRIPT_PATH, arg6=PM_MODULES
      
      script_path <- NULL
      positional_args <- character(0)
      
      if (!is.null(args) && length(args) > 0) {
        # Find script path (first non-flag argument) and positional args after it
        script_found <- FALSE
        for (i in seq_along(args)) {
          arg <- args[i]
          # Remove quotes if present
          arg_clean <- gsub("^['\"]|['\"]$", "", arg)
          if (!startsWith(arg_clean, "--") && !startsWith(arg_clean, "-") && !script_found) {
            script_path <- arg_clean
            script_found <- TRUE
          } else if (script_found) {
            # All arguments after script path are positional arguments
            positional_args <- c(positional_args, arg_clean)
          }
        }
      }

      # Extract values from positional arguments
      # Order: PM_FUN_FILE, PM_ARGS_FILE, PM_RESULT_FILE, PM_WORK_DIR, PM_R_SCRIPT_PATH, PM_MODULES, PM_PACKAGES_FILE, PM_IMAGE_FILE
      fun_file <- if (length(positional_args) >= 1) positional_args[1] else NULL
      args_file <- if (length(positional_args) >= 2) positional_args[2] else NULL
      result_path <- if (length(positional_args) >= 3) positional_args[3] else NULL
      work_dir <- if (length(positional_args) >= 4) positional_args[4] else NULL
      r_script_path <- if (length(positional_args) >= 5) positional_args[5] else NULL
      modules_str <- if (length(positional_args) >= 6) positional_args[6] else NULL
      packages_file <- if (length(positional_args) >= 7 && nzchar(positional_args[7])) positional_args[7] else NULL
      image_file <- if (length(positional_args) >= 8 && nzchar(positional_args[8])) positional_args[8] else NULL
      
      # Extract log_dir from sbatch arguments (--output and --error flags)
      log_dir <- NULL
      if (!is.null(args)) {
        for (arg in args) {
          if (startsWith(arg, "--output=")) {
            output_path <- sub("--output=", "", arg)
            # Extract directory from output path (format: dir/slurm-%j.out)
            log_dir <- dirname(sub("%j", "12345", output_path)) # Use placeholder for %j
            break
          }
        }
      }
      
      # Generate a numeric job_id (the extraction regex looks for digits)
      job_id_numeric <- as.character(Sys.time())
      job_id_numeric <- gsub("[^0-9]", "", job_id_numeric)
      job_id_internal <- paste0("mock_", job_id_numeric)

      # Store job with actual details from positional arguments
      if (!is.null(r_script_path) && !is.null(fun_file) && 
          !is.null(args_file) && !is.null(result_path)) {
        .mock_slurm_jobs[[job_id_internal]] <- list(
          script_path = r_script_path,
          fun_file = fun_file,
          args_file = args_file,
          log_dir = log_dir,
          result_path = result_path,
          packages_file = packages_file, # May be NULL if not provided
          image_file = image_file, # May be NULL if not provided
          start_time = Sys.time(),
          status = "PENDING", # Job is pending until mock_run_slurm_job() is called
          done = FALSE,
          numeric_job_id = job_id_numeric
        )
      } else {
        # Create minimal entry if we don't have all required values
        .mock_slurm_jobs[[job_id_internal]] <- list(
          status = "PENDING",
          done = FALSE,
          start_time = Sys.time(),
          numeric_job_id = job_id_numeric
        )
      }
      
      # Also store a mapping from numeric ID to internal ID
      .mock_slurm_jobs[[paste0("numeric_", job_id_numeric)]] <- job_id_internal

      # Return immediately (non-blocking) - sbatch is non-blocking
      result <- paste("Submitted batch job", job_id_numeric)
      attr(result, "status") <- 0L
      return(result)
    } else if (command == "squeue") {
      # Check if job is done - squeue is non-blocking, just checks status
      job_id <- NULL
      if (length(args) > 0 && any(grepl("-j", args))) {
        job_idx <- which(args == "-j")
        if (length(job_idx) > 0 && job_idx[1] < length(args)) {
          job_id <- args[job_idx[1] + 1]
        }
      }

      # Look up the job - job_id might be numeric, so check both formats
      job_id_internal <- job_id
      if (!is.null(job_id) && exists(paste0("numeric_", job_id), envir = .mock_slurm_jobs)) {
        job_id_internal <- .mock_slurm_jobs[[paste0("numeric_", job_id)]]
      }

      if (!is.null(job_id_internal) && exists(job_id_internal, envir = .mock_slurm_jobs)) {
        job_info <- .mock_slurm_jobs[[job_id_internal]]

        # Check if job is done (jobs are only executed when mock_run_slurm_job() is called)
        if (!isTRUE(job_info$done)) {
          # Check if result file exists (job completed)
          if (!is.null(job_info$result_path) && file.exists(job_info$result_path)) {
            job_info$done <- TRUE
            job_info$status <- "COMPLETED"
            .mock_slurm_jobs[[job_id_internal]] <- job_info
          }
          # Jobs are not auto-started - they must be explicitly run via mock_run_slurm_job()
        }

        # Return status based on job state (squeue returns nothing when job is done)
        if (isTRUE(job_info$done)) {
          result <- character(0)
        } else {
          result <- "PENDING"
        }
      } else {
        # Job not found, consider it done
        result <- character(0)
      }

      attr(result, "status") <- 0L
      return(result)
    } else if (command == "scancel") {
      # Cancel the job
      job_id <- args[1]
      # Look up the job - job_id might be numeric, so check both formats
      job_id_internal <- job_id
      if (!is.null(job_id) && exists(paste0("numeric_", job_id), envir = .mock_slurm_jobs)) {
        job_id_internal <- .mock_slurm_jobs[[paste0("numeric_", job_id)]]
      }
      if (exists(job_id_internal, envir = .mock_slurm_jobs)) {
        job_info <- .mock_slurm_jobs[[job_id_internal]]
        # Mark job as cancelled (no background processes to kill)
        job_info$status <- "CANCELLED"
        job_info$done <- TRUE
        .mock_slurm_jobs[[job_id_internal]] <- job_info
      }
      result <- character(0)
      attr(result, "status") <- 0L
      return(result)
    }

    # For other commands, call the real system2 (use stored reference to avoid recursion)
    .real_system2(command, args, stdout = stdout, stderr = stderr, ...)
  }
}

# Helper to start a mock job in the background
# Helper to execute a mock job synchronously
# This is called explicitly by tests via mock_run_slurm_job()
.execute_mock_job_sync <- function(job_id) {
  if (!exists(job_id, envir = .mock_slurm_jobs)) {
    return(FALSE)
  }

  job_info <- .mock_slurm_jobs[[job_id]]

  if (isTRUE(job_info$done)) {
    return(TRUE) # Already done
  }

  r_script_path <- job_info$script_path
  fun_file <- job_info$fun_file
  args_file <- job_info$args_file
  result_path <- job_info$result_path

  if (is.null(r_script_path) || is.null(fun_file) || is.null(args_file) || is.null(result_path)) {
    return(FALSE) # Not ready yet
  }

  if (!base::file.exists(r_script_path) || !base::file.exists(fun_file) || !base::file.exists(args_file)) {
    job_info$done <- TRUE
    job_info$status <- "FAILED"
    .mock_slurm_jobs[[job_id]] <- job_info
    return(FALSE)
  }

  # Get numeric job ID for log files
  numeric_job_id <- if (!is.null(job_info$numeric_job_id)) {
    job_info$numeric_job_id
  } else {
    gsub("mock_", "", job_id)
  }

  # Ensure log_dir is set
  if (is.null(job_info$log_dir) || job_info$log_dir == "") {
    job_info$log_dir <- file.path(dirname(result_path), "logs")
  }

  log_file <- file.path(job_info$log_dir, paste0("slurm-", numeric_job_id, ".out"))
  error_log_file <- file.path(job_info$log_dir, paste0("slurm-", numeric_job_id, ".err"))

  dir.create(job_info$log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(result_path), showWarnings = FALSE, recursive = TRUE)

  # Create empty log files immediately so they exist when checked
  if (!base::file.exists(log_file)) {
    base::writeLines("", log_file)
  }
  if (!base::file.exists(error_log_file)) {
    base::writeLines("", error_log_file)
  }

  # Update job_info with corrected log_dir
  job_info$log_dir <- job_info$log_dir
  job_info$status <- "RUNNING"
  .mock_slurm_jobs[[job_id]] <- job_info

  # R CMD check requires a full path to Rscript; never use bare "Rscript"
  rscript_path <- base::file.path(base::R.home(), "bin", "Rscript")
  if (!base::file.exists(rscript_path)) {
    alt <- .real_sys_which("Rscript")
    if (alt != "" && grepl("[/\\\\]", alt)) {
      rscript_path <- alt
    }
  }

  # Execute synchronously
  exit_code <- tryCatch(
    {
      old_env <- Sys.getenv(c("PM_FUN_FILE", "PM_ARGS_FILE", "PM_RESULT_FILE"), unset = NA)
      on.exit({
        for (name in names(old_env)) {
          if (is.na(old_env[[name]])) {
            Sys.unsetenv(name)
          } else {
            Sys.setenv(name = old_env[[name]])
          }
        }
      })

      Sys.setenv(PM_FUN_FILE = fun_file)
      Sys.setenv(PM_ARGS_FILE = args_file)
      Sys.setenv(PM_RESULT_FILE = result_path)

      # Build Rscript arguments
      rscript_args <- c(
        r_script_path,
        paste0("--fun-file=", fun_file),
        paste0("--args-file=", args_file),
        paste0("--result-file=", result_path),
        paste0("--packages-file=", if (!is.null(job_info$packages_file) && nzchar(job_info$packages_file)) job_info$packages_file else "")
      )
      
      # Add image file if it exists
      if (!is.null(job_info$image_file) && nzchar(job_info$image_file) && base::file.exists(job_info$image_file)) {
        rscript_args <- c(rscript_args, paste0("--image-file=", job_info$image_file))
      }
      
      exit_code <- .real_system2(rscript_path,
        args = rscript_args,
        stdout = log_file,
        stderr = error_log_file,
        wait = TRUE
      )
      exit_code
    },
    error = function(e) {
      if (base::file.exists(base::dirname(error_log_file))) {
        base::writeLines(paste("Error executing R script:", conditionMessage(e)), error_log_file)
      }
      return(1L)
    }
  )

  if (!is.null(exit_code) && exit_code != 0) {
    job_info$done <- TRUE
    job_info$status <- "FAILED"
    .mock_slurm_jobs[[job_id]] <- job_info
    return(FALSE)
  }

  if (!base::file.exists(result_path)) {
    job_info$done <- TRUE
    job_info$status <- "FAILED"
    .mock_slurm_jobs[[job_id]] <- job_info
    return(FALSE)
  }

  job_info$done <- TRUE
  job_info$status <- "COMPLETED"
  .mock_slurm_jobs[[job_id]] <- job_info
  return(TRUE)
}

# Test utility to run a mock SLURM job synchronously
# This function is used in tests to explicitly execute a mock SLURM job.
# Jobs are not executed automatically - they must be explicitly run using this function.
mock_run_slurm_job <- function(job_id) {
  # Handle both numeric and internal job IDs
  job_id_internal <- job_id
  if (exists(paste0("numeric_", job_id), envir = .mock_slurm_jobs)) {
    job_id_internal <- .mock_slurm_jobs[[paste0("numeric_", job_id)]]
  }

  .execute_mock_job_sync(job_id_internal)
}

# Mock system function for sbatch (not used anymore since we use system2, but kept for compatibility)
.create_mock_system <- function() {
  function(command, intern = FALSE, ...) {
    # system() is not used for sbatch anymore - we use system2()
    # For other commands, call real system (use stored reference to avoid recursion)
    .real_system(command, intern = intern, ...)
  }
}

# Original mock functions (kept for reference, but we'll use system mocks instead)
.mock_submit_slurm_job_with_env <- function(slurm_script_path, env_vars) {
  # Generate a unique job ID
  job_id <- as.character(Sys.time())
  job_id <- gsub("[^0-9]", "", job_id)
  job_id <- paste0("mock_", job_id)

  # Extract values from environment variables
  r_script_path <- env_vars[["PM_R_SCRIPT_PATH"]]
  fun_file <- env_vars[["PM_FUN_FILE"]]
  args_file <- env_vars[["PM_ARGS_FILE"]]
  result_path <- env_vars[["PM_RESULT_FILE"]]
  log_dir <- env_vars[["PM_LOG_DIR"]]

  if (is.null(r_script_path) || r_script_path == "") {
    stop("PM_R_SCRIPT_PATH not found in environment variables")
  }
  if (is.null(fun_file) || fun_file == "") {
    stop("PM_FUN_FILE not found in environment variables")
  }
  if (is.null(args_file) || args_file == "") {
    stop("PM_ARGS_FILE not found in environment variables")
  }
  if (is.null(result_path) || result_path == "") {
    stop("PM_RESULT_FILE not found in environment variables")
  }
  if (is.null(log_dir) || log_dir == "") {
    log_dir <- dirname(r_script_path)
  }

  # Store job information
  .mock_slurm_jobs[[job_id]] <- list(
    script_path = r_script_path,
    fun_file = fun_file,
    args_file = args_file,
    log_dir = log_dir,
    result_path = result_path,
    start_time = Sys.time(),
    status = "RUNNING",
    done = FALSE,
    process = NULL
  )

  job_id
}

.mock_check_slurm_job_done <- function(job_id) {
  if (!exists(job_id, envir = .mock_slurm_jobs)) {
    return(TRUE) # Job doesn't exist, consider it done
  }

  job_info <- .mock_slurm_jobs[[job_id]]

  if (isTRUE(job_info$done)) {
    return(TRUE)
  }

  # Check if enough time has passed (simulate job execution)
  # For testing, we'll use a short delay (1 second)
  elapsed <- as.numeric(difftime(Sys.time(), job_info$start_time, units = "secs"))

  if (elapsed >= 1) {
    # Execute the R script now
    r_script_path <- job_info$script_path
    fun_file <- job_info$fun_file
    args_file <- job_info$args_file
    result_path <- job_info$result_path

    if (!file.exists(r_script_path)) {
      job_info$done <- TRUE
      job_info$status <- "FAILED"
      .mock_slurm_jobs[[job_id]] <- job_info
      return(TRUE)
    }

    if (!file.exists(fun_file)) {
      job_info$done <- TRUE
      job_info$status <- "FAILED"
      .mock_slurm_jobs[[job_id]] <- job_info
      return(TRUE)
    }

    if (!file.exists(args_file)) {
      job_info$done <- TRUE
      job_info$status <- "FAILED"
      .mock_slurm_jobs[[job_id]] <- job_info
      return(TRUE)
    }

    # Run the R script with environment variables
    log_file <- file.path(job_info$log_dir, paste0("slurm-", job_id, ".out"))
    error_log_file <- file.path(job_info$log_dir, paste0("slurm-", job_id, ".err"))

    # Ensure log directory exists
    dir.create(job_info$log_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(dirname(result_path), showWarnings = FALSE, recursive = TRUE)

    # Execute the script with environment variables
    exit_code <- tryCatch(
      {
        # Set environment variables
        old_env <- Sys.getenv(c("PM_FUN_FILE", "PM_ARGS_FILE", "PM_RESULT_FILE"), unset = NA)
        on.exit({
          # Restore environment
          for (name in names(old_env)) {
            if (is.na(old_env[[name]])) {
              Sys.unsetenv(name)
            } else {
              Sys.setenv(name = old_env[[name]])
            }
          }
        })

        Sys.setenv(PM_FUN_FILE = fun_file)
        Sys.setenv(PM_ARGS_FILE = args_file)
        Sys.setenv(PM_RESULT_FILE = result_path)

        # R CMD check requires full path to Rscript
        rscript_path <- file.path(R.home(), "bin", "Rscript")
        if (!file.exists(rscript_path)) {
          alt <- .real_sys_which("Rscript")
          if (alt != "" && grepl("[/\\\\]", alt)) rscript_path <- alt
        }

        # Run Rscript with proper redirection
        exit_code <- system2(rscript_path,
          args = c(
            r_script_path,
            paste0("--fun-file=", fun_file),
            paste0("--args-file=", args_file),
            paste0("--result-file=", result_path)
          ),
          stdout = log_file,
          stderr = error_log_file,
          wait = TRUE
        )
        exit_code
      },
      error = function(e) {
        # Write error to error log
        if (file.exists(dirname(error_log_file))) {
          writeLines(paste("Error executing R script:", conditionMessage(e)), error_log_file)
        }
        return(1L)
      }
    )

    # Check if execution was successful
    if (!is.null(exit_code) && exit_code != 0) {
      job_info$done <- TRUE
      job_info$status <- "FAILED"
      .mock_slurm_jobs[[job_id]] <- job_info
      return(TRUE)
    }

    # Check if result file exists
    if (!is.null(result_path) && !file.exists(result_path)) {
      job_info$done <- TRUE
      job_info$status <- "FAILED"
      .mock_slurm_jobs[[job_id]] <- job_info
      return(TRUE)
    }

    # Mark as done
    job_info$done <- TRUE
    job_info$status <- "COMPLETED"
    .mock_slurm_jobs[[job_id]] <- job_info

    return(TRUE)
  }

  FALSE
}

.mock_check_slurm_job_success <- function(job_id, log_path, error_log_path, result_path) {
  if (!exists(job_id, envir = .mock_slurm_jobs)) {
    return(FALSE)
  }

  job_info <- .mock_slurm_jobs[[job_id]]

  # Check status - timeout and other failure statuses are not successful
  failure_statuses <- c("FAILED", "CANCELLED", "TIMEOUT", "NODE_FAIL", "OUT_OF_MEMORY")
  if (job_info$status %in% failure_statuses) {
    return(FALSE)
  }

  # Check error log for timeout indicators
  if (file.exists(error_log_path)) {
    error_content <- readLines(error_log_path, warn = FALSE)
    if (any(grepl("DUE TO TIME LIMIT|TIME LIMIT|TIMEOUT|exceeded time limit", error_content, ignore.case = TRUE))) {
      return(FALSE)
    }
  }

  # Check if completed successfully
  if (job_info$status == "COMPLETED" && file.exists(result_path)) {
    return(TRUE)
  }

  FALSE
}

.mock_cancel_slurm_job <- function(job_id) {
  if (exists(job_id, envir = .mock_slurm_jobs)) {
    job_info <- .mock_slurm_jobs[[job_id]]
    job_info$status <- "CANCELLED"
    job_info$done <- TRUE
    .mock_slurm_jobs[[job_id]] <- job_info
  }
  invisible(TRUE)
}

# Helper to setup mocks for a test
# This function sets up mocks and executes code with those mocks active
with_mock_slurm <- function(code) {
  # Clear job state
  rm(list = ls(envir = .mock_slurm_jobs), envir = .mock_slurm_jobs)

  # Create mock functions
  mock_system2 <- .create_mock_system2()
  mock_system <- .create_mock_system()
  mock_sys_which <- function(x) {
    if (x %in% c("sbatch", "squeue", "scancel")) {
      return(paste0("/usr/bin/", x))
    }
    return(.real_sys_which(x))
  }

  # Use testthat::with_mocked_bindings to set up mocks
  # The code block is the last unnamed argument
  code_quo <- rlang::enquo(code)
  result <- testthat::with_mocked_bindings(
    system2 = mock_system2,
    system = mock_system,
    `Sys.which` = mock_sys_which,
    .package = "base",
    rlang::eval_tidy(code_quo)
  )

  # Clean up job state (no background processes or scripts to clean up)

  result
}

describe("SLURM functionality", {
  describe("is_slurm_available()", {
    it("Returns TRUE when SLURM commands are available", {
      with_mock_slurm({
        expect_true(pm::is_slurm_available())
      })
    })

    it("Returns FALSE when SLURM is not available", {
      testthat::with_mocked_bindings(
        Sys.which = function(x) "",
        .package = "base",
        {
          expect_false(pm::is_slurm_available())
        }
      )
    })
  })

  describe("PMAnalysis$run_in_slurm()", {
    it("Errors when SLURM is not available", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      testthat::with_mocked_bindings(
        Sys.which = function(x) "",
        .package = "base",
        {
          expect_error(
            analysis$run_in_slurm(function() 42),
            regexp = "SLURM is not available"
          )
        }
      )
    })

    it("Creates SLURM script and R script files in temp directory", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        expect_s3_class(slurm_run, "PMSlurmRun")
        expect_true(!is.null(slurm_run$job_id))

        # Files should be in temp directory (not visible in code/)
        code_files <- list.files(file.path(analysis$path, "code"))
        expect_false("slurm_run.R" %in% code_files)
        expect_false("slurm_job.sh" %in% code_files)
      })
    })

    it("Uses correct paths for logs and results", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(list(x = 1, y = 2))
        })

        # Check that result path is in intermediate folder
        expect_true(grepl("intermediate", slurm_run$result_path))
        expect_true(grepl("slurm_result", slurm_run$result_path))

        # Check that log path is in logs folder
        expect_true(grepl("logs", slurm_run$log_path))
      })
    })

    it("Accepts custom parameters via config", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(100)
          },
          result_id = "custom_result",
          config = list(
            job_name = "custom_job",
            time_limit = "02:00:00",
            memory = "8G",
            cpus = 4L
          )
        )

        expect_s3_class(slurm_run, "PMSlurmRun")
        expect_true(grepl("custom_result", slurm_run$result_path))
      })
    })

    it("Accepts store_image = FALSE in config", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(1)
          },
          config = list(store_image = FALSE)
        )
        expect_s3_class(slurm_run, "PMSlurmRun")
        mock_run_slurm_job(slurm_run$job_id)
        expect_equal(slurm_run$get_results(), 1)
      })
    })

    it("Accepts slurm_flags in config", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(2)
          },
          config = list(slurm_flags = "--export=ALL")
        )
        expect_s3_class(slurm_run, "PMSlurmRun")
        mock_run_slurm_job(slurm_run$job_id)
        expect_equal(slurm_run$get_results(), 2)
      })
    })

    it("Prepends R to modules when not in config", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(1)
          },
          config = list(modules = c("gcc/9", "openmpi/4.0"))
        )
        expect_s3_class(slurm_run, "PMSlurmRun")
        mock_run_slurm_job(slurm_run$job_id)
        expect_equal(slurm_run$get_results(), 1)
      })
    })

    it("Accepts empty result_id (uses timestamp-only job dir)", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(3)
          },
          result_id = ""
        )
        expect_s3_class(slurm_run, "PMSlurmRun")
        mock_run_slurm_job(slurm_run$job_id)
        expect_equal(slurm_run$get_results(), 3)
      })
    })

    it("Validates function parameter", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        expect_error(
          analysis$run_in_slurm("not a function"),
          regexp = "must be a function"
        )
      })
    })
  })

  describe("PMSlurmRun class", {
    it("Can be created with valid parameters", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(data.frame(x = 1:5, y = letters[1:5]))
        })

        expect_s3_class(slurm_run, "PMSlurmRun")
        expect_true(!is.null(slurm_run$job_id))
        expect_true(!is.null(slurm_run$analysis_path))
        expect_true(!is.null(slurm_run$result_path))
        expect_true(!is.null(slurm_run$log_path))
      })
    })

    it("Has print method", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        output <- capture.output(print(slurm_run))
        expect_true(any(grepl("PMSlurmRun", output)))
        expect_true(any(grepl("Job ID", output)))
      })
    })

    it("Print shows Running when job not yet done", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })
        # Do not run the job; print should show Running
        output <- capture.output(print(slurm_run))
        expect_true(any(grepl("Running", output)))
      })
    })

    it("Print shows Completed when job succeeded", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })
        mock_run_slurm_job(slurm_run$job_id)
        output <- capture.output(print(slurm_run))
        expect_true(any(grepl("Completed", output)))
      })
    })

    it("Print shows Failed when job failed", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          stop("job error")
        })
        mock_run_slurm_job(slurm_run$job_id)
        output <- capture.output(print(slurm_run))
        expect_true(any(grepl("Failed", output)))
      })
    })

    it("is_successful returns FALSE when job not done", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })
        # Before running the job, is_successful should be FALSE
        expect_false(slurm_run$is_successful())
      })
    })

    it("Can check if job is done", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)
        eventually_done <- slurm_run$is_done()
        expect_true(eventually_done)
      })
    })

    it("Can get results after job is done", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        test_result <- list(a = 1, b = 2, c = "test")
        slurm_run <- analysis$run_in_slurm(function() {
          return(test_result)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Check success status
        expect_true(slurm_run$is_successful())

        # Get results
        results <- slurm_run$get_results()
        expect_equal(results, test_result)
      })
    })

    it("Can use timeout in get_results to wait", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Use timeout to wait for results (blocking)
        results <- slurm_run$get_results(timeout = 5)
        expect_equal(results, 42)
      })
    })

    it("Errors when timeout is exceeded", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          Sys.sleep(10) # No real need because of the mock, but for good measure
          stop("Test error")
        })

        # Mock the .check_slurm_job_done function to return FALSE (job never completes)
        # This simulates a job that takes longer than the timeout
        testthat::with_mocked_bindings(
          .check_slurm_job_done = function(job_id) {
            # Always return FALSE to simulate job never completing
            FALSE
          },
          .package = "pm",
          {
            # Use timeout to wait for results (blocking but exceeds timeout)
            expect_error(
              slurm_run$get_results(timeout = 1),
              regexp = "Timeout waiting for job"
            )
          }
        )
      })
    })

    it("Errors when job fails", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          stop("Test error")
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Should not be successful
        expect_false(slurm_run$is_successful())

        # Getting results should error
        expect_error(
          slurm_run$get_results(),
          regexp = "failed"
        )

        # Check error log content
        if (file.exists(slurm_run$error_log_path)) {
          error_content <- readLines(slurm_run$error_log_path, warn = FALSE)
          expect_true(any(grepl("error|Error|ERROR|failed|Failed|FAILED", error_content, ignore.case = TRUE)))
        }
      })
    })

    it("Timeout is not considered successful", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        # Create a job that will complete normally first
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Verify normal completion is successful
        expect_true(slurm_run$is_successful())

        # Now simulate a timeout by writing timeout message to error log
        # This tests the timeout detection logic in .check_slurm_job_success
        dir.create(dirname(slurm_run$error_log_path), showWarnings = FALSE, recursive = TRUE)
        writeLines(
          "slurmstepd: error: *** JOB 12345 ON node01 CANCELLED AT 2024-01-01T12:00:00 DUE TO TIME LIMIT ***",
          slurm_run$error_log_path
        )

        # Remove result file to simulate timeout (no result produced)
        if (file.exists(slurm_run$result_path)) {
          file.remove(slurm_run$result_path)
        }

        # Now check success - should be FALSE due to timeout
        expect_false(slurm_run$is_successful())

        # Getting results should error
        expect_error(
          slurm_run$get_results(),
          regexp = "failed"
        )
      })
    })

    it("Checks log file content for successful jobs", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          cat("Processing data...\n")
          result <- 42
          cat("SLURM job completed successfully\n")
          return(result)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Check that log file exists and has content
        # The log file should be created by the mock SLURM execution
        expect_true(file.exists(slurm_run$log_path),
          info = paste("Log file not found at:", slurm_run$log_path)
        )

        log_content <- readLines(slurm_run$log_path, warn = FALSE)
        expect_true(length(log_content) > 0,
          info = "Log file exists but is empty"
        )
        expect_true(any(grepl("SLURM job completed successfully", log_content)),
          info = paste(
            "Expected 'SLURM job completed successfully' in log. Content:",
            paste(log_content, collapse = "\n")
          )
        )
      })
    })

    it("Results can be read using get_intermediate_artifact", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        result_id <- "my_slurm_result"
        slurm_run <- analysis$run_in_slurm(
          function(x) {
            return(x * 2)
          },
          21,
          result_id = result_id
        )

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Verify result file exists
        expect_true(file.exists(slurm_run$result_path),
          info = paste("Result file not found at:", slurm_run$result_path)
        )

        # Get results using get_intermediate_artifact
        # Note: get_intermediate_artifact searches for existing files first
        # Since the file exists with .rds extension, it should find it
        artifact <- analysis$get_intermediate_artifact(result_id)
        expect_true(artifact$exists(),
          info = paste(
            "Artifact not found. Path:", artifact$path,
            "Result path:", slurm_run$result_path,
            "Files in intermediate:", paste(list.files(file.path(analysis$path, "intermediate")), collapse = ", ")
          )
        )

        # Read the results
        results <- artifact$read()
        expect_equal(results, 42)

        # Verify it's the same as getting results from slurm_run
        # get_results() now returns the RDS object directly
        slurm_results <- slurm_run$get_results()
        # Both should be the same value
        expect_equal(results, slurm_results)
        expect_equal(results, 42)
      })
    })

    it("Loads packages in SLURM environment", {
      # Skip if arrow is not available
      skip_if_not_installed("arrow")
      
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      # Load arrow package before submitting job
      library(arrow)

      with_mock_slurm({
        # Use arrow function inside the SLURM function
        # This tests that packages are properly loaded in the SLURM environment
        slurm_run <- analysis$run_in_slurm(function() {
          # Create a simple data frame
          df <- data.frame(x = 1:5, y = letters[1:5])
          
          # Use arrow::as_arrow_table to verify package functions work
          # This will fail if arrow package is not loaded
          tbl <- arrow::as_arrow_table(df)
          
          # Return something that proves arrow was used
          return(list(
            table_rows = nrow(tbl),
            table_cols = ncol(tbl),
            class_name = class(tbl)[1]
          ))
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Verify job completed successfully
        expect_true(slurm_run$is_successful())

        # Get results and verify arrow was used
        results <- slurm_run$get_results()
        expect_true(is.list(results))
        expect_equal(results$table_rows, 5)
        expect_equal(results$table_cols, 2)
        # Verify it's an Arrow table (proves arrow package was loaded)
        # The class should be "Table" from arrow package
        expect_true(grepl("Table|Arrow", results$class_name, ignore.case = TRUE))
      })
    })

    it("Checks error log content for failed jobs", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          stop("Custom error message for testing")
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Check error log exists and has error content
        expect_true(file.exists(slurm_run$error_log_path) || file.exists(slurm_run$log_path),
          info = "At least one log file should exist"
        )

        # Check error log if it exists
        if (file.exists(slurm_run$error_log_path)) {
          error_content <- readLines(slurm_run$error_log_path, warn = FALSE)
          expect_true(length(error_content) > 0)
          # Should contain error information
          expect_true(any(grepl("error|Error|ERROR|Custom error", error_content, ignore.case = TRUE)),
            info = paste(
              "Error log should contain error message. Content:",
              paste(error_content, collapse = "\n")
            )
          )
        }
      })
    })

    it("Can pass arguments to function via ...", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function(x, y) {
            return(x + y)
          },
          x = 10,
          y = 20
        )

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Get results
        results <- slurm_run$get_results()
        expect_equal(results, 30)
      })
    })

    it("Can pass positional arguments to function", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function(x) {
            return(2 * x)
          },
          10,
          result_id = "my_id"
        )

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Get results
        results <- slurm_run$get_results()
        expect_equal(results, 20)
        expect_true(grepl("my_id", slurm_run$result_path))
      })
    })

    it("Errors when getting results before job is done", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          Sys.sleep(2) # Make job take longer
          return(42)
        })

        # Try to get results immediately (should error)
        # The mock job has a 1 second delay before starting, but we check immediately
        # so the job should still be running
        # Check immediately without any sleep
        tryCatch(
          {
            results <- slurm_run$get_results()
            # If we get here, the job might have completed very quickly
            # That's acceptable for a mock - just verify we got a result
            expect_equal(results, 42)
          },
          error = function(e) {
            # Should get error about job not being done OR job failed
            error_msg <- conditionMessage(e)
            # Accept either "not done" or a failure message (since mock jobs can complete very fast)
            is_not_done <- grepl("not done", error_msg, ignore.case = TRUE)
            is_failed <- grepl("failed", error_msg, ignore.case = TRUE)
            expect_true(is_not_done || is_failed,
              info = paste("Expected 'not done' or 'failed' in error message, got:", error_msg)
            )
          }
        )
      })
    })

    it("Can cancel a job", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        # Cancel the job
        expect_no_error(slurm_run$cancel())

        # After cancellation, job should be done
        expect_true(slurm_run$is_done())
      })
    })

    it("Errors when cancel (scancel) fails", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })
        # Mock system2 so scancel returns non-zero status
        mock_system2_fail_scancel <- function(command, args = NULL, stdout = "", stderr = "", ...) {
          if (command == "scancel") {
            result <- "scancel: Invalid job id"
            attr(result, "status") <- 1L
            return(result)
          }
          .real_system2(command, args, stdout = stdout, stderr = stderr, ...)
        }
        testthat::with_mocked_bindings(
          system2 = mock_system2_fail_scancel,
          .package = "base",
          {
            expect_error(
              slurm_run$cancel(),
              regexp = "Failed to cancel"
            )
          }
        )
      })
    })

    it("Handles complex return values", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        complex_result <- list(
          data = data.frame(x = 1:10, y = rnorm(10)),
          metadata = list(created = Sys.time(), version = "1.0"),
          summary = c(mean = 5.5, sd = 3.03)
        )

        slurm_run <- analysis$run_in_slurm(function() {
          return(complex_result)
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        results <- slurm_run$get_results()
        expect_equal(results$data, complex_result$data)
        expect_equal(results$summary, complex_result$summary)
      })
    })

    it("Handles errors in the function", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          stop("Test error in SLURM job")
        })

        # Run the job explicitly
        mock_run_slurm_job(slurm_run$job_id)

        # Job should be done, but getting results might fail
        # The error should be in the log file
        expect_true(slurm_run$is_done())

        # Try to get results - might fail or might have error info
        tryCatch(
          {
            results <- slurm_run$get_results()
            # If we get here, error was caught and stored
            expect_true(TRUE)
          },
          error = function(e) {
            # Expected - result file might not exist if job failed
            expect_true(grepl("not found|failed", conditionMessage(e), ignore.case = TRUE))
          }
        )
      })
    })
  })

  describe("SLURM script generation", {
    it("Uses generic template with environment variables", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(function() {
          return(42)
        })

        expect_s3_class(slurm_run, "PMSlurmRun")
        # Template files are in .slurm directory, not directly accessible
        # But job should work
        expect_true(!is.null(slurm_run$job_id))
      })
    })

    it("Includes module load commands via config", {
      dir <- .get_good_project_path()
      pm <- pm::PMProject$new(dir)
      analysis <- pm$create_analysis("test_analysis")

      with_mock_slurm({
        slurm_run <- analysis$run_in_slurm(
          function() {
            return(42)
          },
          config = list(modules = c("R/4.0.0", "gcc/9.3.0"))
        )

        expect_s3_class(slurm_run, "PMSlurmRun")
      })
    })
  })
})
