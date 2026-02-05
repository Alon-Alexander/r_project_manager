#!/usr/bin/env Rscript
# Generic R script for SLURM jobs
# Reads function from file and runs it, saving results

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Extract parameters from environment variables or command line
fun_file <- Sys.getenv("PM_FUN_FILE", "")
args_file <- Sys.getenv("PM_ARGS_FILE", "")
result_file <- Sys.getenv("PM_RESULT_FILE", "")
packages_file <- Sys.getenv("PM_PACKAGES_FILE", "")
image_file <- Sys.getenv("PM_IMAGE_FILE", "")

# Try to get from command line if not in environment
if (fun_file == "" || args_file == "" || result_file == "") {
  for (arg in args) {
    if (startsWith(arg, "--fun-file=")) {
      fun_file <- sub("--fun-file=", "", arg)
    } else if (startsWith(arg, "--args-file=")) {
      args_file <- sub("--args-file=", "", arg)
    } else if (startsWith(arg, "--result-file=")) {
      result_file <- sub("--result-file=", "", arg)
    } else if (startsWith(arg, "--packages-file=")) {
      packages_file <- sub("--packages-file=", "", arg)
    } else if (startsWith(arg, "--image-file=")) {
      image_file <- sub("--image-file=", "", arg)
    }
  }
}

# Load packages if provided
if (packages_file != "" && file.exists(packages_file)) {
  packages <- readRDS(packages_file)
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      warning(sprintf("Failed to load package: %s", pkg))
    }
  }
}

# Load workspace image if provided
if (image_file != "" && file.exists(image_file)) {
  load(image_file, envir = .GlobalEnv)
}

# Validate inputs
if (fun_file == "" || !file.exists(fun_file)) {
  stop("Function file not found or not specified: ", fun_file)
}

if (args_file == "" || !file.exists(args_file)) {
  stop("Arguments file not found or not specified: ", args_file)
}

if (result_file == "") {
  stop("Result file not specified")
}

# Load the function and arguments
fun <- readRDS(fun_file)
fun_args <- readRDS(args_file)

# Run the function with arguments
result <- tryCatch({
  do.call(fun, fun_args)
}, error = function(e) {
  stop(sprintf("Error in SLURM job: %s", conditionMessage(e)))
})

# Save results as RDS file
saveRDS(result, file = result_file)

cat("SLURM job completed successfully\n")
