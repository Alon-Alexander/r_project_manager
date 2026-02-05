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
cat("========== SLURM Job Parameters ==========\n")
cat(sprintf("Function file:    %s\n", fun_file))
cat(sprintf("Arguments file:   %s\n", args_file))
cat(sprintf("Result file:      %s\n", result_file))
cat(sprintf("Packages file:    %s\n", if (packages_file != "") packages_file else "(none)"))
cat(sprintf("Image file:       %s\n", if (image_file != "") image_file else "(none)"))
cat("==========================================\n")

# Load packages if provided
if (packages_file != "" && file.exists(packages_file)) {
  packages <- readRDS(packages_file)
  cat("Packages to be loaded:", paste(packages, collapse = ", "), "\n")
  for (pkg in packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      warning(sprintf("Failed to load package: %s - %s", pkg, conditionMessage(e)))
    })
  }
} else {
  warning(sprintf("Couldn't load packages from file: %s (file does not exist or not specified)", packages_file))
}

# Load workspace image if provided
if (image_file != "" && file.exists(image_file)) {
  load(image_file, envir = .GlobalEnv)
  cat("Workspace image loaded from:", image_file, "\n")
} else {
  warning(sprintf("Couldn't load workspace image from file: %s (file does not exist or not specified)", image_file))
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
cat("Loading function\n")
fun <- readRDS(fun_file)

cat("Loading arguments\n")
fun_args <- readRDS(args_file)

# Run the function with arguments
cat("Running the function...\n\n\n")
result <- tryCatch({
  do.call(fun, fun_args)
}, error = function(e) {
  stop(sprintf("Error in SLURM job: %s", conditionMessage(e)))
})

# Save results as RDS file
cat("\n\n\nDone!\nSaving results to file\n\n\n")
saveRDS(result, file = result_file)

cat("SLURM job completed successfully\n")
