# Running Analyses with SLURM

The `pm` package provides seamless integration with SLURM (Simple Linux
Utility for Resource Management) for running computationally intensive
analyses on HPC clusters. This vignette demonstrates how to use the
`run_in_slurm()` method to submit R functions as SLURM batch jobs.

## Overview

The SLURM integration in `pm` allows you to:

- Submit R functions as non-blocking SLURM batch jobs
- Check job status and completion
- Retrieve results automatically when jobs complete
- Configure SLURM resources (time, memory, CPUs, modules, etc.)
- Handle job failures and timeouts gracefully

## Basic Usage

### Simple Function Execution

The simplest use case is running a function that returns a result:

``` r

library(pm)

# Create or load a project
pm <- pm_project("path/to/your/project")
analysis <- pm$create_analysis("my_analysis")

# Define a function to run in SLURM
compute_result <- function() {
  # Your computation here
  result <- sum(1:1000)
  return(result)
}

# Submit the job (non-blocking)
slurm_run <- analysis$run_in_slurm(compute_result)

# Check if job is done
slurm_run$is_done()

# Get results when ready
results <- slurm_run$get_results()
```

### Passing Arguments to Functions

You can pass arguments to your function using `...`:

``` r

# Function that takes arguments
process_data <- function(data, multiplier) {
  return(data * multiplier)
}

# Submit with arguments
slurm_run <- analysis$run_in_slurm(
  process_data,
  data = 42,
  multiplier = 2
)

# Get results
results <- slurm_run$get_results()
```

### Using Positional Arguments

Positional arguments are also supported:

``` r

add_numbers <- function(x, y) {
  return(x + y)
}

slurm_run <- analysis$run_in_slurm(add_numbers, 10, 20)
results <- slurm_run$get_results(timeout = 100)  # Returns 30
```

## SLURM Configuration

### Basic Resource Configuration

Configure SLURM resources using the `config` parameter:

``` r

slurm_run <- analysis$run_in_slurm(
  my_function,
  config = list(
    job_name = "my_analysis_job",
    time_limit = "02:00:00",      # 2 hours
    memory = "8G",                 # 8 GB RAM
    cpus = 4                       # 4 CPUs
  )
)
```

### Advanced Configuration

You can also specify SLURM modules and extra flags:

``` r

slurm_run <- analysis$run_in_slurm(
  my_function,
  config = list(
    job_name = "complex_analysis",
    time_limit = "24:00:00",
    memory = "32G",
    cpus = 16,
    modules = c("R/4.3.0", "gcc/9.3.0", "openmpi/4.1.0"),
    slurm_flags = "--partition=gpu --gres=gpu:1"
  )
)
```

### Available Configuration Options

- `job_name`: Name for the SLURM job (default: based on result_id)
- `time_limit`: Time limit in format “HH:MM:SS” (default: “01:00:00”)
- `memory`: Memory requirement, e.g., “8G”, “16G” (default: “32G”)
- `cpus`: Number of CPUs (default: 1)
- `modules`: Character vector of modules to load (default: NULL)
- `slurm_flags`: Additional SLURM flags as a string (default: ““)

## Job Management

### Checking Job Status

The `PMSlurmRun` object provides methods to check job status:

``` r

# Check if job is done
if (slurm_run$is_done()) {
  cat("Job completed!\n")
} else {
  cat("Job still running...\n")
}

# Check if job completed successfully
if (slurm_run$is_successful()) {
  cat("Job succeeded!\n")
} else {
  cat("Job failed or timed out\n")
}
```

### Blocking Result Retrieval with Timeout

You can wait for results with a timeout:

``` r

# Wait up to 5 minutes for results
results <- slurm_run$get_results(timeout = 300)

# If timeout is exceeded, an error is raised
tryCatch({
  results <- slurm_run$get_results(timeout = 60)
}, error = function(e) {
  cat("Timeout exceeded:", conditionMessage(e), "\n")
})
```

### Non-blocking Result Retrieval

For non-blocking retrieval, check status first:

``` r

# Check if done before getting results
if (slurm_run$is_done()) {
  if (slurm_run$is_successful()) {
    results <- slurm_run$get_results()
  } else {
    cat("Job failed. Check logs at:", slurm_run$log_path, "\n")
  }
} else {
  cat("Job still running. Job ID:", slurm_run$job_id, "\n")
}
```

### Canceling Jobs

You can cancel a running job:

``` r

# Cancel the job
slurm_run$cancel()

# Verify it's canceled
slurm_run$is_done()  # Should return TRUE
slurm_run$is_successful()  # Should return FALSE
```

## Result Management

### Custom Result IDs

Specify a custom result ID for better organization:

``` r

slurm_run <- analysis$run_in_slurm(
  my_function,
  result_id = "my_custom_result"
)

# Results are saved to intermediate/my_custom_result.rds
results <- slurm_run$get_results()
```

### Accessing Results via get_intermediate_artifact

Results can also be accessed using the standard artifact methods:

``` r

# After job completes
artifact <- analysis$get_intermediate_artifact("my_custom_result")
if (artifact$exists()) {
  results <- artifact$read()
}
```

## Error Handling

### Handling Job Failures

When a job fails, `get_results()` will raise an error with details:

``` r

tryCatch({
  results <- slurm_run$get_results()
}, error = function(e) {
  cat("Job failed:", conditionMessage(e), "\n")
  # Error message includes job ID and log file location
})
```

### Checking Log Files

Log files are automatically created in the analysis’s `logs/` directory:

``` r

# Log file paths
cat("Output log:", slurm_run$log_path, "\n")
cat("Error log:", slurm_run$error_log_path, "\n")

# Read log files if needed
if (file.exists(slurm_run$log_path)) {
  log_content <- readLines(slurm_run$log_path)
  cat("Output log content:\n")
  cat(paste(log_content, collapse = "\n"), "\n")
}
```

## Complete Example

Here’s a complete example combining all features:

``` r

library(pm)

# Setup
pm <- pm_project("path/to/project")
analysis <- pm$create_analysis("slurm_analysis")

# Define a computationally intensive function
heavy_computation <- function(n, seed = 42) {
  set.seed(seed)
  # Simulate heavy computation
  result <- list(
    data = rnorm(n),
    summary = list(
      mean = mean(rnorm(n)),
      sd = sd(rnorm(n)),
      n = n
    ),
    timestamp = Sys.time()
  )
  return(result)
}

# Submit with configuration
slurm_run <- analysis$run_in_slurm(
  heavy_computation,
  n = 1000000,
  seed = 123,
  result_id = "large_simulation",
  config = list(
    job_name = "large_sim",
    time_limit = "04:00:00",
    memory = "16G",
    cpus = 8,
    modules = c("R/4.3.0")
  )
)

# Monitor job
cat("Job submitted with ID:", slurm_run$job_id, "\n")
cat("Log file:", slurm_run$log_path, "\n")

# Wait for completion (with timeout)
results <- slurm_run$get_results(timeout = 3600)  # 1 hour timeout

# Use results
cat("Computation completed!\n")
cat("Mean:", results$summary$mean, "\n")
cat("SD:", results$summary$sd, "\n")
```

## Best Practices

1.  **Always check job status** before retrieving results to avoid
    errors
2.  **Use appropriate timeouts** based on expected job duration
3.  **Specify sufficient resources** in config to avoid job failures
4.  **Use custom result IDs** for better organization of intermediate
    results
5.  **Check log files** when jobs fail to diagnose issues
6.  **Cancel long-running jobs** if they’re no longer needed

## Controlling workspace images with `store_image`

When you call `run_in_slurm()`, the analysis can save an R workspace
image that is loaded on the SLURM worker before your function runs. By
default, the entire calling environment is saved, which can be wasteful
if you have large intermediate objects in memory that the job does not
actually need.

You can use the `store_image` config option to save only specific
objects. This keeps the job submission lightweight while still making
required objects available on the SLURM node:

``` r

library(pm)

pm <- pm_project("path/to/project")
analysis <- pm$create_analysis("slurm_analysis")

# Suppose your current R session has some large objects
big_raw_data <- readRDS("big-raw-data.rds")      # hundreds of MB
precomputed_lookup <- readRDS("lookup-small.rds")  # small, needed on the worker

heavy_computation <- function(n) {
  # 'precomputed_lookup' will be available on the SLURM worker
  sample(precomputed_lookup, n, replace = TRUE)
}

slurm_run <- analysis$run_in_slurm(
  heavy_computation,
  n = 100000,
  config = list(
    time_limit = "02:00:00",
    memory = "16G",
    # Only save and load selected objects instead of the full workspace
    store_image = c("precomputed_lookup")
  )
)
```

In this example, `big_raw_data` stays in your interactive session but is
not serialized and sent to the SLURM job, while `precomputed_lookup` is
saved and restored for use in `heavy_computation()`. This pattern is
especially useful when your workspace contains large simulation results
or raw data that are not needed by the job itself.

## Technical Details

### How It Works

1.  **Job Submission**: The function and its arguments are serialized to
    RDS files in a temporary directory
2.  **SLURM Script**: A generic SLURM batch script is created with
    environment variables for configuration
3.  **R Script Execution**: SLURM runs a generic R script that loads the
    function and arguments, executes them, and saves results
4.  **Result Storage**: Results are saved as RDS files in the analysis’s
    `intermediate/` directory
5.  **Status Checking**: Job status is checked using `squeue` and log
    file analysis

### File Structure

When you submit a SLURM job, the following files are created:

- **Temporary directory** (invisible to user): Contains function,
  arguments, and SLURM scripts
- **Log files**: `logs/slurm-{job_id}.out` and `logs/slurm-{job_id}.err`
- **Result file**: `intermediate/{result_id}.rds`

### SLURM Availability

The package checks for SLURM availability before submission:

``` r

# Check if SLURM is available
if (pm::is_slurm_available()) {
  cat("SLURM is available\n")
} else {
  cat("SLURM is not available on this system\n")
}
```

If SLURM is not available, `run_in_slurm()` will raise an error.

## Troubleshooting

### Job Never Completes

If a job appears to hang:

1.  Check the job status with `squeue -j {job_id}`
2.  Check log files for errors
3.  Verify SLURM configuration (time, memory, etc.)
4.  Consider canceling and resubmitting with different resources

### Results Not Found

If `get_results()` fails with “Result file not found”:

1.  Check if job completed successfully: `slurm_run$is_successful()`
2.  Verify the result file exists: `file.exists(slurm_run$result_path)`
3.  Check error logs for execution errors

### Timeout Issues

If jobs consistently timeout:

1.  Increase `time_limit` in config
2.  Optimize your function for performance
3.  Check if resources (memory, CPUs) are sufficient

## See Also

- [`?PMAnalysis`](https://alon-alexander.github.io/r_project_manager/reference/PMAnalysis.md)
  for more information on the analysis object
- [`?PMSlurmRun`](https://alon-alexander.github.io/r_project_manager/reference/PMSlurmRun.md)
  for details on job management methods
- [`?is_slurm_available`](https://alon-alexander.github.io/r_project_manager/reference/is_slurm_available.md)
  for checking SLURM availability
