# SLURM Run class to manage SLURM job execution

This object represents a SLURM job that has been submitted. It provides
methods to check job status, get results, and cancel the job.

## Public fields

- `job_id`:

  Character. The SLURM job ID

- `analysis_path`:

  Character. Path to the analysis folder

- `result_path`:

  Character. Path where results will be stored

- `log_path`:

  Character. Path to the SLURM output log file

- `error_log_path`:

  Character. Path to the SLURM error log file

## Methods

### Public methods

- [`PMSlurmRun$new()`](#method-PMSlurmRun-new)

- [`PMSlurmRun$print()`](#method-PMSlurmRun-print)

- [`PMSlurmRun$is_done()`](#method-PMSlurmRun-is_done)

- [`PMSlurmRun$is_successful()`](#method-PMSlurmRun-is_successful)

- [`PMSlurmRun$get_results()`](#method-PMSlurmRun-get_results)

- [`PMSlurmRun$cancel()`](#method-PMSlurmRun-cancel)

- [`PMSlurmRun$clone()`](#method-PMSlurmRun-clone)

------------------------------------------------------------------------

### Method `new()`

Create a PMSlurmRun object

#### Usage

    PMSlurmRun$new(job_id, analysis_path, result_path, log_path, error_log_path)

#### Arguments

- `job_id`:

  Character. The SLURM job ID

- `analysis_path`:

  Character. Path to the analysis folder

- `result_path`:

  Character. Path where results will be stored

- `log_path`:

  Character. Path to the SLURM output log file

- `error_log_path`:

  Character. Path to the SLURM error log file

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for PMSlurmRun

#### Usage

    PMSlurmRun$print()

------------------------------------------------------------------------

### Method `is_done()`

Check if the SLURM job is done

#### Usage

    PMSlurmRun$is_done()

#### Returns

Logical. TRUE if the job is done, FALSE otherwise.

------------------------------------------------------------------------

### Method `is_successful()`

Check if the SLURM job completed successfully

#### Usage

    PMSlurmRun$is_successful()

#### Details

Returns FALSE if the job is still running or if it failed. Only returns
TRUE if the job completed successfully.

#### Returns

Logical. TRUE if the job completed successfully, FALSE if it failed or
is still running.

------------------------------------------------------------------------

### Method `get_results()`

Get the results from the SLURM job

#### Usage

    PMSlurmRun$get_results(timeout = NULL)

#### Arguments

- `timeout`:

  Numeric. Optional timeout in seconds. If provided, will wait for the
  job to complete (blocking) up to the timeout duration before failing.
  If NULL (default), returns immediately if job is not done.

#### Details

If timeout is provided, this method will block and wait for the job to
complete. If the job fails or times out, an error is raised with
details. The results are loaded from the intermediate folder.

#### Returns

The results object (loaded from the result file)

------------------------------------------------------------------------

### Method `cancel()`

Cancel the SLURM job

#### Usage

    PMSlurmRun$cancel()

#### Returns

Invisibly returns TRUE if successful

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PMSlurmRun$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run a function in SLURM
analysis <- pm$get_analysis("my_analysis")
slurm_run <- analysis$run_in_slurm(function() {
  # Your analysis code here
  result <- compute_something()
  return(result)
})

# Check if job is done
slurm_run$is_done()

# Get results (errors if not done)
results <- slurm_run$get_results()

# Cancel the job
slurm_run$cancel()
} # }
```
