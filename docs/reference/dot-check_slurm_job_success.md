# Check if a SLURM job completed successfully

Checks if a SLURM job completed successfully (not just done, but
succeeded).

## Usage

``` r
.check_slurm_job_success(job_id, log_path, error_log_path, result_path)
```

## Arguments

- job_id:

  Character. The SLURM job ID

- log_path:

  Character. Path to the output log file

- error_log_path:

  Character. Path to the error log file

- result_path:

  Character. Path to the result file

## Value

Logical. TRUE if the job completed successfully, FALSE otherwise.
