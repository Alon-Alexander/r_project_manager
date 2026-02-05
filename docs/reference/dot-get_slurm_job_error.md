# Get error message from SLURM job

Extracts error message from SLURM job logs.

## Usage

``` r
.get_slurm_job_error(job_id, log_path, error_log_path)
```

## Arguments

- job_id:

  Character. The SLURM job ID

- log_path:

  Character. Path to the output log file

- error_log_path:

  Character. Path to the error log file

## Value

Character. Error message or empty string if no error found.
