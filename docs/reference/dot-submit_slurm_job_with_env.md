# Submit a SLURM job with environment variables

Submits a SLURM job using sbatch with environment variables and returns
the job ID.

## Usage

``` r
.submit_slurm_job_with_env(slurm_script_path, env_vars)
```

## Arguments

- slurm_script_path:

  Character. Path to the SLURM script file

- env_vars:

  Named character vector. Environment variables to set

## Value

Character. The SLURM job ID
