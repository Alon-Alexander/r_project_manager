# Check if SLURM is available on the system

Checks if SLURM commands (sbatch, squeue, scancel) are available.

## Usage

``` r
is_slurm_available()
```

## Value

Logical. TRUE if SLURM is available, FALSE otherwise.

## Details

This function checks for the presence of sbatch, squeue, and scancel
commands.
