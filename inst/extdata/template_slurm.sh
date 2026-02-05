#!/bin/bash
# Generic SLURM script template
# SLURM directives are passed via sbatch command-line arguments
# Script parameters are passed as positional arguments:
#   $1 = PM_FUN_FILE
#   $2 = PM_ARGS_FILE
#   $3 = PM_RESULT_FILE
#   $4 = PM_WORK_DIR
#   $5 = PM_R_SCRIPT_PATH
#   $6 = PM_MODULES (space-separated list)
#   $7 = PM_IMAGE_FILE (optional, empty if not provided)

# Parse positional arguments
PM_FUN_FILE=$1
PM_ARGS_FILE=$2
PM_RESULT_FILE=$3
PM_WORK_DIR=$4
PM_R_SCRIPT_PATH=$5
PM_MODULES=$6
PM_IMAGE_FILE=$7

# Load modules if specified (parse space-separated list)
if [ -n "${PM_MODULES}" ]; then
  for module in ${PM_MODULES}; do
    module load ${module}
  done
fi

# Set working directory
cd ${PM_WORK_DIR}

# Run generic R script with environment variables
# Pass image file if provided
if [ -n "${PM_IMAGE_FILE}" ]; then
  Rscript ${PM_R_SCRIPT_PATH} --fun-file=${PM_FUN_FILE} --args-file=${PM_ARGS_FILE} --result-file=${PM_RESULT_FILE} --image-file=${PM_IMAGE_FILE}
else
  Rscript ${PM_R_SCRIPT_PATH} --fun-file=${PM_FUN_FILE} --args-file=${PM_ARGS_FILE} --result-file=${PM_RESULT_FILE}
fi
