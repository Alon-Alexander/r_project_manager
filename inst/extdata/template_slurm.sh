#!/bin/bash
# Generic SLURM script template
# SLURM directives are passed via sbatch command-line arguments

# Export environment variables
export PM_FUN_FILE=${PM_FUN_FILE}
export PM_ARGS_FILE=${PM_ARGS_FILE}
export PM_RESULT_FILE=${PM_RESULT_FILE}
export PM_WORK_DIR=${PM_WORK_DIR}

# Load modules if specified
${PM_MODULE_LOAD_COMMANDS}

# Set working directory
cd ${PM_WORK_DIR}

# Run generic R script with environment variables
Rscript ${PM_R_SCRIPT_PATH} --fun-file=${PM_FUN_FILE} --args-file=${PM_ARGS_FILE} --result-file=${PM_RESULT_FILE}
