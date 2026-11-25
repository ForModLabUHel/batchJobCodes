#!/bin/bash

#SBATCH --job-name={{JOB_NAME}}
#SBATCH --partition={{PARTITION}}
#SBATCH --account={{ACCOUNT}}
#SBATCH --output={{OUTPUT}}
#SBATCH --error={{ERROR}}
#SBATCH --time={{TIME}}
#SBATCH --ntasks={{NTASKS}}
#SBATCH --cpus-per-task={{CPUS_PER_TASK}}
#SBATCH --nodes={{NODES}}
#SBATCH --mem={{MEM}}
#SBATCH --array={{ARRAY_SPEC}}

set -euo pipefail

umask {{OUTPUT_UMASK}}

module load r-env/452

# set the number of threads based on --cpus-per-task
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

export JOB_NAME="{{JOB_NAME}}"
export BATCH_FRAMEWORK_DIR="{{FRAMEWORK_DIR}}"
export SET_ID="{{SET_ID}}"
export CAL_RUN_TABLE_FILE="{{RUN_TABLE_FILE}}"

cd "{{PROJECT_ROOT}}"

srun Rscript --no-save $BATCH_FRAMEWORK_DIR/r_scripts/runner.R
