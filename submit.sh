#!/bin/bash

set -euo pipefail

usage() {
    echo "Usage:"
    echo "  ./submit.sh <job_name>"
    echo
    echo "Example:"
    echo "  ./submit.sh germany"
    echo
    echo "Output directory:"
    echo "  BATCH_JOB_CODES_DIR (optional; defaults to ~/batchJobCodesFiles)"
}

if [ "$#" -ne 1 ]; then
    usage
    exit 1
fi

JOB_NAME="$1"

FRAMEWORK_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$FRAMEWORK_DIR"

#
# Verify required structure
#

for dir in config r_scripts slurm
do
    if [ ! -d "${FRAMEWORK_DIR}/${dir}" ]; then
        echo "ERROR: Missing directory:"
        echo "  ${FRAMEWORK_DIR}/${dir}"
        exit 1
    fi
done

for file in \
    config/config.yml \
    slurm/job_template.sh \
    r_scripts/create_batch_job.R \
    r_scripts/create_submission_plan.R \
    r_scripts/submit_job.R
do
    if [ ! -f "${FRAMEWORK_DIR}/${file}" ]; then
        echo "ERROR: Missing file:"
        echo "  ${FRAMEWORK_DIR}/${file}"
        exit 1
    fi
done

PLAN_FILE=$(
    Rscript r_scripts/submit_job.R "$JOB_NAME"
)

if [ ! -f "${PLAN_FILE}" ]; then
    echo "ERROR: Submission plan was not generated."
    exit 1
fi

declare -A JOB_IDS

while IFS='|' read -r job_key job_file depends_on
do

    job_key=$(echo "$job_key" | tr -d '"')
    job_file=$(echo "$job_file" | tr -d '"')
    depends_on=$(echo "$depends_on" | tr -d '"')

    if [ -z "$depends_on" ]; then

        JOB_ID=$(
            sbatch "$job_file" |
            awk '{print $4}'
        )
        
    else

        PREV_ID=${JOB_IDS[$depends_on]}

        JOB_ID=$(
            sbatch \
                --dependency=afterok:${PREV_ID} \
                "$job_file" |
            awk '{print $4}'
        )

    fi

    JOB_IDS[$job_key]=$JOB_ID

    echo "Submitted ${job_key} (${JOB_ID})"

done < <(tail -n +2 "${PLAN_FILE}")
