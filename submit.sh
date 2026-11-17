#!/bin/bash

set -euo pipefail

usage() {
    echo "Usage:"
    echo "  ./submit.sh <job_name>"
    echo
    echo "Example:"
    echo "  ./submit.sh germany"
}

if [ "$#" -ne 1 ]; then
    usage
    exit 1
fi

JOB_NAME="$1"

BASEDIR="$(cd "$(dirname "$0")" && pwd)"

#
# Verify required structure
#

for dir in config r_scripts slurm
do
    if [ ! -d "${BASEDIR}/${dir}" ]; then
        echo "ERROR: Missing directory:"
        echo "  ${BASEDIR}/${dir}"
        exit 1
    fi
done

for file in \
    config/jobs.csv \
    config/resource_tiers.csv \
    config/shared_resources.csv \
    config/root_paths.csv \
    slurm/job_template.sh \
    r_scripts/create_batch_job.R \
    r_scripts/submit_job.R
do
    if [ ! -f "${BASEDIR}/${file}" ]; then
        echo "ERROR: Missing file:"
        echo "  ${BASEDIR}/${file}"
        exit 1
    fi
done

#
# Verify job exists
#

if ! awk -F',' -v job="$JOB_NAME" '
NR > 1 && $1 == job { found=1 }
END { exit !found }
' "${BASEDIR}/config/jobs.csv"
then
    echo "ERROR: Unknown job '${JOB_NAME}'"
    echo
    echo "Available jobs:"
    awk -F',' 'NR>1 {print "  - " $1}' \
        "${BASEDIR}/config/jobs.csv"
    exit 1
fi


PLAN_FILE=$(
    Rscript \
        "${BASEDIR}/r_scripts/submit_job.R" \
        "${JOB_NAME}" \
        "${BASEDIR}"
)

if [ ! -f "${PLAN_FILE}" ]; then
    echo "ERROR: Submission plan was not generated."
    exit 1
fi

declare -A JOB_IDS

tail -n +2 "${PLAN_FILE}" |
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

done
