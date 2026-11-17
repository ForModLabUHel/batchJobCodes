# Create submission plan file from jobs.csv row.


# PARSE ARGS --------------------------------------------------------------


args <- commandArgs(trailingOnly = TRUE)

job_name <- args[1]

basedir <- if (
  length(args) >= 2
) {
  args[2]
} else {
  "batch_jobs"
}


# SOURCE ------------------------------------------------------------------


source(
  file.path(
    basedir,
    "r_scripts",
    "create_batch_job.R"
  )
)

source(
  file.path(
    basedir,
    "r_scripts",
    "create_submission_plan.R"
  )
)


# CREATE PLAN DIR ---------------------------------------------------------


generated_dir <- file.path(
  basedir,
  "config",
  "generated"
)

dir.create(
  generated_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# CREATE PLAN FILE --------------------------------------------------------


plan_file <- file.path(
  generated_dir,
  paste0(job_name, "_submission_plan.csv")
)

plan <- create_submission_plan(
  job_name = job_name,
  basedir = basedir
)

write.table(
  plan,
  plan_file,
  sep = "|",
  row.names = FALSE,
  quote = FALSE
)

cat(plan_file)
