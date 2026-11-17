# Create submission plan file from jobs.csv row.


# PARSE ARGS --------------------------------------------------------------


args <- commandArgs(trailingOnly = TRUE)

job_name <- args[1]


# SOURCE ------------------------------------------------------------------


source("r_scripts/create_batch_job.R")
source("r_scripts/create_submission_plan.R")



# SET VARIABLES -----------------------------------------------------------


config_file <- "config/config.yml"
framework_dir <- normalizePath(getwd())


# CREATE GENERATED DIR ----------------------------------------------------
# Directory for programmatically generated files.


generated_dir <- file.path(
  "generated",
  job_name
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
  generated_dir = generated_dir,
  config_file = config_file,
  framework_dir = framework_dir
)

write.table(
  plan,
  plan_file,
  sep = "|",
  row.names = FALSE,
  quote = FALSE
)

cat(plan_file)
