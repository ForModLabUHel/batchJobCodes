# Create a submission plan from a named YAML job.


# PARSE AND VALIDATE ARGS -------------------------------------------------


args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1 || !nzchar(args[1])) {
  stop("A job name is required")
}

job_name <- args[1]
config_file <- "config/config.yml"


# SET VARIABLES -----------------------------------------------------------


framework_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)


# SOURCE ------------------------------------------------------------------


source(file.path(
  framework_dir,
  "r_scripts",
  "create_batch_job.R"
))
source(file.path(
  framework_dir,
  "r_scripts",
  "create_submission_plan.R"
))
source(file.path(
  framework_dir,
  "r_scripts",
  "validate_cal_files.R"
))
source(file.path(
  framework_dir,
  "r_scripts",
  "handle_cal_files.R"
))


# LOAD AND VALIDATE CONFIG ------------------------------------------------


cfg <- load_job_config(config_file)

if (!job_name %in% names(cfg$jobs)) {
  stop(
    "Unknown job '", job_name, "'. Available jobs: ",
    paste(sort(names(cfg$jobs)), collapse = ", ")
  )
}


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
