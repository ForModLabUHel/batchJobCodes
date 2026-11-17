job_name <- Sys.getenv("JOB_NAME")
basedir  <- Sys.getenv("BATCH_JOBS_DIR")

source(
  file.path(
    basedir,
    "r_scripts",
    "create_batch_job.R"
  )
)

cfg <- load_job_config(basedir)
job <- get_row(
  cfg$jobs,
  "job_name",
  job_name
)

print("basedir")
print(basedir)

root_paths <- file.path(basedir, "config", "root_paths.csv")
roots <- read.csv(root_paths)

resolved_job <- resolve_job_paths(job, roots)

print("resolved job")
print(resolved_job)

list2env(resolved_job, envir = .GlobalEnv)




