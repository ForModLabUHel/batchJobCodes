# GET EXPORTED VARS -------------------------------------------------------


job_name <- Sys.getenv("JOB_NAME")
framework_dir <- Sys.getenv("BATCH_FRAMEWORK_DIR")

if (!nzchar(framework_dir)) {
  stop("BATCH_FRAMEWORK_DIR is not set")
}


# SOURCE ------------------------------------------------------------------


source(
  file.path(
    framework_dir,
    "r_scripts",
    "create_batch_job.R"
  )
)


# LOAD CONFIG -------------------------------------------------------------


cfg <- load_job_config(
  file.path(
    framework_dir,
    "config",
    "config.yml"
  )
)


# GET ROW -----------------------------------------------------------------


job <- get_config_entry(
  cfg$jobs,
  job_name,
  "job"
)


# RESOLVE PATHS -----------------------------------------------------------


resolved_job <- resolve_job_paths(job, cfg$root_paths)

print("resolved job")
print(resolved_job)


# LOAD JOB VARS INTO ENV --------------------------------------------------


list2env(resolved_job, envir = .GlobalEnv)


# SOURCE JOB SCRIPT -------------------------------------------------------


source(resolved_job$runner_script)



