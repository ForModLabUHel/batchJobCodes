# GET EXPORTED VARS -------------------------------------------------------


job_name <- Sys.getenv("JOB_NAME")
framework_dir <- Sys.getenv("BATCH_FRAMEWORK_DIR")
setID <- Sys.getenv("SET_ID")
calRunTableFile <- Sys.getenv("CAL_RUN_TABLE_FILE")

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

if (nzchar(calRunTableFile) && file.exists(calRunTableFile)) {
  source(
    file.path(
      framework_dir,
      "r_scripts",
      "handle_cal_files.R"
    )
  )
  calRunRow <- read_cal_run_table_row(
    calRunTableFile,
    as.integer(setID),
    as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  )
  calRunTableRow <- calRunRow
}


# RESOLVE PATHS -----------------------------------------------------------


resolved_job <- resolve_job_paths(job, cfg$root_paths)
group_access_path <- resolve_group_access_path(
  job,
  cfg$shared_resources,
  cfg$root_paths
)

print("resolved job")
print(resolved_job)


# LOAD JOB VARS INTO ENV --------------------------------------------------


job_env <- list2env(
  resolved_job,
  envir = new.env(parent = globalenv())
)


# SOURCE JOB SCRIPT -------------------------------------------------------


source(resolved_job$runner_script, local = job_env)

print("group access")
print(group_access_path)

apply_group_access_permissions(group_access_path)



