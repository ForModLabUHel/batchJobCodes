create_submission_plan <- function(
    job_name,
    generated_dir,
    config_file,
    framework_dir
) {
  cfg <- load_job_config(config_file)
  
  job <- get_config_entry(
    cfg$jobs,
    job_name,
    "job"
  )
  
  n_sets <- set_job_nParam(job$n_sets)
  n_chains <- set_job_nParam(job$n_chains)
  input_directory <- job$input_directory

  if (!is.null(input_directory) && nzchar(input_directory)) {
    return(create_file_based_submission_plan(
      job_name = job_name,
      input_directory = input_directory,
      generated_dir = generated_dir,
      config_file = config_file,
      framework_dir = framework_dir,
      reporting_job_name = job$reporting_job
    ))
  }
  
  create_chain_plan(
    job_name,
    n_sets,
    n_chains,
    generated_dir = generated_dir,
    config_file = config_file,
    framework_dir
  )
}

set_job_nParam <- function(n_param) {
  if(is.null(n_param) ||
     is.na(n_param)   ||
     n_param <= 1) {
     n_param <- 1
  }
  n_param
}

create_file_based_submission_plan <- function(
    job_name,
    input_directory,
    generated_dir,
    config_file,
    framework_dir,
    reporting_job_name = NULL,
    create_job = create_batch_job
) {
  handler_file <- file.path(
    framework_dir,
    "r_scripts",
    "handle_cal_files.R"
  )
  validator_file <- file.path(
    framework_dir,
    "r_scripts",
    "validate_cal_files.R"
  )

  if (!exists("validate_cal_files", mode = "function")) {
    source(validator_file)
  }
  if (!exists("create_cal_run_table", mode = "function")) {
    source(handler_file)
  }

  cfg <- load_job_config(config_file)
  job <- get_config_entry(cfg$jobs, job_name, "job")
  n_sets <- set_job_nParam(job$n_sets)
  n_chains <- set_job_nParam(job$n_chains)

  validate_cal_files(
    input_directory,
    n_sets,
    n_chains,
    throw = TRUE
  )

  run_table <- create_cal_run_table(
    input_directory,
    n_sets,
    n_chains
  )

  if (nrow(run_table) == 0) {
    return(data.frame(
      job_key = character(),
      job_file = character(),
      depends_on = character(),
      stringsAsFactors = FALSE
    ))
  }

  array_specs <- create_cal_array_specs(run_table)
  rows <- list()
  previous_job_key <- ""

  for (set_row in split(array_specs, array_specs$set_id)) {
    if (!set_row$needs_run) {
      next
    }

    original_set_rows <- run_table[
      run_table$set_id == set_row$set_id,
    ]
    original_set_id <- unique(original_set_rows$original_set_id)
    if (length(original_set_id) != 1) {
      stop(
        "Each run-table set must map to one original calibration set",
        call. = FALSE
      )
    }

    job_key <- paste0(job_name, "_", set_row$set_id)
    job_file <- create_job(
      job_name = job_name,
      generated_dir = generated_dir,
      config_file = config_file,
      extra_values = list(
        framework_dir = framework_dir,
        set_id = original_set_id,
        array_spec = set_row$array_spec
      ),
      output_name = job_key
    )

    rows[[length(rows) + 1]] <- data.frame(
      job_key = job_key,
      job_file = normalizePath(job_file),
      depends_on = previous_job_key,
      stringsAsFactors = FALSE
    )
    previous_job_key <- job_key

    if (!is.null(reporting_job_name) &&
        nzchar(reporting_job_name) &&
        set_row$set_id < max(array_specs$set_id)) {
      report_key <- paste0(job_name, "_report_", set_row$set_id)
      report_file <- create_job(
        job_name = reporting_job_name,
        generated_dir = generated_dir,
        config_file = config_file,
        extra_values = list(
          framework_dir = framework_dir,
          set_id = original_set_id,
          array_spec = "1"
        ),
        output_name = report_key
      )

      rows[[length(rows) + 1]] <- data.frame(
        job_key = report_key,
        job_file = normalizePath(report_file),
        depends_on = previous_job_key,
        stringsAsFactors = FALSE
      )
      previous_job_key <- report_key
    }
  }

  do.call(rbind, rows)
}

create_chain_plan <- function(
    job_name,
    n_sets,
    n_chains,
    generated_dir,
    config_file,
    framework_dir
) {
  rows <- vector(
    "list",
    n_sets
  )
  
  for(set_id in seq_len(n_sets))
  {
    job_key <- paste0(
      job_name,
      "_",
      set_id
    )
    
    job_file <- create_batch_job(
      job_name = job_name,
      generated_dir = generated_dir,
      config_file = config_file,
      extra_values = list(
        framework_dir = framework_dir,
        set_id = set_id,
        array_spec = paste0(
          "1-",
          n_chains
        )
      ),
      output_name = job_key
    )
    
    rows[[set_id]] <- data.frame(
      job_key = job_key,
      job_file = normalizePath(job_file),
      depends_on =
        if(set_id == 1)
          ""
      else
        paste0(job_name, "_", set_id - 1),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(
    rbind,
    rows
  )
}