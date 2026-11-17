create_submission_plan <- function(
    job_name,
    generated_dir,
    config_file,
    framework_dir
) {
  cfg <- load_job_config(config_file)
  
  job <- get_row(
    cfg$jobs,
    "job_name",
    job_name
  )
  
  n_sets <- set_job_nParam(job$n_sets)
  n_chains <- set_job_nParam(job$n_chains)
  
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

create_chain_plan <- function(job_name,
                              n_sets,
                              n_chains,
                              generated_dir = generated_dir,
                              config_file = config_file,
                              framework_dir) {
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
        paste0(
          job_name,
          "_set",
          set_id - 1
        ),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(
    rbind,
    rows
  )
}