create_submission_plan <- function(
    job_name,
    basedir
) {
  cfg <- load_job_config(basedir)
  
  job <- get_row(
    cfg$jobs,
    "job_name",
    job_name
  )
  
  n_sets <- job$n_sets
  n_chains <- job$n_chains
  
  if(
    is.na(n_sets) ||
    n_sets <= 1
  ) {
    create_simple_plan(
      job_name,
      basedir
    )
  } else {
    create_chain_plan(
      job_name,
      n_sets,
      n_chains,
      basedir
    )
  }
}

create_simple_plan <- function(
    job_name,
    basedir
) {
  job_file <- create_batch_job(
    job_name = job_name,
    basedir = basedir,
    extra_values = list(array_spec = "1-1")
  )
  
  data.frame(
    job_key = job_name,
    job_file = normalizePath(job_file),
    depends_on = "",
    stringsAsFactors = FALSE
  )
}

create_chain_plan <- function(
    job_name,
    n_sets,
    n_chains,
    basedir
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
      basedir = basedir,
      extra_values = list(
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