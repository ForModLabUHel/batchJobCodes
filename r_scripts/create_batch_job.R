# CREATE BATCH JOB WORKER -------------------------------------------------


# Resolve the full path for a file using a path_spec eg. PROJECT:Rsrc/runModel.R
# and a lookup file (roots). The lookup file should be a table with the base path
# set for each root ie. column root_name = "PROJECT" and column path = <path to project>
resolve_root_path <- function(path_spec, roots) {
  
  parts <- strsplit(
    path_spec,
    ":",
    fixed = TRUE
  )[[1]]
  
  checkmate::assert_character(
    parts,
    len = 2
  )
  
  root_name <- parts[1]
  rel_path <- parts[2]
  
  root <- roots$path[
    match(
      root_name,
      roots$root_name
    )
  ]
  
  checkmate::assert_string(
    root,
    na.ok = FALSE
  )
  
  file.path(root, rel_path)
}

resolve_job_paths <- function(job, roots) {
  
  path_cols <- c(
    "entry_point",
    "runner_script",
    "database"
  )
  
  for (col in intersect(path_cols, names(job))) {
    
    if (!is.na(job[[col]]) &&
        grepl("^[^:]+:", job[[col]])) {
      
      job[[col]] <- resolve_root_path(
        job[[col]],
        roots
      )
      
    }
    
  }
  
  job
}

# Yaml config
load_job_config <- function(config_file) {
  
  raw <- yaml::read_yaml(config_file)
  
  list(
    jobs = as.data.frame(
      raw$jobs,
      stringsAsFactors = FALSE
    ),
    
    tiers = as.data.frame(
      raw$resource_tiers,
      stringsAsFactors = FALSE
    ),
    
    shared = data.frame(
      key = names(raw$shared_resources),
      value = unlist(raw$shared_resources),
      stringsAsFactors = FALSE
    ),
    
    root_paths = data.frame(
      root_name = names(raw$root_paths),
      path = unlist(raw$root_paths),
      stringsAsFactors = FALSE
    )
  )
}

# Get the correct params row from csv 
get_row <- function(df, column, value) {
  
  res <- df[df[[column]] == value, ]
  
  if (nrow(res) != 1) {
    stop(
      "Expected exactly one match for ",
      column,
      " = ",
      value
    )
  }
  
  res
}

# Transforms each parameter into a bash readable placeholder
create_placeholders <- function(values) {
  
  values <- lapply(
    values,
    function(x) {
      
      if (is.null(x))
        stop("Replacement value is NULL")
      
      if (length(x) != 1)
        stop("Replacement value must have length 1")
      
      as.character(x)
    }
  )
  
  names(values) <- paste0(
    "{{",
    toupper(names(values)),
    "}}"
  )
  
  values
}

# Replaces the template file placeholders with the parameter values
render_template <- function(template, replacements) {
  
  for (nm in names(replacements)) {
    
    template <- gsub(
      nm,
      replacements[[nm]],
      template,
      fixed = TRUE
    )
    
  }
  
  template
}

# Builds the list of placeholders filled in with the new params
build_replacements <- function(
    job,
    tier,
    shared_vals,
    extra_values = list()
) {
  
  tier_vals <- as.list(tier)
  tier_vals["tier"] <- NULL
  
  values <- c(
    as.list(job),
    tier_vals,
    as.list(shared_vals),
    extra_values
  )

  create_placeholders(values)
}


# CREATE BATCH JOB CONTROLLER ---------------------------------------------


# Creates a batch job script using a template with placeholders. Replaces
# the placeholders with parameters from config files. The identifier for the 
# parameters is given in job_name. The directory that contains all required
# files in a structured way is given in basedir (see below). 
# Note: This is not necessarily the root dir of the project.
# Writes the batch script and sets permissions to "755". 
# Returns the batch script path.

# The structure of basedir should be the following:
# submit.sh: The bash script to submit a job.
# config: The files containing the parameters.
# - config/jobs.csv
# - config/resource_tiers.csv
# - config/shared_resources.csv
# - config/root_paths.csv
# slurm: The template script.
# - slurm/job_template.sh
# r_scripts: The required R scripts.
# - r_scripts/create_batch_job.R
# - r_scripts/submit_job.R
# generated: The generated batch job script.
# - eg. generated/denmark_fagus.sh

create_batch_job <- function(
    job_name,
    generated_dir,
    output_name = job_name,
    extra_values = list(),
    config_file = "config/config.yml"
) {
  
  template_file <- file.path(
    "slurm",
    "job_template.sh"
  )
  
  if (!file.exists(template_file)) {
    stop("Missing batch template file")
  }
  
  cfg <- load_job_config(config_file)
  
  required_cols <- c(
    "job_name",
    "entry_point",
    "tier"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(cfg$jobs)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "jobs.csv missing columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
    
  }
  
  job <- get_row(
    cfg$jobs,
    "job_name",
    job_name
  )
  
  job <- resolve_job_paths(
    job,
    cfg$root_paths
  )
  # Resolve all roots separately
  roots <- setNames(
    cfg$root_paths$path,
    paste0(cfg$root_paths$root_name, "_ROOT")
  )
  
  job <- c(job, as.list(roots))
  
  tier <- get_row(
    cfg$tiers,
    "tier",
    job$tier
  )
  
  shared_vals <- setNames(
    cfg$shared$value,
    cfg$shared$key
  )
  
  template <- paste(
    readLines(template_file),
    collapse = "\n"
  )
  
  replacements <- build_replacements(
    job,
    tier,
    shared_vals,
    extra_values
  )
  
  template <- render_template(
    template,
    replacements
  )
  
  output_dir <- generated_dir
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  job_file <- file.path(
    output_dir,
    paste0(output_name, ".sh")
  )
  
  writeLines(
    template,
    job_file
  )
  
  Sys.chmod(
    job_file,
    mode = "755"
  )
  
  job_file
}

