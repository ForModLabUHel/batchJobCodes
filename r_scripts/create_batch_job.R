# CREATE BATCH JOB WORKER -------------------------------------------------


# Resolve the full path for a file using a path_spec eg. PROJECT:Rsrc/runModel.R
# and a named root-path mapping.
resolve_root_path <- function(path_spec, roots) {
  
  parts <- strsplit(
    path_spec,
    ":",
    fixed = TRUE
  )[[1]]
  
  if (length(parts) != 2 || any(!nzchar(parts))) {
    stop("Invalid path specification: ", path_spec)
  }
  
  root_name <- parts[1]
  rel_path <- parts[2]
  
  root <- roots[[root_name]]

  if (is.null(root) || length(root) != 1 || is.na(root)) {
    stop("Unknown root path: ", root_name)
  }
  
  file.path(root, rel_path)
}

resolve_job_paths <- function(job, roots) {
  
  path_cols <- c(
    "entry_point",
    "runner_script",
    "database"
  )
  
  for (col in intersect(path_cols, names(job))) {
    
    if (!is.null(job[[col]]) &&
        length(job[[col]]) == 1 &&
        !is.na(job[[col]]) &&
        grepl("^[^:]+:", job[[col]])) {
      
      job[[col]] <- resolve_root_path(
        job[[col]],
        roots
      )
      
    }
    
  }
  
  job
}

# YAML config
load_job_config <- function(config_file) {
  
  raw <- yaml::read_yaml(config_file)
  
  required_sections <- c(
    "jobs",
    "resource_tiers",
    "shared_resources",
    "root_paths"
  )

  missing_sections <- setdiff(
    required_sections,
    names(raw)
  )

  if (length(missing_sections) > 0) {
    stop(
      "Missing YAML sections: ",
      paste(missing_sections, collapse = ", ")
    )
  }

  for (section in required_sections) {
    if (!is.list(raw[[section]])) {
      stop("YAML section '", section, "' must be a mapping")
    }
  }

  if (is.null(names(raw$jobs)) ||
      is.null(names(raw$resource_tiers)) ||
      is.null(names(raw$shared_resources)) ||
      is.null(names(raw$root_paths))) {
    stop(
      "jobs, resource_tiers, shared_resources, and root_paths ",
      "must be named YAML mappings"
    )
  }

  list(
    jobs = raw$jobs,
    resource_tiers = raw$resource_tiers,
    shared_resources = raw$shared_resources,
    root_paths = raw$root_paths
  )
}

# Get a named configuration entry
get_config_entry <- function(section, name, section_name) {
  
  value <- section[[name]]
  
  if (is.null(value)) {
    stop(
      "Unknown ",
      section_name,
      ": ",
      name
    )
  }
  
  value
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
  
  tier_vals <- tier
  tier_vals[["tier"]] <- NULL
  
  values <- c(
    job,
    tier_vals,
    shared_vals,
    extra_values
  )

  create_placeholders(values)
}


# CREATE BATCH JOB CONTROLLER ---------------------------------------------


# Creates a batch job script using a template with placeholders. Replaces
# the placeholders with parameters from config files. The identifier for the 
# parameters is given in job_name.
# Writes the batch script and sets permissions to "755". 
# Returns the batch script path.
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
  
  job <- get_config_entry(
    cfg$jobs,
    job_name,
    "job"
  )

  if (is.null(job$entry_point) || is.null(job$tier)) {
    stop(
      "Job '",
      job_name,
      "' must define entry_point and tier"
    )
  }

  job$job_name <- job_name

  job <- resolve_job_paths(
    job,
    cfg$root_paths
  )

  roots <- setNames(
    cfg$root_paths,
    paste0(names(cfg$root_paths), "_ROOT")
  )
  
  job <- c(job, as.list(roots))
  
  tier <- get_config_entry(
    cfg$resource_tiers,
    job$tier,
    "resource tier"
  )
  
  shared_vals <- cfg$shared_resources
  
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
