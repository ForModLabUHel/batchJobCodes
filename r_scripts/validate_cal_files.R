# File validation helpers for calibration runs.

validate_cal_file_format <- function(file_paths,
                                     extension = "rdata") {
  extension <- sub("^\\.", "", tolower(extension))
  errors <- character()

  for (file_path in file_paths) {
    file_name <- basename(file_path)

    if (!grepl(paste0("\\.", extension, "$"),
               file_name,
               ignore.case = TRUE)) {
      errors <- c(
        errors,
        paste0(
          "Incorrect file format: '", file_name,
          "' must use .", extension
        )
      )
      next
    }

    if (!file.exists(file_path)) {
      errors <- c(
        errors,
        paste0("Missing file: '", normalizePath(
          file_path,
          winslash = "/",
          mustWork = FALSE
        ), "'")
      )
    }
  }

  errors
}

validate_cal_init_file <- function(directory = ".",
                                   init_name = "calInit",
                                   extension = "rdata") {
  extension <- sub("^\\.", "", tolower(extension))
  errors <- character()

  if (!dir.exists(directory)) {
    return(paste0("Input directory does not exist: '", directory, "'"))
  }

  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)

  init_path_pattern <- paste0(
    "^", escape_regex(init_name), "\\.",
    escape_regex(extension), "$"
  )

  files <- list.files(directory, all.files = FALSE)
  exact_matches <- files[
    grepl(init_path_pattern, files, ignore.case = TRUE)
  ]

  if (length(exact_matches) == 0) {
    errors <- c(
      errors,
      paste0(
        "Missing init file: '", init_name, ".", extension, "'"
      )
    )
  } else if (length(exact_matches) > 1) {
    errors <- c(
      errors,
      paste0(
        "Duplicate init file: '", init_name, ".", extension, "'"
      )
    )
  } else {
    errors <- c(
      errors,
      validate_cal_file_format(
        file.path(directory, exact_matches),
        extension
      )
    )
  }

  matching_names <- files[
    grepl(
      paste0("^", escape_regex(init_name), "\\."),
      files,
      ignore.case = TRUE
    )
  ]

  for (file_name in setdiff(matching_names, exact_matches)) {
    errors <- c(
      errors,
      paste0(
        "Incorrect init file format: '", file_name,
        "'; expected '", init_name, ".", extension, "'"
      )
    )
  }

  errors
}

validate_cal_init_names <- function(file_names,
                                    init_name = "calInit",
                                    extension = "rdata") {
  extension <- sub("^\\.", "", tolower(extension))
  pattern <- paste0(
    "^", escape_regex(init_name), "(?:\\.[^.]+)?$"
  )
  matches <- file_names[
    grepl(pattern, file_names, ignore.case = TRUE)
  ]

  if (length(matches) == 0) {
    return(paste0(
      "Missing init file: '", init_name, ".", extension, "'"
    ))
  }

  exact_pattern <- paste0(
    "^", escape_regex(init_name), "\\.",
    escape_regex(extension), "$"
  )
  exact_matches <- matches[
    grepl(exact_pattern, matches, ignore.case = TRUE)
  ]

  if (length(exact_matches) > 1) {
    return(paste0(
      "Duplicate init file: '", init_name, ".", extension, "'"
    ))
  }

  character()
}

validate_cal_output_names <- function(file_names,
                                      n_sets,
                                      n_chains,
                                      output_prefix = "calOut",
                                      extension = "rdata") {
  extension <- sub("^\\.", "", tolower(extension))
  pattern <- paste0(
    "^", escape_regex(output_prefix),
    "_([0-9]+)_([0-9]+)(?:\\.([^.]+))?$"
  )

  parsed <- list()
  errors <- character()

  for (file_name in file_names) {
    match <- regmatches(
      file_name,
      regexec(pattern, file_name, ignore.case = TRUE)
    )[[1]]
    if (length(match) == 0) {
      next
    }

    set_id <- suppressWarnings(as.integer(match[[2]]))
    chain_id <- suppressWarnings(as.integer(match[[3]]))
    found_extension <- if (length(match) >= 4) match[[4]] else NA_character_

    if (!is.na(set_id) && set_id >= 1 && set_id <= n_sets &&
        !is.na(chain_id) && chain_id >= 1 && chain_id <= n_chains) {
      key <- paste(set_id, chain_id, sep = "_")
      parsed[[key]] <- c(parsed[[key]], file_name)
    }
  }

  for (key in names(parsed)[lengths(parsed) > 1]) {
    set_chain <- strsplit(key, "_", fixed = TRUE)[[1]]
    errors <- c(
      errors,
      paste0(
        "Duplicate calibration output for set ", set_chain[[1]],
        ", chain ", set_chain[[2]]
      )
    )
  }

  errors
}

validate_cal_output_files <- function(directory = ".",
                                      n_sets,
                                      n_chains,
                                      output_prefix = "calOut",
                                      extension = "rdata",
                                      init_name = "calInit") {
  extension <- sub("^\\.", "", tolower(extension))
  errors <- character()

  if (!dir.exists(directory)) {
    return(paste0("Input directory does not exist: '", directory, "'"))
  }

  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  if (!is_single_positive_integer(n_sets)) {
    errors <- c(errors, "n_sets must be a positive integer")
  }

  if (!is_single_positive_integer(n_chains)) {
    errors <- c(errors, "n_chains must be a positive integer")
  }

  if (length(errors) > 0) {
    return(errors)
  }

  files <- list.files(directory, all.files = FALSE)
  output_pattern <- paste0(
    "^", escape_regex(output_prefix), "_([0-9]+)_([0-9]+)",
    "(\\.([^.]+))?$"
  )

  parsed_files <- list()

  for (file_name in files) {
    match <- regmatches(
      file_name,
      regexec(output_pattern, file_name, ignore.case = TRUE)
    )[[1]]

    if (length(match) == 0) {
      if (grepl(
        paste0("^", escape_regex(output_prefix), "_"),
        file_name,
        ignore.case = TRUE
      )) {
        errors <- c(
          errors,
          paste0(
            "Malformed calibration output file name: '", file_name,
            "'; expected '", output_prefix,
            "_<set>_<chain>.", extension, "'"
          )
        )
      }
      next
    }

    set_id <- suppressWarnings(as.integer(match[[2]]))
    chain_id <- suppressWarnings(as.integer(match[[3]]))
    found_extension <- if (length(match) >= 5) match[[5]] else NA_character_

    if (is.na(set_id) || set_id < 1 || set_id > n_sets) {
      errors <- c(
        errors,
        paste0(
          "Invalid set ID in '", file_name,
          "': expected 1-", n_sets, ", found ", match[[2]]
        )
      )
    }

    if (is.na(chain_id) || chain_id < 1 || chain_id > n_chains) {
      errors <- c(
        errors,
        paste0(
          "Invalid chain ID in '", file_name,
          "': expected 1-", n_chains, ", found ", match[[3]]
        )
      )
    }

    if (!is.na(set_id) && set_id >= 1 && set_id <= n_sets &&
        !is.na(chain_id) && chain_id >= 1 && chain_id <= n_chains) {
      key <- paste(set_id, chain_id, sep = "_")
      parsed_files[[key]] <- c(parsed_files[[key]], file_name)
    }

    if (is.na(found_extension) ||
        !grepl(paste0("^", extension, "$"),
               found_extension,
               ignore.case = TRUE)) {
      errors <- c(
        errors,
        paste0(
          "Incorrect file format: '", file_name,
          "' must use .", extension
        )
      )
      next
    }

    errors <- c(
      errors,
      validate_cal_file_format(
        file.path(directory, file_name),
        extension
      )
    )
  }

  duplicate_keys <- names(parsed_files)[lengths(parsed_files) > 1]
  for (key in duplicate_keys) {
    set_chain <- strsplit(key, "_", fixed = TRUE)[[1]]
    errors <- c(
      errors,
      paste0(
        "Duplicate calibration output for set ", set_chain[[1]],
        ", chain ", set_chain[[2]], ": ",
        paste(sort(parsed_files[[key]]), collapse = ", ")
      )
    )
  }

  init_file <- paste0(init_name, ".", extension)
  init_exists <- init_file %in% files

  for (set_id in seq_len(n_sets)) {
    for (chain_id in seq_len(n_chains)) {
      output_key <- paste(set_id, chain_id, sep = "_")
      if (is.null(parsed_files[[output_key]])) {
        next
      }

      output_file <- paste0(
        output_prefix, "_", set_id, "_", chain_id, ".", extension
      )

      if (set_id == 1) {
        if (!init_exists) {
          errors <- c(
            errors,
            paste0(
              "Missing input '", init_file,
              "' for existing output '", output_file, "'"
            )
          )
        }
        next
      }

      input_set_id <- set_id - 1
      input_key <- paste(input_set_id, chain_id, sep = "_")
      if (is.null(parsed_files[[input_key]])) {
        input_file <- paste0(
          output_prefix, "_", input_set_id, "_",
          chain_id, ".", extension
        )
        errors <- c(
          errors,
          paste0(
            "Missing input '", input_file,
            "' for existing output '", output_file,
            "' (set ", input_set_id, ", chain ", chain_id,
            " is incomplete)"
          )
        )
      }
    }
  }

  unique(errors)
}

validate_cal_files <- function(directory = ".",
                               n_sets,
                               n_chains,
                               init_name = "calInit",
                               output_prefix = "calOut",
                               extension = "rdata",
                               throw = TRUE) {
  errors <- c(
    validate_cal_init_file(directory, init_name, extension),
    validate_cal_output_files(
      directory,
      n_sets,
      n_chains,
      output_prefix,
      extension,
      init_name
    ),
    validate_cal_init_names(
      list.files(directory, all.files = FALSE),
      init_name,
      extension
    ),
    validate_cal_output_names(
      list.files(directory, all.files = FALSE),
      n_sets,
      n_chains,
      output_prefix,
      extension
    )
  )
  errors <- unique(errors)

  if (throw && length(errors) > 0) {
    stop(
      paste0(
        "Calibration file validation failed:\n",
        paste0("- ", errors, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  invisible(errors)
}

escape_regex <- function(text) {
  gsub("([][{}()+*?$^\\\\.|])", "\\\\\\1", text)
}

is_single_positive_integer <- function(value) {
  is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    value >= 1 &&
    value == as.integer(value)
}
