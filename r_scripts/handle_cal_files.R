# Build the calibration run table from the files that already exist.

check_cal_integer <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 ||
      is.na(value) || value < 1 || value != as.integer(value)) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
}

cal_output_file_name <- function(output_prefix, set_id, chain_id, extension) {
  paste0(output_prefix, "_", set_id, "_", chain_id, ".", extension)
}

scan_cal_files <- function(directory = ".",
                           n_sets,
                           n_chains,
                           init_name = "calInit",
                           output_prefix = "calOut",
                           extension = "rdata") {
  check_cal_integer(n_sets, "n_sets")
  check_cal_integer(n_chains, "n_chains")

  if (!dir.exists(directory)) {
    stop("Input directory does not exist: '", directory, "'", call. = FALSE)
  }

  extension <- sub("^\\.", "", tolower(extension))
  files <- list.files(directory, all.files = FALSE, full.names = FALSE)
  init_file <- paste0(init_name, ".", extension)
  rows <- list()

  for (set_id in seq_len(n_sets)) {
    for (chain_id in seq_len(n_chains)) {
      input_file <- if (set_id == 1) {
        init_file
      } else {
        cal_output_file_name(
          output_prefix, set_id - 1, chain_id, extension
        )
      }
      output_file <- cal_output_file_name(
        output_prefix, set_id, chain_id, extension
      )

      input_exists <- input_file %in% files
      output_exists <- output_file %in% files
      needs_run <- !input_exists || !output_exists

      reason <- if (!input_exists) {
        paste0("input file is missing: ", input_file)
      } else if (!output_exists) {
        paste0("output file is missing: ", output_file)
      } else {
        "input and output are available"
      }

      rows[[length(rows) + 1]] <- data.frame(
        set_id = set_id,
        chain_id = chain_id,
        input_file = input_file,
        output_file = output_file,
        needs_run = needs_run,
        reason = reason,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

create_cal_run_table <- function(directory = ".",
                                 n_sets,
                                 n_chains,
                                 init_name = "calInit",
                                 output_prefix = "calOut",
                                 extension = "rdata") {
  check_cal_integer(n_sets, "n_sets")
  check_cal_integer(n_chains, "n_chains")

  if (!dir.exists(directory)) {
    stop("Input directory does not exist: '", directory, "'", call. = FALSE)
  }

  extension <- sub("^\\.", "", tolower(extension))
  files <- list.files(directory, all.files = FALSE, full.names = FALSE)
  init_file <- paste0(init_name, ".", extension)

  highest_set <- 0L
  for (set_id in seq_len(n_sets)) {
    has_output <- any(vapply(seq_len(n_chains), function(chain_id) {
      cal_output_file_name(
        output_prefix, set_id, chain_id, extension
      ) %in% files
    }, logical(1)))

    if (has_output) {
      highest_set <- set_id
    }
  }

  if (highest_set == 0L) {
    first_set <- 1L
  } else {
    complete_chains <- vapply(seq_len(n_chains), function(chain_id) {
      cal_output_file_name(
        output_prefix, highest_set, chain_id, extension
      ) %in% files
    }, logical(1))

    if (all(complete_chains)) {
      first_set <- highest_set + 1L
    } else {
      first_set <- highest_set
    }
  }

  if (first_set > n_sets) {
    return(data.frame(
      set_id = integer(),
      chain_id = integer(),
      input_file = character(),
      output_file = character(),
      needs_run = logical(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  first_set_is_partial <- FALSE

  if (highest_set > 0L) {
    complete_chains <- vapply(seq_len(n_chains), function(chain_id) {
      cal_output_file_name(
        output_prefix, highest_set, chain_id, extension
      ) %in% files
    }, logical(1))
    first_set_is_partial <- first_set == highest_set && !all(complete_chains)
  }

  remaining_set_number <- 1L
  for (original_set_id in seq(first_set, n_sets)) {
    chain_ids <- seq_len(n_chains)

    if (first_set_is_partial && original_set_id == first_set) {
      chain_ids <- chain_ids[
        !vapply(chain_ids, function(chain_id) {
          cal_output_file_name(
            output_prefix, original_set_id, chain_id, extension
          ) %in% files
        }, logical(1))
      ]
    }

    for (chain_id in chain_ids) {
      input_file <- if (original_set_id == 1L) {
        init_file
      } else {
        cal_output_file_name(
          output_prefix, original_set_id - 1L, chain_id, extension
        )
      }
      output_file <- cal_output_file_name(
        output_prefix, original_set_id, chain_id, extension
      )

      rows[[length(rows) + 1]] <- data.frame(
        set_id = remaining_set_number,
        chain_id = chain_id,
        input_file = input_file,
        output_file = output_file,
        needs_run = TRUE,
        reason = paste0(
          "calibration set ", original_set_id,
          ", chain ", chain_id, " needs to run"
        ),
        stringsAsFactors = FALSE
      )
    }

    remaining_set_number <- remaining_set_number + 1L
  }

  do.call(rbind, rows)
}

handle_cal_files <- create_cal_run_table

create_cal_array_specs <- function(run_table) {
  set_ids <- sort(unique(run_table$set_id))

  specs <- lapply(set_ids, function(set_id) {
    set_rows <- run_table[run_table$set_id == set_id, ]
    required_rows <- sort(unique(set_rows$chain_id[set_rows$needs_run]))

    array_spec <- if (length(required_rows) == 0) {
      ""
    } else if (length(required_rows) == 1) {
      as.character(required_rows)
    } else if (length(required_rows) == max(required_rows) -
               min(required_rows) + 1L &&
               all(required_rows == seq.int(
                 min(required_rows),
                 max(required_rows)
               ))) {
      paste0(min(required_rows), "-", max(required_rows))
    } else {
      paste(required_rows, collapse = ",")
    }

    data.frame(
      set_id = set_id,
      needs_run = length(required_rows) > 0,
      array_spec = array_spec,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, specs)
}

is_cal_run_complete <- function(directory = ".",
                                n_sets,
                                n_chains,
                                init_name = "calInit",
                                output_prefix = "calOut",
                                extension = "rdata") {
  extension <- sub("^\\.", "", tolower(extension))

  if (!dir.exists(directory)) {
    return(FALSE)
  }

  files <- list.files(directory, all.files = FALSE, full.names = FALSE)
  if (!paste0(init_name, ".", extension) %in% files) {
    return(FALSE)
  }

  for (set_id in seq_len(n_sets)) {
    for (chain_id in seq_len(n_chains)) {
      if (!cal_output_file_name(
        output_prefix, set_id, chain_id, extension
      ) %in% files) {
        return(FALSE)
      }
    }
  }

  TRUE
}
