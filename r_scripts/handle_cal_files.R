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
        original_set_id = set_id,
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
  run_table <- scan_cal_files(
    directory,
    n_sets,
    n_chains,
    init_name,
    output_prefix,
    extension
  )

  # A missing output or input makes this chain rerun. Then every downstream
  # chain with the same chain ID is rerun too.
  for (set_id in seq_len(n_sets)) {
    for (chain_id in seq_len(n_chains)) {
      row <- run_table[
        run_table$set_id == set_id &
        run_table$chain_id == chain_id,
      ]
      if (nrow(row) == 1 && row$needs_run) {
        for (downstream_set in seq(set_id + 1L, n_sets)) {
          if (downstream_set > n_sets) {
            break
          }
          downstream <- run_table$set_id == downstream_set &
            run_table$chain_id == chain_id
          run_table$needs_run[downstream] <- TRUE
          run_table$reason[downstream] <- paste0(
            "upstream set ", set_id,
            ", chain ", chain_id, " needs to run"
          )
        }
      }
    }
  }

  run_table <- run_table[run_table$needs_run, ]
  if (nrow(run_table) == 0) {
    return(data.frame(
      set_id = integer(),
      original_set_id = integer(),
      chain_id = integer(),
      input_file = character(),
      output_file = character(),
      needs_run = logical(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  first_incomplete_set <- min(run_table$set_id)
  original_sets <- sort(unique(
    run_table$set_id[run_table$set_id >= first_incomplete_set]
  ))

  for (submission_set_id in seq_along(original_sets)) {
    original_set_id <- original_sets[[submission_set_id]]
    run_table$set_id[
      run_table$set_id == original_set_id
    ] <- submission_set_id
  }

  run_table
}

handle_cal_files <- create_cal_run_table

read_cal_run_table_row <- function(run_table_file,
                                   original_set_id,
                                   chain_id) {
  if (!nzchar(run_table_file) ||
      !file.exists(run_table_file)) {
    stop("Calibration run table does not exist: '",
         run_table_file, "'", call. = FALSE)
  }

  run_table <- utils::read.table(
    run_table_file,
    sep = "|",
    header = TRUE,
    stringsAsFactors = FALSE
  )

  rows <- run_table[
    run_table$original_set_id == original_set_id &
    run_table$chain_id == chain_id,
  ]

  if (nrow(rows) != 1) {
    stop(
      "Run table has ", nrow(rows),
      " rows for original set ", original_set_id,
      ", chain ", chain_id,
      call. = FALSE
    )
  }

  rows
}

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
