library(testthat)

repository_root <- normalizePath(
  file.path(getwd(), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
source(file.path(repository_root, "r_scripts", "validate_cal_files.R"))

created_temp_dirs <- character()

make_temp_directory <- function(prefix) {
  directory <- tempfile(prefix)
  dir.create(directory)
  created_temp_dirs <<- c(created_temp_dirs, directory)
  directory
}

teardown({
  unlink(created_temp_dirs, recursive = TRUE, force = TRUE)
})

make_directory <- function(file_names = character()) {
  directory <- make_temp_directory("cal-validation-")

  for (file_name in file_names) {
    file_path <- file.path(directory, file_name)
    if (grepl("\\.rdata$", file_name, ignore.case = TRUE)) {
      value <- 1
      save(value, file = file_path)
    } else {
      writeLines("invalid", file_path)
    }
  }

  directory
}

valid_files <- function() {
  c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  )
}

test_that("valid files pass validation", {
  directory <- make_directory(valid_files())

  expect_length(
    validate_cal_files(directory, 2, 2, throw = FALSE),
    0
  )
})

test_that("missing init is reported for an existing output", {
  directory <- make_directory("calOut_1_1.rdata")

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl("Missing init file", errors)))
  expect_true(any(grepl("Missing input.*calInit.rdata", errors)))
})

test_that("later set output requires the previous set chain", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_2_2.rdata"
  ))

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl(
    "Missing input.*calOut_1_2.rdata.*calOut_2_2.rdata",
    errors
  )))
})

test_that("partial highest set is valid resume state", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata"
  ))

  expect_length(
    validate_cal_files(directory, 2, 2, throw = FALSE),
    0
  )
})

test_that("later set chain cannot exist without earlier chain", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_2_2.rdata"
  ))

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl(
    "Missing input.*calOut_1_2.rdata",
    errors
  )))
})

test_that("complete later set cannot hide missing earlier set chains", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  ))

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl(
    "Missing input.*calOut_1_2.rdata",
    errors
  )))
})

test_that("set and chain IDs must be within configured ranges", {
  directory <- make_directory(
    c(valid_files(), "calOut_1_3.rdata", "calOut_3_1.rdata")
  )

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl("Invalid chain ID.*found 3", errors)))
  expect_true(any(grepl("Invalid set ID.*found 3", errors)))
})

test_that("wrong init and output formats are reported", {
  directory <- make_directory(
    c(
      "calInit.csv",
      "calOut_1_1.txt",
      "calOut_1_2.rdata",
      "calOut_2_1.rdata",
      "calOut_2_2.rdata"
    )
  )

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl("Incorrect init file format", errors)))
  expect_true(any(grepl("calOut_1_1.txt", errors)))
})

test_that("malformed names are reported", {
  directory <- make_directory(
    c(
      "calInit.rdata",
      "calOut_bad.rdata",
      "calOut_1_1.rdata",
      "calOut_1_2.rdata",
      "calOut_2_1.rdata",
      "calOut_2_2.rdata"
    )
  )

  errors <- validate_cal_files(directory, 2, 2, throw = FALSE)

  expect_true(any(grepl("Malformed calibration output", errors)))
})

test_that("duplicates are reported", {
  init_errors <- validate_cal_init_names(
    c("calInit.rdata", "calInit.RData")
  )
  output_errors <- validate_cal_output_names(
    c("calOut_1_1.rdata", "calOut_1_1.RData"),
    2,
    2
  )

  expect_true(any(grepl("Duplicate init file", init_errors)))
  expect_true(any(grepl("Duplicate calibration output", output_errors)))
})

test_that("all errors are included in one thrown error", {
  directory <- make_directory(
    c("calOut_1_3.rdata", "calOut_3_1.rdata")
  )

  thrown <- tryCatch(
    validate_cal_files(directory, 2, 2),
    error = identity
  )

  message <- conditionMessage(thrown)
  expect_true(grepl("Missing init file", message))
  expect_true(grepl("Invalid chain ID", message))
  expect_true(grepl("Invalid set ID", message))
})
