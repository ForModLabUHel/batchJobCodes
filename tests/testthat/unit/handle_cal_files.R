library(testthat)

repository_root <- normalizePath(
  file.path(getwd(), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
source(file.path(repository_root, "r_scripts", "handle_cal_files.R"))

make_directory <- function(file_names = character()) {
  directory <- tempfile("cal-handler-")
  dir.create(directory)
  for (file_name in file_names) {
    file.create(file.path(directory, file_name))
  }
  directory
}

test_that("init only creates the full ordered run table", {
  directory <- make_directory("calInit.rdata")

  table <- create_cal_run_table(directory, 2, 2)

  expect_equal(table$set_id, c(1, 1, 2, 2))
  expect_equal(table$chain_id, c(1, 2, 1, 2))
  expect_equal(table$input_file, c(
    "calInit.rdata",
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata"
  ))
  expect_equal(table$output_file, c(
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  ))
  expect_true(all(table$needs_run))
})

test_that("highest partial set runs only missing chains", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata"
  ))

  table <- create_cal_run_table(directory, 2, 2)

  expect_equal(nrow(table), 1)
  expect_equal(table$set_id, 1)
  expect_equal(table$chain_id, 2)
  expect_equal(table$input_file, "calOut_1_2.rdata")
  expect_equal(table$output_file, "calOut_2_2.rdata")
  expect_true(table$needs_run)
})

test_that("partial highest set is followed by remaining sets", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_1_3.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  ))

  table <- create_cal_run_table(directory, 4, 3)

  expect_equal(
    table$chain_id[table$set_id == 1],
    3
  )
  expect_equal(
    table$set_id,
    c(1, 2, 2, 2, 3, 3, 3)
  )
  expect_equal(
    table$output_file[
      table$set_id == 2 & table$chain_id == 1
    ],
    "calOut_3_1.rdata"
  )
})

test_that("complete highest set starts the next set", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  ))

  table <- create_cal_run_table(directory, 4, 2)

  expect_equal(table$set_id, c(1, 1, 2, 2))
  expect_equal(table$chain_id, c(1, 2, 1, 2))
  expect_equal(table$input_file, c(
    "calOut_2_1.rdata",
    "calOut_2_2.rdata",
    "calOut_3_1.rdata",
    "calOut_3_2.rdata"
  ))
  expect_equal(table$output_file, c(
    "calOut_3_1.rdata",
    "calOut_3_2.rdata",
    "calOut_4_1.rdata",
    "calOut_4_2.rdata"
  ))
})

test_that("run after set 18 creates only sets 1 and 2", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_18_1.rdata",
    "calOut_18_2.rdata"
  ))

  table <- create_cal_run_table(directory, 20, 2)

  expect_equal(sort(unique(table$set_id)), c(1, 2))
  expect_true(all(table$output_file == c(
    "calOut_19_1.rdata",
    "calOut_19_2.rdata",
    "calOut_20_1.rdata",
    "calOut_20_2.rdata"
  )))
})

test_that("a complete run creates no remaining rows", {
  directory <- make_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata",
    "calOut_2_2.rdata"
  ))

  table <- create_cal_run_table(directory, 2, 2)

  expect_equal(nrow(table), 0)
  expect_true(is_cal_run_complete(directory, 2, 2))
})

test_that("scan exposes input and output for each set and chain", {
  directory <- make_directory("calInit.rdata")
  table <- scan_cal_files(directory, 2, 2)

  expect_true(all(c(
    "set_id",
    "chain_id",
    "input_file",
    "output_file",
    "needs_run",
    "reason"
  ) %in% names(table)))
})

test_that("array specs use partial and full array ranges", {
  table <- data.frame(
    set_id = c(1, 1, 1, 2, 2, 2),
    chain_id = c(1, 2, 3, 1, 2, 3),
    needs_run = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )

  specs <- create_cal_array_specs(table)

  expect_equal(specs$array_spec[specs$set_id == 1], "1-2")
  expect_equal(specs$array_spec[specs$set_id == 2], "1-3")
})
