library(testthat)

repository_root <- normalizePath(
  file.path(getwd(), "..", "..", ".."),
  winslash = "/",
  mustWork = TRUE
)

source(file.path(repository_root, "r_scripts", "create_batch_job.R"))
source(file.path(repository_root, "r_scripts", "handle_cal_files.R"))
source(file.path(repository_root, "r_scripts", "validate_cal_files.R"))
source(file.path(repository_root, "r_scripts", "create_submission_plan.R"))

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

make_calibration_directory <- function(file_names = character()) {
  directory <- make_temp_directory("cal-plan-input-")

  for (file_name in file_names) {
    file_path <- file.path(directory, file_name)
    if (grepl("\\.(rdata|rds)$", file_name, ignore.case = TRUE)) {
      value <- 1
      save(value, file = file_path)
    } else {
      file.create(file_path)
    }
  }

  directory
}

make_calibration_config <- function(n_sets, n_chains,
                                    calibration_params = character()) {
  directory <- make_temp_directory("cal-plan-config-")
  config_file <- file.path(directory, "config.yml")

  writeLines(c(
    "jobs:",
    "  calibration:",
    "    entry_point: run.R",
    "    tier: small",
    paste0("    n_sets: ", n_sets),
    paste0("    n_chains: ", n_chains),
    "  reporting:",
    "    entry_point: report.R",
    "    tier: small",
    "resource_tiers:",
    "  small:",
    "    cpus_per_task: 1",
    "    mem: 1G",
    "    time: \"00:10:00\"",
    "shared_resources:",
    "  partition: test",
    "  account: test",
    "  output: test.out",
    "  error: test.err",
    "  ntasks: 1",
    "  nodes: 1",
    "root_paths:",
    "  TEST: /tmp/test"
  ), config_file)

  config_file
}

fake_create_job <- function(job_name,
                            generated_dir,
                            config_file,
                            extra_values,
                            output_name) {
  dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)
  job_file <- file.path(generated_dir, paste0(output_name, ".sh"))
  writeLines(
    paste(
      job_name,
      extra_values$set_id,
      extra_values$array_spec,
      sep = "|"
    ),
    job_file
  )
  job_file
}

test_that("init-only state creates the full file-based plan", {
  input_directory <- make_calibration_directory("calInit.rdata")
  config_file <- make_calibration_config(2, 3)
  generated_dir <- make_temp_directory("cal-plan-output-")

  plan <- create_file_based_submission_plan(
    job_name = "calibration",
    input_directory = input_directory,
    generated_dir = generated_dir,
    config_file = config_file,
    framework_dir = repository_root,
    create_job = fake_create_job
  )

  expect_equal(plan$job_key, c("calibration_1", "calibration_2"))
  expect_equal(plan$depends_on, c("", "calibration_1"))
  expect_true(all(file.exists(plan$job_file)))

  first_script <- readLines(plan$job_file[[1]])
  expect_equal(first_script, "calibration|1|1-3")
})

test_that("partial highest set creates partial array and remaining sets", {
  input_directory <- make_calibration_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_1_2.rdata",
    "calOut_2_1.rdata"
  ))
  config_file <- make_calibration_config(4, 3)
  generated_dir <- make_temp_directory("cal-plan-output-")

  plan <- create_file_based_submission_plan(
    job_name = "calibration",
    input_directory = input_directory,
    generated_dir = generated_dir,
    config_file = config_file,
    framework_dir = repository_root,
    create_job = fake_create_job
  )

  expect_equal(plan$job_key, c(
    "calibration_1",
    "calibration_2",
    "calibration_3",
    "calibration_4"
  ))

  scripts <- lapply(plan$job_file, readLines)
  expect_equal(scripts[[1]], "calibration|1|3")
  expect_equal(scripts[[2]], "calibration|2|2-3")
  expect_equal(scripts[[3]], "calibration|3|1-3")
  expect_equal(scripts[[4]], "calibration|4|1-3")
})

test_that("reporting job can be inserted between sets", {
  input_directory <- make_calibration_directory("calInit.rdata")
  config_file <- make_calibration_config(3, 2)
  generated_dir <- make_temp_directory("cal-plan-output-")

  plan <- create_file_based_submission_plan(
    job_name = "calibration",
    input_directory = input_directory,
    generated_dir = generated_dir,
    config_file = config_file,
    framework_dir = repository_root,
    reporting_job_name = "reporting",
    create_job = fake_create_job
  )

  expect_equal(plan$job_key, c(
    "calibration_1",
    "calibration_report_1",
    "calibration_2",
    "calibration_report_2",
    "calibration_3"
  ))
  expect_equal(
    plan$depends_on,
    c(
      "",
      "calibration_1",
      "calibration_report_1",
      "calibration_2",
      "calibration_report_2"
    )
  )
})

test_that("invalid validator state prevents plan creation", {
  input_directory <- make_calibration_directory(c(
    "calInit.rdata",
    "calOut_1_1.rdata",
    "calOut_2_2.rdata"
  ))
  config_file <- make_calibration_config(2, 2)

  expect_error(
    create_file_based_submission_plan(
      job_name = "calibration",
      input_directory = input_directory,
      generated_dir = make_temp_directory("cal-plan-output-"),
      config_file = config_file,
      framework_dir = repository_root,
      create_job = fake_create_job
    ),
    "Calibration file validation failed"
  )
})

test_that("custom calibration names are passed to validator and handler", {
  input_directory <- make_calibration_directory(c(
    "start.rdata",
    "result_1_1.rdata"
  ))
  config_file <- make_calibration_config(2, 2)

  plan <- create_file_based_submission_plan(
    job_name = "calibration",
    input_directory = input_directory,
    generated_dir = make_temp_directory("cal-plan-output-"),
    config_file = config_file,
    framework_dir = repository_root,
    init_name = "start",
    output_prefix = "result",
    extension = "rdata",
    create_job = fake_create_job
  )

  expect_equal(nrow(plan), 2)
  expect_equal(readLines(plan$job_file[[1]]), "calibration|1|2")
})
