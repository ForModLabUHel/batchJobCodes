library(testthat)

test_files <- list.files(
  "tests/testthat/unit",
  pattern = "\\.R$",
  full.names = TRUE
)

if (length(test_files) == 0) {
  stop("No test files found")
}

for (test_file in test_files) {
  testthat::test_file(test_file, reporter = "summary")
}
