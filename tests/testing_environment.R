# First, install the testthat package if you don't have it
install.packages("testthat")

# If you want to run these tests within your package directory:
library(testthat)

# Create the tests directory structure
dir.create("tests", showWarnings = FALSE)
dir.create("tests/testthat", showWarnings = FALSE)
dir.create("tests/fixtures", showWarnings = FALSE)

# Create a test file for core utilities
writeLines(
  'context("Core Utilities")

# Setup a temporary log file for testing
temp_log <- tempfile()

test_that("logging functions work correctly", {
  # Test log_info
  expect_silent(log_info("Test info message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("INFO - Test info message", log_content)))
  
  # Test log_warning
  expect_silent(log_warning("Test warning message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("WARNING - Test warning message", log_content)))
  
  # Test log_error
  expect_silent(log_error("Test error message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("ERROR - Test error message", log_content)))
})

test_that("standardize_field_name works correctly", {
  expect_equal(standardize_field_name("First Name"), "first_name")
  expect_equal(standardize_field_name("CustomerID"), "customer_id")
  expect_equal(standardize_field_name("user-email@domain.com"), "user_email_domain_com")
  expect_equal(standardize_field_name("  Spaces  Around  "), "spaces_around")
})
', 
  "tests/testthat/test-core-utilities.R")

# Create a main test file
writeLines(
  'library(testthat)
library(harmonizeR)

test_check("harmonizeR")
', 
  "tests/testthat.R")

# Create a simple fixture file for testing file detection
write.csv(data.frame(id = 1:5, name = c("A", "B", "C", "D", "E")),
          "tests/fixtures/sample.csv")

devtools::test() 
