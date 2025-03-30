# Load required libraries
library(testthat)
source_dir("scripts/05_R package scripts/harmonizeR-package.r")

# Create a test file for core utility functions
test_that("Logging functions work correctly", {
  # Create a temporary log file for testing
  temp_log <- tempfile()
  
  # Test log_info function
  expect_invisible(log_info("Test info message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("INFO - Test info message", log_content)))
  
  # Test log_warning function
  expect_invisible(log_warning("Test warning message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("WARNING - Test warning message", log_content)))
  
  # Test log_error function
  expect_invisible(log_error("Test error message", log_file = temp_log))
  log_content <- readLines(temp_log)
  expect_true(any(grepl("ERROR - Test error message", log_content)))
  
  # Clean up
  file.remove(temp_log)
})

test_that("standardize_field_name handles various input formats", {
  # Test basic functionality
  expect_equal(standardize_field_name("First Name"), "first_name")
  expect_equal(standardize_field_name("CustomerID"), "customer_id")
  
  # Test with special characters
  expect_equal(standardize_field_name("User-Email"), "user_email")
  expect_equal(standardize_field_name("Product#ID"), "product_id")
  
  # Test with multiple spaces and special characters
  expect_equal(standardize_field_name("  Multiple   Spaces  "), "multiple_spaces")
  expect_equal(standardize_field_name("Multiple___Underscores"), "multiple_underscores")
  
  # Test with mixed case formats
  expect_equal(standardize_field_name("camelCase"), "camel_case")
  expect_equal(standardize_field_name("PascalCase"), "pascal_case")
  expect_equal(standardize_field_name("snake_case"), "snake_case")
  expect_equal(standardize_field_name("UPPER_CASE"), "upper_case")
  
  # Test with leading/trailing underscores
  expect_equal(standardize_field_name("_leading_underscore"), "leading_underscore")
  expect_equal(standardize_field_name("trailing_underscore_"), "trailing_underscore")
})

test_that("scan_directory finds supported files", {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Create test files of different types
  write.csv(data.frame(a = 1:3, b = 4:6), file.path(temp_dir, "test.csv"))
  saveRDS(data.frame(x = 1:3, y = 4:6), file.path(temp_dir, "test.rds"))
  write.table(data.frame(c = 1:3, d = 4:6), file.path(temp_dir, "test.txt"), row.names = FALSE)
  
  # Run the function
  file_info <- scan_directory(temp_dir)
  
  # Test results
  expect_true(any(grepl("test.csv", file_info$filename)))
  expect_equal(sum(grepl("\\.csv$", file_info$filename)), 1)
  expect_false(any(grepl("test.rds", file_info$filename))) # Should not detect unsupported types
  expect_false(any(grepl("test.txt", file_info$filename))) # Should not detect unsupported types
  
  # Test returned dataframe structure
  expect_true(all(c("path", "filename", "extension", "size_bytes") %in% names(file_info)))
  expect_equal(file_info$extension[grepl("test.csv", file_info$filename)], "csv")
})

test_that("detect_delimiter correctly identifies CSV delimiters", {
  # Create temporary CSV files with different delimiters
  comma_csv <- tempfile(fileext = ".csv")
  semicolon_csv <- tempfile(fileext = ".csv")
  tab_csv <- tempfile(fileext = ".csv")
  pipe_csv <- tempfile(fileext = ".csv")
  
  # Write test data with different delimiters
  writeLines("a,b,c\n1,2,3\n4,5,6", comma_csv)
  writeLines("a;b;c\n1;2;3\n4;5;6", semicolon_csv)
  writeLines("a\tb\tc\n1\t2\t3\n4\t5\t6", tab_csv)
  writeLines("a|b|c\n1|2|3\n4|5|6", pipe_csv)
  
  # Test the function
  expect_equal(detect_delimiter(comma_csv), ",")
  expect_equal(detect_delimiter(semicolon_csv), ";")
  expect_equal(detect_delimiter(tab_csv), "\t")
  expect_equal(detect_delimiter(pipe_csv), "|")
  
  # Clean up
  file.remove(comma_csv, semicolon_csv, tab_csv, pipe_csv)
})

test_that("might_be_date correctly identifies date formats", {
  # Test with various date formats
  expect_true(might_be_date(c("2023-01-01", "2023-01-02", "2023-01-03")))
  expect_true(might_be_date(c("01/01/2023", "02/01/2023", "03/01/2023")))
  expect_true(might_be_date(c("01-01-2023", "02-01-2023", "03-01-2023")))
  expect_true(might_be_date(c("01.01.2023", "02.01.2023", "03.01.2023")))
  expect_true(might_be_date(c("20230101", "20230102", "20230103")))
  
  # Test with mixed formats that should still be detected
  expect_true(might_be_date(c("2023-01-01", "01/02/2023", "03.01.2023")))
  
  # Test with non-date formats
  expect_false(might_be_date(c("abc", "def", "ghi")))
  expect_false(might_be_date(c("123", "456", "789")))
  
  # Test with mixed date/non-date (should return false if less than 70% are dates)
  expect_false(might_be_date(c("2023-01-01", "not a date", "also not a date", "still not a date")))
})

test_that("analyze_dataframe correctly analyzes data structure", {
  # Create a test dataframe
  test_df <- data.frame(
    id = 1:5,
    name = c("A", "B", "C", "D", "E"),
    value = c(10.1, 20.2, 30.3, NA, 50.5),
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
    stringsAsFactors = FALSE
  )
  
  # Create file info structure
  file_info <- data.frame(
    path = "dummy_path",
    filename = "test.csv",
    extension = "csv",
    size_bytes = 1000,
    stringsAsFactors = FALSE
  )
  
  # Run analysis
  result <- analyze_dataframe(test_df, file_info)
  
  # Test result structure
  expect_equal(result$filename, "test.csv")
  expect_equal(result$row_count, 5)
  expect_equal(result$column_count, 4)
  expect_equal(length(result$columns), 4)
  
  # Test column details
  expect_true(all(c("id", "name", "value", "date") %in% names(result$column_details)))
  
  # Check specific column analysis
  id_details <- result$column_details$id
  expect_equal(id_details$null_count, 0)
  expect_equal(id_details$unique_values, 5)
  expect_equal(id_details$inferred_type, "numeric")
  
  # Check date column identification
  date_details <- result$column_details$date
  expect_equal(date_details$inferred_type, "datetime")
  
  # Check handling of NA values
  value_details <- result$column_details$value
  expect_equal(value_details$null_count, 1)
  expect_equal(value_details$null_percentage, 20)
})

# Run the tests
test_file("scripts/06_testscripts")