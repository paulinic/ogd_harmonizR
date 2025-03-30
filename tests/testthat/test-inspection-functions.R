# Load required libraries
library(testthat)
source_dir("scripts/05_R package scripts/harmonizeR-package.r")

# Test the inspection phase functions
test_that("scan_directory correctly identifies data files", {
  # Create a temporary directory with test files
  temp_dir <- tempdir()
  
  # Create test files of different supported types
  write.csv(data.frame(a = 1:3, b = 4:6), file.path(temp_dir, "test.csv"))
  openxlsx::write.xlsx(data.frame(x = 1:3, y = 4:6), file.path(temp_dir, "test.xlsx"))
  jsonlite::write_json(list(p = 1:3, q = 4:6), file.path(temp_dir, "test.json"))
  
  # Add an unsupported file type
  writeLines("Some text file", file.path(temp_dir, "test.txt"))
  
  # Run the function
  file_info <- scan_directory(temp_dir)
  
  # Test results
  expect_true(is.data.frame(file_info))
  expect_equal(nrow(file_info), 3) # Should find 3 supported files
  expect_true(all(c("csv", "xlsx", "json") %in% file_info$extension))
  expect_false("txt" %in% file_info$extension) # Should not detect unsupported types
  
  # Verify file_info structure
  expect_true(all(c("path", "filename", "extension", "size_bytes") %in% names(file_info)))
})

test_that("analyze_csv correctly analyzes CSV structure", {
  # Create a test CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    id = 1:5,
    name = c("A", "B", "C", "D", "E"),
    value = c(10.1, 20.2, NA, 40.4, 50.5),
    date = c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"),
    stringsAsFactors = FALSE
  )
  write.csv(test_data, temp_csv, row.names = FALSE)
  
  # Create file_info structure
  file_info <- data.frame(
    path = temp_csv,
    filename = basename(temp_csv),
    extension = "csv",
    size_bytes = file.size(temp_csv),
    stringsAsFactors = FALSE
  )
  
  # Run analysis
  result <- analyze_csv(file_info)
  
  # Verify results
  expect_equal(result$filename, basename(temp_csv))
  expect_equal(result$row_count, 5)
  expect_equal(result$column_count, 4)
  expect_true(all(c("id", "name", "value", "date") %in% result$columns))
  
  # Check column details
  expect_equal(result$column_details$id$inferred_type, "numeric")
  expect_equal(result$column_details$name$inferred_type, "text")
  expect_equal(result$column_details$value$null_count, 1)
  expect_equal(result$column_details$date$inferred_type, "potential_date")
  
  # Clean up
  file.remove(temp_csv)
})

test_that("analyze_excel correctly analyzes Excel structure", {
  # Skip test if readxl is not available
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx")
  
  # Create a test Excel file with multiple sheets
  temp_xlsx <- tempfile(fileext = ".xlsx")
  
  sheet1_data <- data.frame(
    id = 1:3,
    name = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  
  sheet2_data <- data.frame(
    code = c("X1", "Y2", "Z3"),
    value = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::addWorksheet(wb, "Sheet2")
  openxlsx::writeData(wb, "Sheet1", sheet1_data)
  openxlsx::writeData(wb, "Sheet2", sheet2_data)
  openxlsx::saveWorkbook(wb, temp_xlsx, overwrite = TRUE)
  
  # Create file_info structure
  file_info <- data.frame(
    path = temp_xlsx,
    filename = basename(temp_xlsx),
    extension = "xlsx",
    size_bytes = file.size(temp_xlsx),
    stringsAsFactors = FALSE
  )
  
  # Run analysis
  result <- analyze_excel(file_info)
  
  # Verify results
  expect_equal(result$filename, basename(temp_xlsx))
  expect