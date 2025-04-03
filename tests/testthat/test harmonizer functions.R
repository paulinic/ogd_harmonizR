# Load required libraries
library(testthat)
source("scripts/05_R package scripts/harmonizeR-package.r")

# Test the harmonization phase functions
test_that("load_files correctly loads data files", {
  # Create a temporary directory with test files
  temp_dir <- tempdir()
  
  # Create test files
  df1 <- data.frame(
    ID = 1:3,
    Name = c("A", "B", "C"),
    Date = c("2023-01-01", "2023-01-02", "2023-01-03")
  )
  
  df2 <- data.frame(
    ID = 2:4,
    Value = c(10.5, 20.5, 30.5),
    Status = c("Active", "Inactive", "Active")
  )
  
  write.csv(df1, file.path(temp_dir, "data1.csv"), row.names = FALSE)
  write.csv(df2, file.path(temp_dir, "data2.csv"), row.names = FALSE)
  
  # Create a mock report
  report <- list(
    file_summaries = list(
      "data1.csv" = list(
        columns = c("ID", "Name", "Date"),
        column_details = list(
          ID = list(inferred_type = "numeric"),
          Name = list(inferred_type = "text"),
          Date = list(inferred_type = "potential_date")
        )
      ),
      "data2.csv" = list(
        columns = c("ID", "Value", "Status"),
        column_details = list(
          ID = list(inferred_type = "numeric"),
          Value = list(inferred_type = "numeric"),
          Status = list(inferred_type = "text")
        )
      )
    )
  )
  
  # Test loading files
  dataframes <- load_files(report, temp_dir)
  
  # Verify results
  expect_equal(length(dataframes), 2)
  expect_true(all(c("data1.csv", "data2.csv") %in% names(dataframes)))
  
  # Check structure of loaded dataframes
  expect_equal(ncol(dataframes[["data1.csv"]]), 3)
  expect_equal(nrow(dataframes[["data1.csv"]]), 3)
  expect_equal(ncol(dataframes[["data2.csv"]]), 3)
  expect_equal(nrow(dataframes[["data2.csv"]]), 3)
  
  # Check column names
  expect_true(all(c("ID", "Name", "Date") %in% names(dataframes[["data1.csv"]])))
  expect_true(all(c("ID", "Value", "Status") %in% names(dataframes[["data2.csv"]])))
})

test_that("convert_to_datetime handles various date formats", {
  # Test with ISO format dates
  df1 <- data.frame(
    date = c("2023-01-01", "2023-01-02", "2023-01-03", NA)
  )
  result1 <- convert_to_datetime(df1, "date")
  expect_true(result1$success)
  expect_true(inherits(result1$df$date, "POSIXct") || inherits(result1$df$date, "Date"))
  
  # Test with US format dates
  df2 <- data.frame(
    date = c("01/01/2023", "01/02/2023", "01/03/2023", NA)
  )
  result2 <- convert_to_datetime(df2, "date")
  expect_true(result2$success)
  
  # Test with European format dates
  df3 <- data.frame(
    date = c("01.01.2023", "02.01.2023", "03.01.2023", NA)
  )
  result3 <- convert_to_datetime(df3, "date")
  expect_true(result3$success)
  
  # Test with datetime format
  df4 <- data.frame(
    date = c("2023-01-01 12:34:56", "2023-01-02 10:20:30", NA)
  )
  result4 <- convert_to_datetime(df4, "date")
  expect_true(result4$success)
  
  # Test with mixed formats
  df5 <- data.frame(
    date = c("2023-01-01", "01/02/2023", "03.01.2023", NA)
  )
  result5 <- convert_to_datetime(df5, "date")
  expect_true(result5$success)
  
  # Test with non-date formats (should fail gracefully)
  df6 <- data.frame(
    date = c("not a date", "also not a date", "still not a date")
  )
  result6 <- convert_to_datetime(df6, "date")
  expect_false(result6$success)
  expect_equal(result6$df$date, df6$date) # Original values should be preserved
})

test_that("convert_to_numeric handles various numeric formats", {
  # Test with clean numeric strings
  df1 <- data.frame(
    value = c("123", "456.78", "-789", NA)
  )
  result1 <- convert_to_numeric(df1, "value")
  expect_true(result1$success)
  expect_equal(result1$df$value, c(123, 456.78, -789, NA))
  
  # Test with currency symbols
  df2 <- data.frame(
    value = c("$123.45", "€456.78", "£789.01", NA)
  )
  result2 <- convert_to_numeric(df2, "value")
  expect_true(result2$success)
  expect_equal(result2$df$value, c(123.45, 456.78, 789.01, NA))
  
  # Test with thousands separators
  df3 <- data.frame(
    value = c("1,234.56", "7,890", "12,345.67", NA)
  )
  result3 <- convert_to_numeric(df3, "value")
  expect_true(result3$success)
  
  # Test with numeric values embedded in text
  df4 <- data.frame(
    value = c("Price: 123.45", "Quantity: 67", "Amount: -89.01", NA)
  )
  result4 <- convert_to_numeric(df4, "value")
  expect_true(result4$success)
  expect_equal(result4$df$value, c(123.45, 67, -89.01, NA))
  
  # Test with non-numeric values (should fail gracefully)
  df5 <- data.frame(
    value = c("abc", "def", "ghi")
  )
  result5 <- convert_to_numeric(df5, "value")
  expect_false(result5$success)
  expect_equal(result5$df$value, df5$value) # Original values should be preserved
})

test_that("generate_harmonization_plan creates a valid plan", {
  # Create a mock inspection report
  report <- list(
    file_summaries = list(
      "data1.csv" = list(
        columns = c("CustomerID", "First Name", "Last Name", "DOB"),
        column_details = list(
          CustomerID = list(inferred_type = "numeric"),
          `First Name` = list(inferred_type = "text"),
          `Last Name` = list(inferred_type = "text"),
          DOB = list(inferred_type = "potential_date")
        )
      ),
      "data2.csv" = list(
        columns = c("CustomerID", "Email", "Phone", "Registration Date"),
        column_details = list(
          CustomerID = list(inferred_type = "numeric"),
          Email = list(inferred_type = "text"),
          Phone = list(inferred_type = "text"),
          `Registration Date` = list(inferred_type = "potential_date")
        )
      )
    ),
    common_fields = list(
      common_fields = list(
        CustomerID = c("data1.csv", "data2.csv")
      )
    )
  )
  
  # Generate harmonization plan
  plan <- generate_harmonization_plan(report)
  
  # Verify plan structure
  expect_true(is.list(plan))
  expect_true("fields" %in% names(plan))
  expect_true("standardize_names" %in% names(plan))
  expect_true(plan$standardize_names)
  
  # Check field details
  expect_true("CustomerID" %in% names(plan$fields))
  
  # Verify field properties
  customer_id_field <- plan$fields$CustomerID
  expect_equal(customer_id_field$field_name, "CustomerID")
  expect_equal(customer_id_field$standardized_name, "customer_id")
  expect_equal(customer_id_field$target_type, "numeric")
  expect_equal(customer_id_field$sources, c("data1.csv", "data2.csv"))
})

test_that("enhance_harmonization_plan adds key variable information", {
  # Create a mock harmonization plan
  harmonization_plan <- list(
    fields = list(
      CustomerID = list(
        field_name = "CustomerID",
        standardized_name = "customer_id",
        sources = c("data1.csv", "data2.csv"),
        target_type = "numeric"
      ),
      Name = list(
        field_name = "Name",
        standardized_name = "name",
        sources = c("data1.csv"),
        target_type = "text"
      )
    ),
    standardize_names = TRUE
  )
  
  # Create mock key variables
  key_variables <- list(
    CustomerID = list(
      uniqueness_ratio = list(
        "data1.csv" = 1.0,
        "data2.csv" = 1.0
      ),
      average_uniqueness = 1.0,
      overlap_scores = list(
        "data1.csv_vs_data2.csv" = 0.8
      ),
      average_overlap = 0.8,
      score = 0.9
    )
  )
  
  # Enhance plan
  enhanced_plan <- enhance_harmonization_plan(harmonization_plan, key_variables)
  
  # Verify enhanced plan
  expect_true(enhanced_plan$fields$CustomerID$is_key)
  expect_false(enhanced_plan$fields$Name$is_key)
  
  # Check key info
  expect_equal(enhanced_plan$fields$CustomerID$key_info$average_uniqueness, 1.0)
  expect_equal(enhanced_plan$fields$CustomerID$key_info$average_overlap, 0.8)
  expect_equal(enhanced_plan$fields$CustomerID$key_info$score, 0.9)
})

test_that("harmonize_dataframes standardizes columns and data types", {
  # Create a temporary output directory
  temp_out_dir <- file.path(tempdir(), "harmonized")
  if (dir.exists(temp_out_dir)) {
    unlink(temp_out_dir, recursive = TRUE)
  }
  dir.create(temp_out_dir)
  
  # Create test dataframes
  dataframes <- list(
    "data1.csv" = data.frame(
      CustomerID = 1:3,
      `First Name` = c("John", "Jane", "Bob"),
      DOB = c("2000-01-01", "1995-05-15", "1980-12-31"),
      stringsAsFactors = FALSE
    ),
    "data2.csv" = data.frame(
      CustomerID = 2:4,
      Email = c("jane@example.com", "bob@example.com", "alice@example.com"),
      Balance = c("$100.50", "$200.75", "$0.00"),
      stringsAsFactors = FALSE
    )
  )
  
  # Create harmonization plan
  harmonization_plan <- list(
    fields = list(
      CustomerID = list(
        field_name = "CustomerID",
        standardized_name = "customer_id",
        sources = c("data1.csv", "data2.csv"),
        target_type = "numeric",
        is_key = TRUE
      ),
      `First Name` = list(
        field_name = "First Name",
        standardized_name = "first_name",
        sources = c("data1.csv"),
        target_type = "text",
        is_key = FALSE
      ),
      DOB = list(
        field_name = "DOB",
        standardized_name = "dob",
        sources = c("data1.csv"),
        target_type = "datetime",
        is_key = FALSE
      ),
      Email = list(
        field_name = "Email",
        standardized_name = "email",
        sources = c("data2.csv"),
        target_type = "text",
        is_key = FALSE
      ),
      Balance = list(
        field_name = "Balance",
        standardized_name = "balance",
        sources = c("data2.csv"),
        target_type = "numeric",
        is_key = FALSE
      )
    ),
    standardize_names = TRUE
  )
  
  # Run harmonization
  harmonized <- harmonize_dataframes(dataframes, harmonization_plan, temp_out_dir)
  
  # Verify results
  expect_equal(length(harmonized), 2)
  expect_true(all(c("data1.csv", "data2.csv") %in% names(harmonized)))
  
  # Check standardized column names
  expect_true(all(c("customer_id", "first_name", "dob") %in% names(harmonized[["data1.csv"]])))
  expect_true(all(c("customer_id", "email", "balance") %in% names(harmonized[["data2.csv"]])))
  
  # Check data type conversions
  expect_true(is.numeric(harmonized[["data1.csv"]]$customer_id))
  expect_true(is.character(harmonized[["data1.csv"]]$first_name))
  expect_true(inherits(harmonized[["data1.csv"]]$dob, "POSIXct") || 
                inherits(harmonized[["data1.csv"]]$dob, "Date"))
  expect_true(is.numeric(harmonized[["data2.csv"]]$balance))
  
  # Check output files
  expect_true(file.exists(file.path(temp_out_dir, "harmonized_data1.csv")))
  expect_true(file.exists(file.path(temp_out_dir, "harmonized_data2.csv")))
  
  # Clean up
  unlink(temp_out_dir, recursive = TRUE)
})

test_that("identify_key_variables finds good linking candidates", {
  # Create test dataframes
  dataframes <- list(
    "data1.csv" = data.frame(
      id = 1:5,
      code = c("A1", "B2", "C3", "D4", "E5"),
      name = c("Item A", "Item B", "Item C", "Item D", "Item E"),
      stringsAsFactors = FALSE
    ),
    "data2.csv" = data.frame(
      id = c(2, 3, 4, 5, 6),
      code = c("B2", "C3", "D4", "E5", "F6"),
      price = c(10.5, 20.5, 30.5, 40.5, 50.5),
      stringsAsFactors = FALSE
    )
  )
  
  # Create a mock report
  report <- list(
    common_fields = list(
      common_fields = list(
        id = c("data1.csv", "data2.csv"),
        code = c("data1.csv", "data2.csv")
      )
    )
  )
  
  # Identify key variables
  key_vars <- identify_key_variables(dataframes, report)
  
  # Verify results
  expect_true(length(key_vars) > 0)
  expect_true(all(c("id", "code") %in% names(key_vars)))
  
  # Check key variable properties
  id_key <- key_vars$id
  expect_true(id_key$average_uniqueness > 0.9)  # All IDs should be unique
  expect_true("average_overlap" %in% names(id_key))
  expect_true("score" %in% names(id_key))
  
  code_key <- key_vars$code
  expect_true(code_key$average_uniqueness > 0.9)  # All codes should be unique
  expect_true("average_overlap" %in% names(code_key))
  expect_true("score" %in% names(code_key))
})

test_that("link_datasets merges dataframes using key variables", {
  # Create a temporary output directory
  temp_out_dir <- file.path(tempdir(), "linked")
  if (dir.exists(temp_out_dir)) {
    unlink(temp_out_dir, recursive = TRUE)
  }
  dir.create(temp_out_dir)
  
  # Create harmonized dataframes
  harmonized_dfs <- list(
    "data1.csv" = data.frame(
      customer_id = 1:5,
      name = c("Customer A", "Customer B", "Customer C", "Customer D", "Customer E"),
      stringsAsFactors = FALSE
    ),
    "data2.csv" = data.frame(
      customer_id = c(2, 3, 4, 5, 6),
      email = c("b@example.com", "c@example.com", "d@example.com", "e@example.com", "f@example.com"),
      stringsAsFactors = FALSE
    )
  )
  
  # Create key variables
  key_variables <- list(
    customer_id = list(
      uniqueness_ratio = list(
        "data1.csv" = 1.0,
        "data2.csv" = 1.0
      ),
      average_uniqueness = 1.0,
      overlap_scores = list(
        "data1.csv_vs_data2.csv" = 0.8
      ),
      average_overlap = 0.8,
      score = 0.9
    )
  )
  
  # Link datasets
  result <- link_datasets(harmonized_dfs, key_variables, temp_out_dir)
  
  # Verify results
  expect_true(!is.null(result$linked_data))
  expect_equal(result$link_report$status, "success")
  expect_equal(result$link_report$key_variable, "customer_id")
  
  # Check linked data structure
  linked_data <- result$linked_data
  expect_true(all(c("customer_id", "name", "email") %in% names(linked_data)))
  expect_true(nrow(linked_data) >= 6)  # Should include all unique IDs
  
  # Check output file
  expect_true(file.exists(file.path(temp_out_dir, "linked_data.csv")))
  
  # Check linking statistics
  expect_true(!is.null(result$link_report$statistics))
  
  # Clean up
  unlink(temp_out_dir, recursive = TRUE)
})

test_that("process_data executes complete workflow", {
  # Create a temporary directory structure
  temp_dir <- tempdir()
  input_dir <- file.path(temp_dir, "input")
  output_dir <- file.path(temp_dir, "output")
  
  # Create directories if they don't exist
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create test files
  df1 <- data.frame(
    CustomerID = 1:5,
    Name = c("A", "B", "C", "D", "E"),
    BirthDate = c("2000-01-01", "1995-05-15", "1980-12-31", "1975-06-20", "1990-03-10"),
    stringsAsFactors = FALSE
  )
  
  df2 <- data.frame(
    CustomerID = 3:7,
    Email = c("c@example.com", "d@example.com", "e@example.com", "f@example.com", "g@example.com"),
    Balance = c("$100", "$200", "$300", "$400", "$500"),
    stringsAsFactors = FALSE
  )
  
  write.csv(df1, file.path(input_dir, "customers.csv"), row.names = FALSE)
  write.csv(df2, file.path(input_dir, "accounts.csv"), row.names = FALSE)
  
  # Create a mock inspection report
  report <- list(
    files_analyzed = 2,
    file_summaries = list(
      "customers.csv" = list(
        columns = c("CustomerID", "Name", "BirthDate"),
        column_details = list(
          CustomerID = list(inferred_type = "numeric"),
          Name = list(inferred_type = "text"),
          BirthDate = list(inferred_type = "potential_date")
        )
      ),
      "accounts.csv" = list(
        columns = c("CustomerID", "Email", "Balance"),
        column_details = list(
          CustomerID = list(inferred_type = "numeric"),
          Email = list(inferred_type = "text"),
          Balance = list(inferred_type = "numeric")
        )
      )
    ),
    common_fields = list(
      common_fields = list(
        CustomerID = c("customers.csv", "accounts.csv")
      ),
      common_field_count = 1
    )
  )
  
  # Run the complete workflow
  result <- process_data(
    report = report,
    directory_path = input_dir,
    output_dir = output_dir
  )
  
  # Verify results
  expect_true(!is.null(result))
  expect_true("harmonized_data" %in% names(result))
  expect_true("key_variables" %in% names(result))
  expect_true("harmonization_plan" %in% names(result))
  expect_true("linked_data" %in% names(result))
  
  # Check harmonized data
  expect_equal(length(result$harmonized_data), 2)
  expect_true(all(c("customers.csv", "accounts.csv") %in% names(result$harmonized_data)))
  
  # Check key variables
  expect_true("CustomerID" %in% names(result$key_variables))
  
  # Check harmonization plan
  expect_true("CustomerID" %in% names(result$harmonization_plan$fields))
  
  # Check linked data
  expect_true(!is.null(result$linked_data))
  expect_true(all(c("customer_id", "name", "birth_date", "email", "balance") %in% 
                    names(result$linked_data)))
  
  # Check output files
  expect_true(file.exists(file.path(output_dir, "harmonized_customers.csv")))
  expect_true(file.exists(file.path(output_dir, "harmonized_accounts.csv")))
  expect_true(file.exists(file.path(output_dir, "linked_data.csv")))
  
  # Clean up
  unlink(c(input_dir, output_dir), recursive = TRUE)
})

# Run all tests
test_file("path/to/your/test_harmonization.R")