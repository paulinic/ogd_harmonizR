# Simple reproducible script to download datasets from opendata.swiss
# Install and load required packages
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
library(httr)
library(jsonlite)

download_opendata_datasets <- function(num_datasets = 10, save_path = "opendata_files", seed = 123) {
  # Create directories
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  
  cat("Starting download of", num_datasets, "datasets to", save_path, "\n")
  
  # Use fixed seed for reproducibility
  set.seed(seed)
  
  # Step 1: Get list of datasets
  cat("Getting dataset list...\n")
  response <- GET("https://opendata.swiss/api/3/action/package_list")
  
  if (status_code(response) != 200) {
    stop("API error: Status code ", status_code(response))
  }
  
  all_datasets <- fromJSON(rawToChar(response$content))$result
  cat("Found", length(all_datasets), "total datasets\n")
  
  # Reproducibly select datasets
  selected_datasets <- sample(all_datasets, min(num_datasets, length(all_datasets)))
  cat("Selected", length(selected_datasets), "datasets to download\n")
  
  # Download counter
  successful_downloads <- 0
  
  # Process each dataset
  for (i in seq_along(selected_datasets)) {
    dataset_id <- selected_datasets[i]
    cat("\nProcessing dataset", i, "of", length(selected_datasets), ":", dataset_id, "\n")
    
    # Get dataset metadata
    metadata_url <- paste0("https://opendata.swiss/api/3/action/package_show?id=", dataset_id)
    metadata_response <- GET(metadata_url)
    
    if (status_code(metadata_response) != 200) {
      cat("  Skipping dataset - HTTP error\n")
      next
    }
    
    dataset_info <- fromJSON(rawToChar(metadata_response$content))$result
    cat("  Dataset:", dataset_info$title, "\n")
    
    # Skip if no resources
    if (length(dataset_info$resources) == 0) {
      cat("  No resources available\n")
      next
    }
    
    # Find a downloadable resource (prefer CSV, JSON, XML)
    resource_to_download <- NULL
    preferred_formats <- c("csv", "json", "xml", "xlsx", "xls")
    
    # First try preferred formats
    for (format in preferred_formats) {
      for (resource in dataset_info$resources) {
        if (!is.null(resource$format) && 
            tolower(resource$format) == format && 
            !is.null(resource$url) && 
            resource$url != "") {
          resource_to_download <- resource
          cat("  Found resource in format:", format, "\n")
          break
        }
      }
      if (!is.null(resource_to_download)) break
    }
    
    # If no preferred format, take first available
    if (is.null(resource_to_download)) {
      for (resource in dataset_info$resources) {
        if (!is.null(resource$url) && resource$url != "") {
          resource_to_download <- resource
          cat("  Using first available resource\n")
          break
        }
      }
    }
    
    # Skip if no suitable resource found
    if (is.null(resource_to_download)) {
      cat("  No downloadable resources\n")
      next
    }
    
    # Prepare filename
    safe_title <- gsub("[^a-zA-Z0-9]", "_", dataset_info$title)
    if (nchar(safe_title) > 40) safe_title <- substr(safe_title, 1, 40)
    
    # Get file extension from format or URL
    file_ext <- "data"
    if (!is.null(resource_to_download$format) && resource_to_download$format != "") {
      file_ext <- tolower(gsub("[^a-zA-Z0-9]", "", resource_to_download$format))
    } else {
      # Try to get extension from URL
      url_parts <- strsplit(resource_to_download$url, "\\.")
      if (length(url_parts[[1]]) > 1) {
        file_ext <- tolower(tail(url_parts[[1]], 1))
      }
    }
    
    # Create output filename
    output_file <- file.path(save_path, paste0(safe_title, "_", substr(dataset_id, 1, 6), ".", file_ext))
    
    # Download file
    cat("  Downloading to:", basename(output_file), "\n")
    download_url <- URLencode(resource_to_download$url)
    
    download_success <- FALSE
    
    # Try method 1: httr
    tryCatch({
      download_response <- GET(download_url, 
                               write_disk(output_file, overwrite = TRUE),
                               timeout(60))
      
      if (status_code(download_response) == 200 && file.exists(output_file) && file.size(output_file) > 0) {
        download_success <- TRUE
        cat("  Download successful. File size:", file.size(output_file), "bytes\n")
      }
    }, error = function(e) {
      cat("  Error in first download method:", conditionMessage(e), "\n")
    })
    
    # Try method 2 if method 1 failed
    if (!download_success) {
      tryCatch({
        download.file(download_url, output_file, mode = "wb", quiet = TRUE)
        
        if (file.exists(output_file) && file.size(output_file) > 0) {
          download_success <- TRUE
          cat("  Download successful with alternate method\n")
        }
      }, error = function(e) {
        cat("  Error in second download method:", conditionMessage(e), "\n")
      })
    }
    
    # Update counter if successful
    if (download_success) {
      successful_downloads <- successful_downloads + 1
    } else {
      cat("  Download failed\n")
      if (file.exists(output_file)) file.remove(output_file)
    }
    
    # Be nice to the server
    Sys.sleep(1)
  }
  
  # Final summary
  cat("\n=== Download Summary ===\n")
  cat("Downloaded", successful_downloads, "of", length(selected_datasets), "datasets\n")
  cat("Files saved to:", normalizePath(save_path), "\n")
  
  return(successful_downloads)
}

# Usage examples:
# For reproducible results with the same 10 datasets every time:
# download_opendata_datasets(10, "opendata_files", 123)

# For a different set of 5 datasets:
# download_opendata_datasets(5, "different_files", 456)