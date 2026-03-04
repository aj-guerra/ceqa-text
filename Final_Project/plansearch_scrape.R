# Load required libraries
library(dplyr)
library(stringr)
library(tools)

# Function to extract city and year from URL and create filename
extract_info_from_url <- function(url) {
  # Extract city name using regex
  # Pattern looks for "City-" followed by any characters until "_"
  city_match <- str_match(url, "City-([^_]+)_(\\d{4})")[c(2, 3)]
  
  if (!is.na(city_match[1]) && !is.na(city_match[2])) {
    city <- city_match[1]
    year <- city_match[2]
    
    # Format city name (capitalize first letter of each word, replace hyphens with spaces)
    city <- gsub("-", " ", city)
    city <- str_to_title(city)
    # Replace spaces with underscores for filename
    city <- gsub(" ", "_", city)
    
    # Create filename
    filename <- paste0(city, "_", year)
    return(list(filename = filename, city = city, year = year))
  } else {
    return(NULL)
  }
}

# Function to generate a unique filename if file already exists
get_unique_filename <- function(base_filename, extension = ".pdf") {
  if (!file.exists(paste0(base_filename, extension))) {
    return(paste0(base_filename, extension))
  }
  
  counter <- 1
  while (file.exists(paste0(base_filename, "_", counter, extension))) {
    counter <- counter + 1
  }
  
  return(paste0(base_filename, "_", counter, extension))
}

# Function to download PDF with proper error handling
download_pdf <- function(url, filename) {
  tryCatch({
    # Create a connection with a timeout
    temp_file <- tempfile()
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    
    # Move file to final destination
    file.copy(temp_file, filename)
    file.remove(temp_file)
    
    # Return success message
    return(paste("Successfully downloaded:", filename))
  }, 
  error = function(e) {
    return(paste("Error downloading", filename, ":", e$message))
  },
  finally = {
    # Clean up temp file if it exists
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
  })
}

# Main function to process the CSV and download PDFs
process_pdf_urls <- function(csv_file, output_dir = ".") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read CSV file (assuming it has one column with no header)
  urls <- read.csv(csv_file, header = FALSE, stringsAsFactors = FALSE)
  colnames(urls) <- c("url")
  
  # Process each URL
  results <- character(nrow(urls))
  
  for (i in 1:nrow(urls)) {
    url <- urls$url[i]
    
    # Extract info from URL
    info <- extract_info_from_url(url)
    
    if (!is.null(info)) {
      # Get unique filename
      output_file <- file.path(output_dir, get_unique_filename(info$filename))
      
      # Download the PDF
      result <- download_pdf(url, output_file)
      results[i] <- result
      
      # Print progress
      cat(sprintf("[%d/%d] %s\n", i, nrow(urls), result))
    } else {
      results[i] <- paste("Failed to extract info from URL:", url)
      cat(sprintf("[%d/%d] Failed to extract info from URL: %s\n", i, nrow(urls), url))
    }
    
    # Force garbage collection to free memory
    gc()
  }
  
  return(results)
}

# Example usage:
process_pdf_urls("pdf_links.csv", "downloaded_pdfs")