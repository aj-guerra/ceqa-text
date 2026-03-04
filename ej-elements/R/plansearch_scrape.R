# plansearch_scrape.R
# ---------------------------------------------------------
# Stage 1: Download city plan PDFs from a URL list.
#
# Inputs:  pdf_links.csv       (one URL per row, no header; place in repo root)
# Outputs: downloaded_pdfs/    (gitignored; placed in repo root)
#
# Run directly:
#   source("R/plansearch_scrape.R")        # uses default paths
# Or call process_pdf_urls() with custom arguments.
# ---------------------------------------------------------

library(dplyr)
library(stringr)
library(tools)

# Extract city name and year from a URL of the form:
#   ...City-<CityName>_<YYYY>...
extract_info_from_url <- function(url) {
  city_match <- str_match(url, "City-([^_]+)_(\\d{4})")[c(2, 3)]

  if (!is.na(city_match[1]) && !is.na(city_match[2])) {
    city <- city_match[1]
    year <- city_match[2]

    city <- gsub("-", " ", city)
    city <- str_to_title(city)
    city <- gsub(" ", "_", city)

    filename <- paste0(city, "_", year)
    return(list(filename = filename, city = city, year = year))
  } else {
    return(NULL)
  }
}

# Return a unique filename by appending an incrementing counter if needed.
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

# Download a single PDF with error handling.
download_pdf <- function(url, filename) {
  tryCatch({
    temp_file <- tempfile()
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    file.copy(temp_file, filename)
    file.remove(temp_file)
    return(paste("Successfully downloaded:", filename))
  },
  error = function(e) {
    return(paste("Error downloading", filename, ":", e$message))
  },
  finally = {
    if (exists("temp_file") && file.exists(temp_file)) file.remove(temp_file)
  })
}

# Process a CSV of URLs and download all PDFs.
process_pdf_urls <- function(csv_file = "pdf_links.csv",
                              output_dir = "downloaded_pdfs") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  urls <- read.csv(csv_file, header = FALSE, stringsAsFactors = FALSE)
  colnames(urls) <- c("url")

  results <- character(nrow(urls))

  for (i in seq_len(nrow(urls))) {
    url  <- urls$url[i]
    info <- extract_info_from_url(url)

    if (!is.null(info)) {
      output_file <- file.path(output_dir, get_unique_filename(
        file.path(output_dir, info$filename)))
      result      <- download_pdf(url, output_file)
      results[i]  <- result
      cat(sprintf("[%d/%d] %s\n", i, nrow(urls), result))
    } else {
      results[i] <- paste("Failed to extract info from URL:", url)
      cat(sprintf("[%d/%d] Failed to extract info from URL: %s\n", i, nrow(urls), url))
    }

    gc()
  }

  return(results)
}

process_pdf_urls("pdf_links.csv", "downloaded_pdfs")
