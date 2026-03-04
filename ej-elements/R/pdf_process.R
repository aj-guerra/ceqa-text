# pdf_process.R
# ---------------------------------------------------------
# Stage 2: Process downloaded PDFs into quanteda corpus objects.
#
# Inputs:  downloaded_pdfs/*.pdf  (produced by plansearch_scrape.R)
# Outputs: data/plans_df.rds             (metadata dataframe)
#          data/plans_corpus_doc.rds     (per-document corpus)
#          data/plans_corpus_by_page.rds (per-page corpus)
#          data/temp.csv                 (OTHER-typed files for manual review)
#
# Run from the REPO ROOT:
#   source("R/pdf_process.R")
# ---------------------------------------------------------

library(dplyr)
library(stringr)
library(pdftools)
library(quanteda)

pdf_files <- list.files(path    = "downloaded_pdfs",
                        pattern = "\\.pdf$",
                        full.names = TRUE)

# ---------------------------------------------------------
# Helper: detect plan type from first-page text
# ---------------------------------------------------------

get_plan_type <- function(text) {
  if      (grepl("Environmental Justice Element", text, ignore.case = TRUE)) "EJE"
  else if (grepl("Housing Element",               text, ignore.case = TRUE)) "HE"
  else if (grepl("General Plan",                  text, ignore.case = TRUE)) "GP"
  else                                                                         "OTHER"
}

cleantext <- function(text) gsub("\\s+", " ", text)

# ---------------------------------------------------------
# Process each PDF
# ---------------------------------------------------------

plans_list          <- vector("list", length(pdf_files))
corpus_doc_list     <- vector("list", length(pdf_files))
corpus_page_list    <- vector("list", length(pdf_files))

for (i in seq_along(pdf_files)) {
  tryCatch({

    file      <- pdf_files[[i]]
    text      <- pdf_text(file)
    file_name <- basename(file)
    message("Processing [", i, "/", length(pdf_files), "]: ", file_name)

    # Parse city name and year from filename (pattern: City_Name_YYYY.pdf)
    city_name <- str_sub(
      str_replace_all(
        str_remove(file_name, str_split_i(file_name, "_", -1)),
        "_", " "),
      1, -2)

    plan_year <- as.numeric(
      tail(gsub("\\.pdf$", "", str_split(file_name, "_")[[1]]), 1))

    # Detect plan type from first two pages
    plan_type <- get_plan_type(gsub("\\s+", " ", text[1]))
    if (plan_type == "OTHER" && length(text) >= 2)
      plan_type <- get_plan_type(gsub("\\s+", " ", text[2]))

    plan_length <- length(text)

    # Metadata row
    plans_list[[i]] <- data.frame(
      plan_id     = i,
      file_name   = file_name,
      city_name   = city_name,
      plan_year   = plan_year,
      plan_type   = plan_type,
      plan_length = plan_length
    )

    # Clean text
    text_cleaned <- as.character(lapply(text, cleantext))

    # Per-page corpus
    corpus_page_list[[i]] <- corpus(
      text_cleaned,
      docnames = paste0(city_name, plan_year, "_", seq_along(text_cleaned)),
      docvars  = data.frame(
        file_name = file_name,
        city_name = city_name,
        plan_year = plan_year,
        plan_type = plan_type
      ))

    # Per-document corpus (all pages collapsed)
    corpus_doc_list[[i]] <- corpus(
      str_c(text_cleaned, collapse = ""),
      docnames = paste(city_name, plan_year),
      docvars  = data.frame(
        file_name   = file_name,
        city_name   = city_name,
        plan_year   = plan_year,
        plan_type   = plan_type,
        plan_length = plan_length
      ))

  }, error = function(e) {
    message("ERROR processing ", pdf_files[[i]], ": ", e$message)
  })
}

# Combine and save
plans_df            <- bind_rows(plans_list)
full_corpus_doc     <- do.call("+", Filter(Negate(is.null), corpus_doc_list))
full_corpus_by_page <- do.call("+", Filter(Negate(is.null), corpus_page_list))

saveRDS(plans_df,            file = "data/plans_df.rds")
saveRDS(full_corpus_doc,     file = "data/plans_corpus_doc.rds")
saveRDS(full_corpus_by_page, file = "data/plans_corpus_by_page.rds")

# ---------------------------------------------------------
# Generate data/temp.csv listing OTHER-typed files for manual review.
# Edit the generated file, fill in correct plan types, then incorporate
# corrections into data/plans_metadata.csv.
# ---------------------------------------------------------

others <- plans_df %>% filter(plan_type == "OTHER") %>% pull(file_name)

writeLines(
  c("# Fill in the correct plan_type for each file (EJE / HE / GP)",
    paste0(others, ",OTHER")),
  con = "data/temp.csv"
)

message("Saved: data/plans_df.rds, data/plans_corpus_doc.rds, ",
        "data/plans_corpus_by_page.rds")
if (length(others) > 0)
  message(length(others), " OTHER-typed files listed in data/temp.csv for review.")
