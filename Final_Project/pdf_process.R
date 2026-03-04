# pdf_process.R
# ---------------------------------------------------------
# Processes downloaded PDF files into quanteda corpus objects.
#
# Inputs:  ../downloaded_pdfs/*.pdf  (PDFs downloaded via plansearch_scrape.R)
# Outputs: plans_df.rds             (metadata dataframe)
#          plans_corpus_doc.rds     (per-document corpus)
#          plans_corpus_by_page.rds (per-page corpus)
#          temp.csv                 (list of OTHER-typed files for manual correction)
#
# NOTE: Run from the Final_Project/ directory (or adjust paths accordingly).
# ---------------------------------------------------------

library(dplyr)
library(stringr)
library(pdftools)
library(jsonlite)
library(quanteda)

pdf_files <- list.files(path = "../downloaded_pdfs",
                        pattern = "\\.pdf$",
                        full.names = TRUE)

# ---------------------------------------------------------
# Build metadata dataframe and corpus objects
# ---------------------------------------------------------

plans_df <- data.frame(
  plan_id    = numeric(),
  file_name  = character(),
  city_name  = character(),
  plan_year  = numeric(),
  plan_type  = character(),
  plan_length = numeric()
)

for (i in seq_along(pdf_files)) {

  tryCatch({

    file <- pdf_files[[i]]
    text <- pdf_text(file)

    # --- metadata ---
    file_name <- basename(file)
    print(paste("Processing file:", file_name))

    city_name <- str_sub(
      str_replace_all(
        str_remove(file_name, str_split_i(file_name, "_", -1)),
        "_", " "),
      1, -2)

    text_page1 <- gsub("\\s+", " ", text[1])
    text_page2 <- gsub("\\s+", " ", text[2])

    plan_year <- as.numeric(
      tail(gsub(".pdf", "", str_split(file_name, "_")[[1]]), 1))

    get_plan_type <- function(text) {
      if (grepl("Environmental Justice Element", text, ignore.case = TRUE)) {
        return("EJE")
      } else if (grepl("Housing Element", text, ignore.case = TRUE)) {
        return("HE")
      } else if (grepl("General Plan", text, ignore.case = TRUE)) {
        return("GP")
      } else {
        return("OTHER")
      }
    }

    plan_type <- get_plan_type(text_page1)
    if (plan_type == "OTHER") plan_type <- get_plan_type(text_page2)

    plan_length <- length(text)

    plan_df_temp <- data.frame(
      plan_id     = i,
      file_name   = file_name,
      city_name   = city_name,
      plan_year   = plan_year,
      plan_type   = plan_type,
      plan_length = plan_length
    )
    plans_df <- rbind(plans_df, plan_df_temp)

    # --- text cleaning and corpus construction ---
    cleantext <- function(text) {
      gsub("\\s+", " ", text)   # collapse extra whitespace
    }

    text_cleaned <- as.character(lapply(text, cleantext))

    # per-page corpus
    text_corpus_by_page <- corpus(
      text_cleaned,
      docnames = paste0(city_name, plan_year, "_", seq_along(text_cleaned)),
      docvars  = data.frame(
        file_name = file_name,
        city_name = city_name,
        plan_year = plan_year,
        plan_type = plan_type
      ))

    # per-document corpus (all pages collapsed)
    text_cleaned_unlist <- str_c(text_cleaned, collapse = '')
    text_corpus_doc <- corpus(
      text_cleaned_unlist,
      docnames = paste(city_name, plan_year),
      docvars  = data.frame(
        file_name   = file_name,
        city_name   = city_name,
        plan_year   = plan_year,
        plan_type   = plan_type,
        plan_length = plan_length
      ))

    # combine
    if (i == 1) {
      full_corpus_by_page <- text_corpus_by_page
      full_corpus_doc     <- text_corpus_doc
    } else {
      full_corpus_by_page <- full_corpus_by_page + text_corpus_by_page
      full_corpus_doc     <- full_corpus_doc + text_corpus_doc
    }

  }, error = function(e) {
    message("Error processing file: ", pdf_files[i], " : ", e$message)
  })
}

saveRDS(plans_df,           file = "plans_df.rds")
saveRDS(full_corpus_doc,    file = "plans_corpus_doc.rds")
saveRDS(full_corpus_by_page, file = "plans_corpus_by_page.rds")

# ---------------------------------------------------------
# Generate temp.csv for manually correcting OTHER-typed plans
# (fill in the correct plan_type for each row, then incorporate
#  corrections into plans_metadata.csv)
# ---------------------------------------------------------

others <- plans_df %>%
  filter(plan_type == 'OTHER') %>%
  pull(file_name)

empt <- data.frame()
write.csv(empt, file = "temp.csv", row.names = FALSE)

for (o in others) {
  txt <- paste0('file_name == "', o, '" ~ "GP"')
  write(txt, file = "temp.csv", append = TRUE)
}
