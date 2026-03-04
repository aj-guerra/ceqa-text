# run_all.R
# ---------------------------------------------------------
# Full pipeline for:
#   "The Effects of Consultant Use on Environmental Justice Planning"
#
# Pipeline stages:
#   1. SCRAPE    plansearch_scrape.R  -- download PDFs from URL list
#   2. PROCESS   pdf_process.R        -- build corpus objects from PDFs
#   3. ANALYSE   analysis.R           -- run models, generate figures/tables
#
# Stages 1 and 2 are skipped automatically when their outputs already exist.
# If you need to reprocess from scratch, delete the relevant output files
# and re-run this script.
#
# NOTE: Run from the Final_Project/ directory:
#   setwd("path/to/Final_Project"); source("run_all.R")
# ---------------------------------------------------------

cat("=== CEQA Environmental Justice Planning Analysis ===\n\n")

# ---- helpers ---------------------------------------------------------------

run_stage <- function(label, script, check_outputs = NULL, inputs_exist = TRUE) {
  cat(sprintf("--- Stage: %s ---\n", label))

  if (!inputs_exist) {
    cat("  SKIP: required inputs not found.\n\n")
    return(invisible(FALSE))
  }

  if (!is.null(check_outputs) && all(file.exists(check_outputs))) {
    cat("  SKIP: outputs already present.\n\n")
    return(invisible(FALSE))
  }

  cat(sprintf("  Running %s ...\n", script))
  tryCatch(
    source(script, local = new.env()),
    error = function(e) stop(sprintf("  ERROR in %s: %s", script, e$message))
  )
  cat(sprintf("  Done: %s\n\n", label))
  return(invisible(TRUE))
}

# ---- Stage 1: scrape -------------------------------------------------------
# Requires:  pdf_links.csv  (list of PDF URLs; supply manually)
# Outputs:   ../downloaded_pdfs/*.pdf

run_stage(
  label          = "Scrape PDFs",
  script         = "plansearch_scrape.R",
  check_outputs  = "../downloaded_pdfs",          # skip if directory exists
  inputs_exist   = file.exists("pdf_links.csv")
)

# ---- Stage 2: process PDFs into corpus objects ----------------------------
# Requires:  ../downloaded_pdfs/*.pdf
# Outputs:   plans_df.rds, plans_corpus_doc.rds, plans_corpus_by_page.rds

run_stage(
  label         = "Process PDFs",
  script        = "pdf_process.R",
  check_outputs = c("plans_df.rds", "plans_corpus_doc.rds", "plans_corpus_by_page.rds"),
  inputs_exist  = dir.exists("../downloaded_pdfs") &&
                    length(list.files("../downloaded_pdfs", pattern = "\\.pdf$")) > 0
)

# ---- Stage 3: analysis -----------------------------------------------------
# Requires:  plans_corpus_doc.rds, plans_metadata.csv
#            Census API key (set CENSUS_API_KEY env var or call
#            tidycensus::census_api_key() before running)
# Outputs:   figures and model tables printed to the R session

corpus_ready <- all(file.exists(c("plans_corpus_doc.rds", "plans_metadata.csv")))

run_stage(
  label        = "Run Analysis",
  script       = "analysis.R",
  inputs_exist = corpus_ready
)

if (!corpus_ready) {
  cat("NOTE: plans_corpus_doc.rds or plans_metadata.csv not found.\n")
  cat("      Run Stages 1 and 2, or place pre-built data files in Final_Project/.\n")
}

cat("=== Done ===\n")
