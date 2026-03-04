# run_all.R
# ---------------------------------------------------------
# Full pipeline for:
#   "The Effects of Consultant Use on Environmental Justice Planning"
#
# Pipeline stages:
#   1. SCRAPE    R/plansearch_scrape.R  -- download PDFs from URL list
#   2. PROCESS   R/pdf_process.R        -- build corpus objects from PDFs
#   3. ANALYSE   R/analysis.R           -- run models, generate figures/tables
#
# Stages 1 and 2 are skipped automatically when their outputs already exist.
# Delete the relevant output files and re-run to force a rebuild.
#
# Run from the REPO ROOT:
#   setwd("path/to/ej-elements"); source("R/run_all.R")
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
# Requires:  pdf_links.csv  (one URL per row, no header; place in repo root)
# Outputs:   downloaded_pdfs/*.pdf  (gitignored)

run_stage(
  label         = "Scrape PDFs",
  script        = "R/plansearch_scrape.R",
  check_outputs = "downloaded_pdfs",
  inputs_exist  = file.exists("pdf_links.csv")
)

# ---- Stage 2: process PDFs into corpus objects ----------------------------
# Requires:  downloaded_pdfs/*.pdf
# Outputs:   data/plans_df.rds
#            data/plans_corpus_doc.rds
#            data/plans_corpus_by_page.rds

run_stage(
  label         = "Process PDFs",
  script        = "R/pdf_process.R",
  check_outputs = c("data/plans_df.rds",
                    "data/plans_corpus_doc.rds",
                    "data/plans_corpus_by_page.rds"),
  inputs_exist  = dir.exists("downloaded_pdfs") &&
                    length(list.files("downloaded_pdfs", pattern = "\\.pdf$")) > 0
)

# ---- Stage 3: analysis -----------------------------------------------------
# Requires:  data/plans_corpus_doc.rds, data/plans_metadata.csv
#            Census API key (set CENSUS_API_KEY env var or call
#            tidycensus::census_api_key() before running)
# Outputs:   figures and model tables printed to the R session

corpus_ready <- all(file.exists(c("data/plans_corpus_doc.rds",
                                  "data/plans_metadata.csv")))

if (!corpus_ready) {
  cat("NOTE: data/plans_corpus_doc.rds or data/plans_metadata.csv not found.\n")
  cat("      Run Stages 1 and 2, or copy pre-built corpus files into data/.\n\n")
}

run_stage(
  label        = "Run Analysis",
  script       = "R/analysis.R",
  inputs_exist = corpus_ready
)

cat("=== Done ===\n")
