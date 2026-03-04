# The Effects of Consultant Use on Environmental Justice Planning

**Aaron Guerra**

Research paper examining whether third-party consultant use shapes the content
and similarity of Environmental Justice Elements (EJEs) in California municipal
general plans.

---

## Research Questions

1. Do pairs of plans that share a consultant exhibit higher textual similarity
   than those that do not?
2. Do pairs of plans where neither jurisdiction used a consultant exhibit lower
   textual similarity?

Similarity is measured as cosine similarity over a TF-IDF-weighted
dictionary-based document-feature matrix. Models use beta regression
(response bounded in (0,1)).

---

## Data Sources

| Source | Description |
|---|---|
| City general plans (PDFs) | Downloaded via `plansearch_scrape.R` from a manually compiled URL list |
| `plans_metadata.csv` | Manually annotated: plan type, consultant name(s), binary consultant indicator |
| ACS 5-year estimates | Retrieved via `tidycensus`; population and MHI by California place |

**Saved corpus objects** (`plans_corpus_doc.rds`, `plans_corpus_by_page.rds`,
`plans_df.rds`) are pre-built and checked into the repo so Stages 1–2 can be
skipped for analysis reproduction.

---

## File Overview

```
Final_Project/
├── run_all.R               # Pipeline driver — run this to reproduce everything
├── plansearch_scrape.R     # Stage 1: download PDFs from URL list
├── pdf_process.R           # Stage 2: convert PDFs → quanteda corpus objects
├── analysis.R              # Stage 3: EJ dictionary DFM, models, figures
│
├── plans_metadata.csv      # Manual annotations (consultant info, plan type)
├── plans_df.rds            # Metadata dataframe (output of Stage 2)
├── plans_corpus_doc.rds    # Per-document corpus (output of Stage 2)
├── plans_corpus_by_page.rds# Per-page corpus (output of Stage 2)
│
├── pol_essay_final.qmd     # Paper (political science format)
├── esp_essay_final.qmd     # Paper (environmental studies format)
├── pol_presentation.qmd    # Presentation slides (political science)
├── esp_presentation.qmd    # Presentation slides (environmental studies)
├── essay_draft.qmd         # Working draft
├── models.qmd              # Early analysis draft (superseded by analysis.R)
├── pdf_process.qmd         # Early processing draft (superseded by pdf_process.R)
│
├── references.bib          # Bibliography
└── custom-moon.scss        # Quarto presentation theme
```

---

## Reproducing the Analysis

### Quick start (pre-built corpus available)

```r
setwd("path/to/Final_Project")
source("run_all.R")
```

Stages 1 and 2 are skipped automatically when `plans_corpus_doc.rds` and
`plans_metadata.csv` are present. Only the Census API call in `analysis.R`
requires external access.

### Census API key

The ACS data pull in `analysis.R` requires a free Census Bureau API key:

```r
tidycensus::census_api_key("your_key_here", install = TRUE)
# then restart R, or:
Sys.setenv(CENSUS_API_KEY = "your_key_here")
```

Get a key at: https://api.census.gov/data/key_signup.html

### Full re-run from scratch

1. Provide `pdf_links.csv` (one URL per row, no header) in `Final_Project/`.
2. Run `source("run_all.R")` — Stage 1 downloads PDFs to `../downloaded_pdfs/`,
   Stage 2 rebuilds corpus objects, Stage 3 runs the analysis.

---

## Key Packages

```r
install.packages(c(
  "tidyverse", "quanteda", "quanteda.textstats",
  "topicmodels", "tidytext", "tidycensus",
  "betareg", "stargazer", "broom", "gt",
  "pdftools"
))
```
