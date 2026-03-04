# The Effects of Consultant Use on Environmental Justice Planning

**Aaron Guerra**

Research paper examining whether third-party consultant use shapes the content
and textual similarity of Environmental Justice Elements (EJEs) in California
municipal general plans.

---

## Research Questions

1. Do pairs of plans that share a consultant exhibit higher textual similarity
   than those that do not?
2. Do pairs of plans where neither jurisdiction used a consultant exhibit lower
   textual similarity?

Similarity is measured as cosine similarity over a TF-IDF-weighted,
dictionary-based document-feature matrix. Models use beta regression
(response bounded in (0, 1)).

---

## Repository Structure

```
ej-elements/
├── R/
│   ├── run_all.R               # Pipeline driver — start here
│   ├── plansearch_scrape.R     # Stage 1: download PDFs from URL list
│   ├── pdf_process.R           # Stage 2: PDF → quanteda corpus objects
│   └── analysis.R              # Stage 3: DFM, models, figures, tables
│
├── data/
│   ├── plans_metadata.csv      # Manual annotations (consultant info, plan type)
│   ├── plans_df.rds            # Metadata dataframe (output of Stage 2)
│   ├── plans_corpus_doc.rds    # Per-document corpus [gitignored — rebuild or copy]
│   └── plans_corpus_by_page.rds# Per-page corpus    [gitignored — rebuild or copy]
│
├── output/
│   ├── pol_essay_final.qmd     # Paper (political science format)
│   ├── esp_essay_final.qmd     # Paper (environmental studies format)
│   ├── pol_presentation.qmd    # Slides (political science)
│   ├── esp_presentation.qmd    # Slides (environmental studies)
│   ├── references.bib          # Bibliography
│   └── custom-moon.scss        # Quarto presentation theme
│
├── _quarto.yml                 # Sets execute-dir: project (all paths from root)
├── .gitignore
└── ej-elements.Rproj
```

---

## Reproducing the Analysis

### Prerequisites

```r
install.packages(c(
  "tidyverse", "quanteda", "quanteda.textstats",
  "topicmodels", "tidytext", "tidycensus",
  "betareg", "stargazer", "broom", "gt", "pdftools"
))
```

A free Census Bureau API key is required for the ACS data pull in `analysis.R`:

```r
tidycensus::census_api_key("your_key_here", install = TRUE)
# restart R after install = TRUE, or:
Sys.setenv(CENSUS_API_KEY = "your_key_here")
```

Get a key at: <https://api.census.gov/data/key_signup.html>

### Quick start (pre-built corpus available)

If `data/plans_corpus_doc.rds` and `data/plans_metadata.csv` are present,
Stages 1 and 2 are skipped automatically:

```r
# From the repo root:
source("R/run_all.R")
```

### Full re-run from scratch

1. Place `pdf_links.csv` (one URL per row, no header) in the repo root.
2. `source("R/run_all.R")` — Stage 1 downloads PDFs to `downloaded_pdfs/`,
   Stage 2 builds corpus objects in `data/`, Stage 3 runs the analysis.

### Rendering papers and slides

From the repo root (requires Quarto CLI):

```bash
quarto render output/pol_essay_final.qmd
quarto render output/esp_essay_final.qmd
quarto render output/pol_presentation.qmd
quarto render output/esp_presentation.qmd
```

---

## Notes

- **All R script paths** are relative to the repo root (set your working
  directory to the repo root before sourcing).
- **All Quarto code-chunk paths** are also relative to the repo root, enforced
  by `execute-dir: project` in `_quarto.yml`.
- The large corpus objects (`plans_corpus_doc.rds`, ~220 MB uncompressed) are
  gitignored. Copy them in from a previous run or rebuild with `pdf_process.R`.
- Draft and module files from the original course repo (`models.qmd`,
  `pdf_process.qmd`, homework modules) are not included here; they remain in
  the `ceqa-text` course repository.
