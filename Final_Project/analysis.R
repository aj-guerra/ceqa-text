# analysis.R
# ---------------------------------------------------------
# Main analysis for:
#   "The Effects of Consultant Use on Environmental Justice Planning"
#
# Inputs:  plans_corpus_doc.rds   (built by pdf_process.R)
#          plans_metadata.csv     (manually annotated metadata)
# Requires: Census API key set via Sys.setenv(CENSUS_API_KEY = "your_key_here")
#           or tidycensus::census_api_key("your_key_here")
#
# NOTE: Run from the Final_Project/ directory (or adjust paths accordingly).
# ---------------------------------------------------------

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(topicmodels)
library(tidytext)
library(tidycensus)
library(stargazer)
library(betareg)
library(broom)
library(gt)

# =============================================================
# 1. EJ DICTIONARY
# =============================================================

ej_dictionary <- dictionary(list(
  identifying_disadvantaged_communities = c(
    "disadvantaged communit*", "disproprtionat* affect*",
    "pollution* burden*", "pollution*",
    "hazard*", "health effect*",
    "exposure*", "environmental degradation*",
    "CalEnviroScreen", "pollution burden*", "census tract*", "screen*", "community-specific data",
    "vulnera*",
    "household income*", "median income", "MHI", "income limit*",
    "health risk factor*", "environmental burden*",
    "health risk*", "community needs",
    "historical trend*",
    "fringe communit*", "unincorporated communit*",
    "equit*", "inequit*",
    "low-income area*", "low-income population*", "sensitive population*",
    "minority population*",
    "racial minorit*", "communit* of color",
    "tribal government*", "tribal culture*",
    "environmental racism", "redlining", "disinvestment*", "segregat*", "systemic oppression"
  ),
  reducing_pollution_exposure_air_quality = c(
    "pollut*", "pollution exposure*", "reduce exposure*", "reduc* pollut*",
    "unique health risk*", "compounded health risk*", "health hazard*",
    "air qualit*",
    "mobile source*", "stationary source*",
    "emission*",
    "fossil fuel combustion",
    "vehicle miles traveled", "VMT", "mode shift", "active transportation", "zero-emission", "electric fleet*",
    "sensitive land use*",
    "high-volume roadway*", "major arterial*", "freeway*", "truck route*",
    "freight-handling facilit*", "manufacturing facilit*", "industrial air pollution*",
    "mitigation procedure*", "mitigation*", "low-carbon technolog*",
    "zero-emission vehicle*", "ZEV",
    "solar generation", "wind generation",
    "indoor filtration system*", "polluting substance*", "water qualit*", "soil pollution*",
    "noise",
    "asthma", "birth defect*", "cancer*", "heart disease*", "neurologic disorder*", "reproductive disorder*",
    "involuntary exposure*", "second-hand smoke", "third-hand smoke", "tobacco smoke", "smoke-free zone*", "tobacco outlet*",
    "pesticide*", "integrated pest management", "regenerative agriculture",
    "air quality management district*", "aqmd", "carb", "california air resources board",
    "community air protection program*", "ab 617", "community emissions reduction program*",
    "air quality monitoring", "emissions control limit*", "emissions data",
    "toxic hot spot*",
    "transitional land use*", "industrial facilit*",
    "hazardous facilit*", "hazardous waste", "solid waste site*", "brownfield development",
    "remediation", "cleanup",
    "near-roadway siting",
    "diesel PM", "toxic air contaminant*", "ozone", "pm 2.5",
    "groundwater threat*", "water contaminant",
    "impaired water bod*", "toxic cleanup site*",
    "traffic densit*"
  ),
  promoting_public_facilities = c(
    "public facilit*", "public improvement*", "public service*",
    "community center*", "librar*", "public transit", "recreation facilit*",
    "safe drinking water", "wastewater service*",
    "active transportation infrastructure", "flood control", "water drainage",
    "health care service*", "hospital*", "health clinic*",
    "broadband access", "internet access",
    "disaster preparedness", "recovery capacit*",
    "amenit*", "park", "parks", "trail*", "sidewalk*", "public transit"
  ),
  promoting_food_access = c(
    "food access", "healthy food*", "affordable food*", "food desert*",
    "food insecur*", "nutrition", "obesit*",
    "food choice*", "food system*",
    "grocery store*", "farmer* market*",
    "community garden*", "urban farm*", "convenience store*",
    "fresh produce", "food procurement polic*",
    "food production", "food distribution", "food processing", "food consumption",
    "waste disposal",
    "locally grown food*", "farm to market",
    "CalFresh",
    "food recovery program*", "SB 1383",
    "fresh fruit*", "fresh vegetable*",
    "self-reported food insecurit*"
  ),
  promoting_safe_sanitary_homes = c(
    "safe home*", "sanitary home*", "promot* safe and sanitary home*",
    "housing location*", "housing qualit*", "housing affordabilit*", "housing stabilit*",
    "lower income household*", "special needs household*",
    "Regional Housing Need Allocation", "RHNA",
    "fair share planning",
    "accessible transit",
    "pest infestation", "water intrusion", "mold", "poor insulation",
    "exposure to toxin*", "lead exposure", "lead based paint",
    "second-hand smoke", "third-hand smoke",
    "weatherize home*", "modernize home*",
    "green building practice*", "sustainable building practice*",
    "new housing construction", "major retrofit*",
    "affordable housing", "unstable living condition*", "housing price*",
    "household occupancy rate*", "overcrowded living condition*",
    "rising rent*", "displacement*", "marginally housed", "homelessness",
    "renter protection",
    "preserving affordable housing", "housing cost burden", "homelessness data",
    "household characteristic*"
  ),
  promoting_physical_activity = c(
    "physical activit*", "physical inactivit*",
    "chronic disease*",
    "obesit*", "diabetes", "high blood pressure", "high cholesterol", "heart disease*",
    "mental health", "well-being",
    "park", "parks", "recreation", "open space", "recreational facilit*",
    "park poor", "park improvement*",
    "no smoking polic*",
    "joint use agreement*", "shared use agreement*",
    "active transportation",
    "walking", "biking", "daily routine*", "perform errand*",
    "walking to work", "biking to work", "walking to school", "biking to school",
    "active design guideline*",
    "connected bike route*", "pedestrian on-street route*",
    "infill development", "Complete Street*",
    "multimodal transit", "interconnected transit",
    "first mile polic*", "last mile polic*",
    "covered rest area*", "shade", "age friendly seating", "bike storage", "trail network*",
    "Sustainable Communities and Climate Protection Act", "SB 375",
    "Active Transportation Program", "Safe Routes to School", "SRTS",
    "safety of route*", "aging in place",
    "naturally occurring retirement communit*", "NORC",
    "walkable communit*",
    "park access", "unintentional injur* involving pedestrian*",
    "crash data", "walk trip* per capita", "bike trip* per capita",
    "children who walk* to school", "children who bike* to school", "children who roll* to school",
    "walk map*", "bike map*",
    "perception of safet*"
  ),
  reducing_unique_compounded_health_risks = c(
    "unique health risk*", "compounded health risk*",
    "reduce health risk*",
    "climate change", "climate vulnerabilit*", "climate adaptation",
    "adaptation", "resilience", "resiliency",
    "flooding", "drought", "wildfire*", "extreme heat",
    "greenhouse gas emission*", "GHG", "vulnerable communit*",
    "temperature record*", "heat stroke", "heat-related complication*", "heat island*", "cooling center*",
    "green infrastructure", "urban forestry", "urban greening", "cool surface*", "cool roof*",
    "green roof*", "tree canop*",
    "carbon capture",
    "energy efficienc*",
    "drought mitigation", "stagnant water", "mosquito reproduction", "insect-borne disease*",
    "dengue", "yellow fever",
    "sea level rise",
    "fire hazard severity zone*", "FHSZ",
    "precipitation change*", "snowpack loss", "extreme precipitation event*",
    "extended drought scenario*", "wildfire threat*", "air conditioning access"
  ),
  promoting_civic_engagement = c(
    "civic engagement",
    "public decision-making process*", "public participation",
    "community engagement", "community vision*",
    "local histor*", "relationship*", "trust", "rebuild trust",
    "community-based organization*", "CBO*",
    "advocacy group*",
    "trusted leader*",
    "barrier* to participation", "time conflict*",
    "meeting format*", "planning process*", "ongoing engagement",
    "general plan implementation",
    "local neighborhood-level specific plan*",
    "revitalization effort*",
    "community-facing program*",
    "community input", "community buy-in", "community support",
    "stakeholder communit*", "community advisory committee"
  )
))

# =============================================================
# 2. LOAD DATA
# =============================================================

plan_corpus <- readRDS("plans_corpus_doc.rds")

plan_metadata <- read_csv('plans_metadata.csv') %>%
  mutate(consultant_list = str_split(consultant_name, pattern = ";\\s*"))

docvars(plan_corpus) <- plan_metadata

plan_corpus <- corpus_subset(plan_corpus, (plan_type == "EJE" | plan_type == "GP" | plan_type == "HE"))

# =============================================================
# 3. CENSUS DATA (requires API key)
# =============================================================
# Uncomment and set key if needed:
# tidycensus::census_api_key("your_key_here")

ca_data <- get_acs(
  geography = 'place',
  state     = 'California',
  variables = c('B01003_001',  # population
                'B19013_001'   # median household income
  )) %>%
  filter(!str_detect(NAME, "Burbank CDP|El Cerrito CDP|Greenfield CDP|Live Oak CDP|Mountain View CDP|Rolling Hills CDP"))

census_data <- ca_data %>%
  mutate(city_name = str_remove(NAME, " (city|CDP), California")) %>%
  select(-c(NAME, moe, GEOID)) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(population = B01003_001,
         mhi        = B19013_001)

# =============================================================
# 4. BUILD DOCUMENT VARIABLE DATAFRAME
# =============================================================

docvar_df <- docvars(plan_corpus) %>%
  cbind(docnames(plan_corpus)) %>%
  select(`docnames(plan_corpus)`, everything()) %>%
  rename(doc_id = `docnames(plan_corpus)`) %>%
  left_join(census_data, by = "city_name") %>%
  mutate(
    pop_bin = as.factor(cut(population,
                            breaks = quantile(population, probs = seq(0, 1, length.out = 5), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = FALSE)),
    mhi_bin = as.factor(cut(mhi,
                            breaks = quantile(mhi, probs = seq(0, 1, length.out = 5), na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = FALSE)),
    single_consultant = str_split_i(consultant_name, ';', 1),
    n_consultants = ifelse(is.na(consultant_name),
                           0,
                           str_count(consultant_name, ";") + 1)
  ) %>%
  tibble()

# =============================================================
# 5. TOKENIZE AND BUILD DFM
# =============================================================

plan_tokens <- tokens(plan_corpus) %>%
  tokens_tolower() %>%
  tokens(split_hyphens   = TRUE,
         remove_punct    = TRUE,
         remove_symbols  = TRUE,
         remove_numbers  = TRUE)

ej_tokens <- plan_tokens %>%
  tokens_compound(ej_dictionary,
                  valuetype       = "glob",
                  case_insensitive = TRUE,
                  join            = FALSE) %>%
  tokens_keep(ej_dictionary,
              valuetype = 'glob')

ej_dfm <- ej_tokens %>%
  dfm()

docvars(ej_dfm) <- docvar_df

ej_dfm_tfidf <- ej_dfm %>%
  dfm_tfidf()

# =============================================================
# 6. PAIRWISE COSINE SIMILARITY DATAFRAME
# =============================================================

ej_sim_df <- tibble(as.data.frame(textstat_simil(ej_dfm_tfidf, method = 'cosine'))) %>%
  left_join(docvar_df, by = c("document1" = "doc_id")) %>%
  left_join(docvar_df, by = c("document2" = "doc_id")) %>%
  mutate(
    same_year   = as.factor(ifelse(plan_year.x == plan_year.y, 1, 0)),
    same_pop    = as.factor(ifelse(pop_bin.x == pop_bin.y, 1, 0)),
    same_mhi    = as.factor(ifelse(mhi_bin.x == mhi_bin.y, 1, 0)),
    both_used_consultant = as.factor(ifelse(consultant_binary.x == 1 & consultant_binary.y == 1, 1, 0)),
    any_shared_consultant = as.factor(
      mapply(function(a, b) {
        if (is.null(a) || is.null(b) || (all(is.na(a)) & all(is.na(b)))) {
          0
        } else {
          as.integer(length(intersect(a, b)) > 0)
        }
      }, consultant_list.x, consultant_list.y)),
    n_shared_consultants = mapply(function(a, b) {
      if (
        (is.null(a) || all(is.na(a)) || identical(a, "NA")) &&
        (is.null(b) || all(is.na(b)) || identical(b, "NA"))
      ) {
        0
      } else if (is.null(a) || all(is.na(a)) || identical(a, "NA") ||
                 is.null(b) || all(is.na(b)) || identical(b, "NA")) {
        0
      } else {
        length(intersect(a, b))
      }
    }, consultant_list.x, consultant_list.y),
    both_no_consultant = as.factor(ifelse(consultant_binary.x == 0 & consultant_binary.y == 0, 1, 0)),
    same_plan_type = as.factor(ifelse(plan_type.x == plan_type.y, 1, 0)),
    both_eje = as.factor(ifelse(plan_type.x == "EJE" & plan_type.y == "EJE", 1, 0)),
    both_gp  = as.factor(ifelse(plan_type.x == "GP"  & plan_type.y == "GP",  1, 0)),
    both_he  = as.factor(ifelse(plan_type.x == "HE"  & plan_type.y == "HE",  1, 0)),
    length_dif = abs(plan_length.x - plan_length.y)
  )

# =============================================================
# 7. REGRESSION MODELS (Beta Regression)
# =============================================================

# H1: shared consultant -> higher similarity
mod_1a <- betareg(cosine ~ any_shared_consultant + same_year + same_pop + same_mhi + both_eje + both_gp + both_he,
                  link = 'logit',
                  data = ej_sim_df)

mod_1a_int <- mod_1a %>% tidy() %>%
  filter(component == "mean", term == "(Intercept)") %>% pull(estimate)
mod_1a_eff <- mod_1a %>% tidy() %>%
  filter(component == "mean", term == "any_shared_consultant1") %>% pull(estimate)
# Marginal effect: plogis(mod_1a_int + mod_1a_eff) - plogis(mod_1a_int)

# H1 interaction with plan type
mod_2a <- betareg(cosine ~ same_year + same_pop + same_mhi + both_eje + any_shared_consultant*both_gp + any_shared_consultant*both_he,
                  link = 'logit',
                  data = ej_sim_df)

stargazer(mod_1a, mod_2a,
          type  = 'text',
          title = "Similarity for Shared Consultant Use",
          dep.var.labels = 'Cosine Similarity',
          covariate.labels = c(
            'Same Year', 'Same Population Bin', 'Same MHI Bin',
            'Shared Consultant', 'Both EJE', 'Both GP', 'Both HE',
            'Shared Consultant * Both GP', 'Shared Consultant * Both HE'
          ),
          order = c('year', 'pop', 'mhi'))

# H2: both no consultant -> lower similarity
mod_1b <- betareg(cosine ~ both_no_consultant + same_year + same_pop + same_mhi + both_eje + both_gp + both_he,
                  link = 'logit',
                  data = ej_sim_df)

mod_1b_int <- mod_1b %>% tidy() %>%
  filter(component == "mean", term == "(Intercept)") %>% pull(estimate)
mod_1b_eff <- mod_1b %>% tidy() %>%
  filter(component == "mean", term == "both_no_consultant1") %>% pull(estimate)
# Marginal effect: plogis(mod_1b_int + mod_1b_eff) - plogis(mod_1b_int)

# H2 interaction with plan type
mod_2b <- betareg(cosine ~ same_year + same_pop + same_mhi + both_no_consultant*both_eje + both_no_consultant*both_gp + both_no_consultant*both_he,
                  link = 'logit',
                  data = ej_sim_df)

stargazer(mod_1b, mod_2b,
          type  = 'text',
          title = "Similarity for No Consultant Use",
          dep.var.labels = 'Cosine Similarity',
          covariate.labels = c(
            'Same Year', 'Same Population Bin', 'Same MHI Bin',
            'Both No Consultant', 'Both EJE', 'Both GP', 'Both HE',
            'Both No Consultant * Both EJE',
            'Both No Consultant * Both GP',
            'Both No Consultant * Both HE'
          ),
          order = c('year', 'pop', 'mhi'))

# =============================================================
# 8. TERM FREQUENCY FIGURES
# =============================================================

# Fig 1: Raw EJ term frequency (top 30)
freq_count <- ej_dfm %>%
  textstat_frequency() %>%
  slice_max(order_by = frequency, n = 30) %>%
  arrange(desc(frequency))

freq_tfidf <- ej_dfm_tfidf %>%
  textstat_frequency(force = TRUE) %>%
  slice_max(order_by = frequency, n = 30) %>%
  arrange(desc(frequency))

all_terms <- data.frame(term = c(freq_count$feature, freq_tfidf$feature))
appear_in_both <- count(all_terms, term) %>% filter(n > 1) %>% pull(term)

freq_count <- freq_count %>%
  mutate(appear_in_both = as.factor(ifelse(feature %in% appear_in_both, 1, 0)))

freq_tfidf <- freq_tfidf %>%
  mutate(appear_in_both = as.factor(ifelse(feature %in% appear_in_both, 1, 0)))

freq_count %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(aes(fill = appear_in_both)) +
  coord_flip() +
  labs(x = "Feature", y = "Count Frequency", title = "EJ Term Frequency (unweighted)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none')

# Fig 2: TF-IDF weighted EJ term frequency
freq_tfidf %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col(aes(fill = appear_in_both)) +
  coord_flip() +
  labs(x = "Feature", y = "TF-IDF Weighted Frequency", title = "EJ Term Frequency (TF-IDF weighted)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'none')

# =============================================================
# 9. CONSULTANT VS NON-CONSULTANT WORD DIFFERENCES
# =============================================================

noncon_words <- textstat_frequency(ej_dfm_tfidf,
                                   groups = "consultant_binary",
                                   force  = TRUE) %>%
  filter(group == 0) %>%
  filter(frequency > 10, docfreq > 10) %>%
  mutate(freq_std = frequency / docfreq) %>%
  tibble()

con_words <- textstat_frequency(ej_dfm_tfidf,
                                groups = "consultant_binary",
                                force  = TRUE) %>%
  filter(group == 1) %>%
  filter(frequency > 10, docfreq > 23) %>%
  mutate(freq_std = frequency / docfreq) %>%
  tibble()

words_used <- inner_join(noncon_words, con_words, by = "feature") %>%
  mutate(diff = freq_std.x - freq_std.y) %>%
  arrange(diff) %>%
  select(feature, freq_std.x, freq_std.y, diff)

words_h10 <- words_used %>% head(10)
words_t10 <- words_used %>% tail(10)

rbind(words_h10, words_t10) %>%
  mutate(`Used More By` = ifelse(diff > 0, "Non-Consultants", "Consultants")) %>%
  ggplot(aes(x = reorder(feature, diff), y = diff)) +
  geom_col(aes(fill = `Used More By`)) +
  coord_flip() +
  labs(x = "Term", y = "Difference in Relative Frequency",
       title = "EJ Terms used more by Consultants and Non-Consultants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# =============================================================
# 10. THEME-LEVEL FREQUENCY BY CONSULTANT USE
# =============================================================

ej_dfm_dict <- ej_dfm_tfidf %>%
  dfm_lookup(ej_dictionary,
             valuetype        = "glob",
             case_insensitive = TRUE)

noncon_agg <- textstat_frequency(ej_dfm_dict,
                                 groups = "consultant_binary",
                                 force  = TRUE) %>%
  filter(group == 0) %>%
  mutate(freq_std = frequency / docfreq) %>%
  tibble()

con_agg <- textstat_frequency(ej_dfm_dict,
                              groups = "consultant_binary",
                              force  = TRUE) %>%
  filter(group == 1) %>%
  mutate(freq_std = frequency / docfreq) %>%
  tibble()

inner_join(noncon_agg, con_agg, by = "feature", suffix = c(".noncon", ".con")) %>%
  select(feature, freq_std.noncon, freq_std.con) %>%
  pivot_longer(cols = starts_with("freq_std"),
               names_to  = "Group",
               values_to = "freq_std") %>%
  mutate(Group = recode(Group,
                        "freq_std.noncon" = "Non-Consultants",
                        "freq_std.con"    = "Consultants")) %>%
  ggplot(aes(x = reorder(feature, freq_std), y = freq_std, fill = Group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Theme", y = "Relative Frequency", fill = "Group",
       title = "EJ Themes used by Consultants and Non-Consultants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# =============================================================
# 11. POPULATION / MHI QUARTILE TABLE
# =============================================================

quants <- as.data.frame(rbind(
  quantile(docvar_df$population, probs = seq(0, 1, length.out = 5), na.rm = TRUE),
  quantile(docvar_df$mhi,        probs = seq(0, 1, length.out = 5), na.rm = TRUE)
)) %>%
  mutate(rowname = c('Population', "MHI"))

gt(quants) %>% fmt_number(decimals = 0)

# =============================================================
# 12. APPENDIX: DICTIONARY TABLES
# =============================================================

keys <- names(ej_dictionary)

key_table  <- data.frame(key = character(), terms = character())
key_tables <- list()

for (key in keys) {
  terms <- ej_dictionary[[key]]
  kt    <- data.frame(key = key, terms = terms)
  key_tables[[key]] <- rbind(key_table, kt)
}

key_tables[[1]] %>% gt()
head(key_tables[[2]], 41) %>% gt()
tail(key_tables[[2]], 41) %>% gt()
key_tables[[3]] %>% gt()
key_tables[[5]] %>% gt()
head(key_tables[[6]], 32) %>% gt()
tail(key_tables[[6]], 32) %>% gt()
key_tables[[7]] %>% gt()
key_tables[[8]] %>% gt()
