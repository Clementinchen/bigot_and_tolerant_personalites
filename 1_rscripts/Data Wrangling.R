# =============================================================================
# DATA WRANGLING – Studies 1–5
# -----------------------------------------------------------------------------
# Input : ./0_data/  (raw data + codebook "collective_nature_of_pejudice_codebook.xlsx")
# Output: ./1_analysis_data/  (one .rds per analysis data set)
#
# Naming convention (all studies):
#   dsX_raw  raw data as read from disk (never modified)
#   cdbk_X   codebook (sheet X)
#   dsX      working data set (cleaned, recoded, scored)
#   dsX_wde  final analysis data set, wide format
#   dsX_lng  final analysis data set, long format (studies 1–3)
#
#   Study 1: RWA/SDO revisited, Study 1a      (online sample)
#   Study 2: RWA/SDO revisited, Study 1b      (online sample)
#   Study 3: Personality & group prejudice    (3 waves)
#   Study 4: European Social Survey, wave 4 (2008)
#   Study 5: European Social Survey, wave 9 (2018)
# =============================================================================


# 0  SETUP --------------------------------------------------------------------

library(tidyverse)
library(haven)
library(psych)

set.seed(1234)              # fa.parallel() uses random resampling
run_diagnostics <- TRUE     # FALSE = skip factor analyses, alphas, descriptives

data_dir  <- "./0_data"
out_dir   <- "./1_analysis_data"
cdbk_file <- file.path(data_dir, "collective_nature_of_pejudice_codebook.xlsx")
dir.create(out_dir, showWarnings = FALSE)


# 0.1 Helper functions --------------------------------------------------------

# Read one codebook sheet
read_codebook <- function(sheet) openxlsx::read.xlsx(cdbk_file, sheet = sheet)

# Rename variables according to a codebook (column `from` -> column `to`).
# Reports codebook variables that were not found in the data.
rename_from_codebook <- function(data, cdbk, from, to) {
  lookup  <- setNames(cdbk[[from]], cdbk[[to]])        # new_name = old_name
  missing <- setdiff(lookup, names(data))
  if (length(missing) > 0)
    message("Codebook variables not found in data: ", paste(missing, collapse = ", "))
  rename(data, any_of(lookup))
}

# Reverse-code an item on a scale from `min` to `max`
reverse <- function(x, min, max) min + max - x

# Clean factor levels (snake_case); empty labels (-> "x") become NA
clean_region <- function(x) {
  x <- forcats::fct_relabel(as.factor(x), janitor::make_clean_names)
  droplevels(replace(x, x == "x", NA))
}

# Studies 1–2: recode gender and education (ISCED)
recode_demographics <- function(data) {
  data %>%
    mutate(
      gender    = factor(case_match(gender, 1 ~ "female", 2 ~ "male", 3 ~ "divers", NA ~ "preferred not to answer")),
      education = factor(education, levels = 1:7,
                         labels = c("isced_1", "isced_2", "isced_2", "isced_3",
                                    "isced_4", "isced_6", "isced_7"))
    )
}

# Studies 1–2: long format (one row per person x target)
make_long_targets <- function(data) {
  data %>%
    mutate(across(c(rwa, sdo, polid), ~ as.numeric(scale(.x)), .names = "{.col}_scl")) %>%
    select(any_of(c("case", "age", "gender", "education")),
           rwa, sdo, polid, ends_with("_scl"),
           starts_with(c("prj_"))) %>%
    pivot_longer(starts_with(c("prj_")),
                 names_to = "prj_target", values_to = "prj_rating") %>%
    group_by(prj_target) %>%
    mutate(prj_rating_scl = as.numeric(scale(prj_rating)),
           prj_rating_gmc = prj_rating - mean(prj_rating, na.rm = TRUE)) %>%
    ungroup()
}

# Diagnostics: parallel analysis, 1-factor ML solution, Cronbach's alpha
check_scale <- function(data, items, label = "") {
  cat("\n====", label, "====\n")
  x <- na.omit(data[items])
  psych::fa.parallel(x, fm = "ml", fa = "fa", n.iter = 100, main = label)
  print(psych::fa(x, nfactors = 1, rotate = "oblimin", fm = "ml")$loadings, cutoff = .3)
  if (length(items) > 1) print(psych::alpha(data[items])$total)
}


# =============================================================================
# 1  STUDY 1 (RWA/SDO revisited, Study 1a)
# =============================================================================

## 1.1 Read data & codebook ----
ds1_raw <- read.csv(file.path(data_dir, "rwa_sdo_revisited_study_1.csv"),
                    na.strings = c("-1", "-9", NA)) %>%
  janitor::clean_names()

cdbk_1 <- read_codebook(1) %>% mutate(variable = tolower(variable))

## 1.2 Rename, demographics, reverse-code prejudice ----
# Prejudice items: 11-point scale (1–11), reversed (= 12 - x) so that high = more prejudice
ds1 <- ds1_raw %>%
  rename_from_codebook(cdbk_1, from = "variable", to = "variable_label") %>%
  recode_demographics() %>%
  mutate(across(starts_with("prj_"), ~ reverse(.x, 1, 11)))

## 1.3 Predictors: RWA, SDO (scale means); polid = single item ----
rwa_items_1 <- names(select(ds1, starts_with("rwa_")))
sdo_items_1 <- names(select(ds1, starts_with("sdo_")))
trgt_items_1 <- names(select(ds1, starts_with("prj_")))

ds1 <- ds1 %>%
  mutate(rwa = rowMeans(pick(all_of(rwa_items_1)), na.rm = TRUE),
         sdo = rowMeans(pick(all_of(sdo_items_1)), na.rm = TRUE))

## 1.4 Diagnostics ----
if (run_diagnostics) {
  check_scale(ds1, rwa_items_1, "Study 1: RWA")
  check_scale(ds1, sdo_items_1, "Study 1: SDO")
  print(psych::describe(select(ds1, rwa, sdo, polid, starts_with("prj"))))
}

## 1.5 Analysis data sets ----
ds1_wde <- ds1 %>%
  select(case, age, gender, education, rwa, sdo, polid,
         starts_with("prj_"))

ds1_lng <- make_long_targets(ds1_wde)


# =============================================================================
# 2  STUDY 2 (RWA/SDO revisited, Study 1b)
# =============================================================================

## 2.1 Read data & codebook ----
ds2_raw <- read.csv(file.path(data_dir, "rwa_sdo_revisited_study_2.csv"),
                    na.strings = c("-1", "-9", NA)) %>%
  janitor::clean_names()

cdbk_2 <- read_codebook(2) %>% mutate(variable = tolower(variable))

## 2.2 Rename, demographics, reverse-code prejudice ----
# Prejudice items: 101-point scale (1–101), reversed (= 102 - x)
ds2 <- ds2_raw %>%
  rename_from_codebook(cdbk_2, from = "variable", to = "variable_label") %>%
  recode_demographics() %>%
  mutate(across(starts_with("prj_"), ~ reverse(.x, 1, 101)))

## 2.3 Predictors ----
rwa_items_2  <- names(select(ds2, starts_with("rwa_")))
sdo_items_2  <- names(select(ds2, starts_with("sdo_")))
trgt_items_2 <- names(select(ds2, starts_with("prj_")))

ds2 <- ds2 %>%
  mutate(rwa = rowMeans(pick(all_of(rwa_items_2)), na.rm = TRUE),
         sdo = rowMeans(pick(all_of(sdo_items_2)), na.rm = TRUE))

## 2.4 Diagnostics ----
if (run_diagnostics) {
  check_scale(ds2, rwa_items_2, "Study 2: RWA")
  check_scale(ds2, sdo_items_2, "Study 2: SDO")
  print(psych::describe(select(ds2, rwa, sdo, polid, starts_with("prj"))))
}

## 2.5 Analysis data sets ----
ds2_wde <- ds2 %>%
  select(any_of(c("case", "age", "gender", "education")), rwa, sdo, polid,
         starts_with(c("prj_")))

ds2_lng <- make_long_targets(ds2_wde)


# =============================================================================
# 3  STUDY 3 (Personality & group prejudice; 3 waves)
# =============================================================================

## 3.1 Read data & codebook ----
ds3_raw <- read.csv(file.path(data_dir, "rwa_sdo_revisited_study_3.csv"),
                    header = T, na.strings = c("", "-9", "-1"))

cdbk_3 <- read_codebook(3)



## 3.2 Sample selection, rename, reverse-code prejudice ----
# Inclusion: CO02 == 1 and MAXPAGE > 34
# Prejudice items: 101-point scale (1–101), reversed (= 102 - x)
ds3 <- ds3_raw %>%
  filter(CO02 == 1 & MAXPAGE > 34) %>%
  rename_from_codebook(cdbk_3, from = "variable", to = "variable_label") %>%
  janitor::clean_names() %>%
  select(case, starts_with(c("rwa_", "sdo_", "prj_")), age, gender, polid, education) %>%
  mutate(across(starts_with("prj_"), ~ reverse(.x, 1, 101)),
         education = case_when(education == 1 ~ "isced_1",education == 2 ~ "isced_2",
                               education == 3 ~ "isced_2",education == 4 ~ "isced_2",
                               education == 5 ~ "isced_3",education == 6 ~ "isced_3",
                               education == 7 ~ "isced_3",education == 8 ~ "isced_4",
                               education == 9 ~ NA))

## 3.3 Predictors per wave: RWA (9 items), SDO (4 items) ----
waves <- paste0("w", 1:3)
rwa_items_3 <- function(w) sprintf("rwa_%02d_%s", 1:9, w)
sdo_items_3 <- function(w) sprintf("sdo_%02d_%s", 1:4, w)

if (run_diagnostics) {
  for (w in waves) {
    check_scale(ds3, rwa_items_3(w), paste("Study 3: RWA", w))
    check_scale(ds3, sdo_items_3(w), paste("Study 3: SDO", w))
  }
}

for (w in waves) {
  ds3[[paste0("rwa_", w)]] <- rowMeans(ds3[rwa_items_3(w)], na.rm = TRUE)
  ds3[[paste0("sdo_", w)]] <- rowMeans(ds3[sdo_items_3(w)], na.rm = TRUE)
}

if (run_diagnostics) {
  print(psych::describe(rowMeans(ds3[paste0("rwa_", waves)])))   # person mean across waves
  print(psych::describe(rowMeans(ds3[paste0("sdo_", waves)])))
}

## 3.4 Analysis data sets ----
ds3_wde <- ds3

# Long format: one row per person x wave x target
# RWA/SDO standardised within wave; polid standardised once (time-invariant)
ds3_prd <- ds3 %>%
  select(case, polid, matches("^(rwa|sdo)_w\\d$")) %>%
  pivot_longer(matches("^(rwa|sdo)_w\\d$"),
               names_to = c(".value", "wave"), names_sep = "_") %>%
  group_by(wave) %>%
  mutate(
    rwa_scl = as.numeric(scale(rwa)),
    sdo_scl = as.numeric(scale(sdo)),
    polid_scl = as.numeric(scale(polid))
  ) %>%
  ungroup()

ds3_prj <- ds3 %>%
  select(case, starts_with("prj_")) %>%
  pivot_longer(-case, names_to = c("prj_target", "wave"),
               names_pattern = "^(prj_.*)_(w\\d)$", values_to = "prj_rating")

ds3_lng <- full_join(ds3_prd, ds3_prj, by = c("case", "wave"))
rm(ds3_prd, ds3_prj)


# =============================================================================
# 4  STUDY 4 (European Social Survey, wave 4, 2008) ####
# =============================================================================

## 4.1 Read data & codebook ----
ds4_raw <- read_sav(file.path(data_dir, "european_social_survey_wave_4_2008_4.6.sav"))
cdbk_4  <- read_codebook(4)

## 4.2 Select, merge region variables, rename ----
# ESS 4 has one region variable per country -> merged into `region`.
# Most are named region<cc>, but eight countries use regioa<cc> / regiob<cc>
# (CH, CZ, DK, ES, FI, GR, IE, PT), hence the prefix "regio".
ds4 <- ds4_raw %>%
  select(any_of(cdbk_4$ess_variable_name), starts_with("regio")) %>%
  mutate(across(starts_with("regio"), ~ as.character(as_factor(.x, levels = "labels")))) %>%
  unite("region", starts_with("regio"), na.rm = TRUE) %>%
  rename_from_codebook(cdbk_4, from = "ess_variable_name", to = "variable_label") %>%
  mutate(cntry_lbl = as_factor(cntry, levels = "labels"),
         cntry_cde = as_factor(cntry, levels = "values"),
         gender      = as_factor(gender,  levels = "labels"),
         .keep = "unused") 

## 4.3 Variable types, age outliers, IDs ----
num_vars_4 <- cdbk_4$variable_label[cdbk_4$var_type == "numeric"]

ds4 <- ds4 %>%
  mutate(
    across(any_of(num_vars_4), as.numeric),
    education_num = as_factor(education, levels = "values"),
    education     = as_factor(education, levels = "labels"),
    age = if_else(age > median(age, na.rm = TRUE) + 3 * sd(age, na.rm = TRUE), NA_real_, age),
    region = clean_region(region),
    # Region labels are only unique within a country (e.g. "center" in PT and RU):
    # region_id identifies regions in the models; `region` is kept for shapefile matching
    region_id = factor(if_else(is.na(region), NA, paste(cntry_cde, region, sep = "_"))),
    case   = paste0(cntry_cde, "_", case)
  )

## 4.4 Reverse-code (high = more traditionalism / prejudice) ----
ds4 <- ds4 %>%
  mutate(
    across(all_of(paste0("trad_", 1:5)),        ~ reverse(.x, 1, 6)),
    across(all_of(paste0("prj_immi_", 1:3)),    ~ reverse(.x, 0, 10)),
    across(c(prj_unempl_1, prj_wmn_1, prj_wmn_2), ~ reverse(.x, 1, 5)),
    across(c(prj_age20, prj_age70),             ~ reverse(.x, 0, 10))
  )

## 4.5 Scales ----
# For each scale: _raw = mean on original metric, _lin = stretched to 0–1
# (using the documented scale endpoints), _scl = z-standardised
scales_4 <- list(
  trad           = list(items = paste0("trad_", 1:5),     range = c(1, 6)),
  polid          = list(items = "polid",                  range = c(0, 10)),
  prj_immigrants = list(items = paste0("prj_immi_", 1:3), range = c(0, 10)),
  prj_gay        = list(items = "prj_gay_1",              range = c(1, 5)),
  prj_unemployed = list(items = "prj_unempl_1",           range = c(1, 5)),
  prj_women      = list(items = paste0("prj_wmn_", 1:2),  range = c(1, 5)),
  prj_age20      = list(items = "prj_age20",              range = c(0, 10)),
  prj_age70      = list(items = "prj_age70",              range = c(0, 10))
)

if (run_diagnostics) {
  for (s in names(scales_4)) {
    if (length(scales_4[[s]]$items) > 1) {
      cat("\n==== Study 4:", s, "====\n")
      print(psych::alpha(ds4[scales_4[[s]]$items])$total)
    }
  }
}

for (s in names(scales_4)) {
  sc  <- scales_4[[s]]
  raw <- rowMeans(ds4[sc$items], na.rm = TRUE)
  ds4[[paste0(s, "_raw")]] <- raw
  ds4[[paste0(s, "_lin")]] <- (raw - sc$range[1]) / diff(sc$range)
  ds4[[paste0(s, "_scl")]] <- as.numeric(scale(raw))
}

if (run_diagnostics) print(psych::describe(select(ds4, ends_with("_lin"))))

prj_targets_4 <- names(scales_4)[-1]

## 4.6 Analysis data set (items dropped) ----
ds4_wde <- ds4 %>% select(-all_of(unlist(map(scales_4, "items"))))


# =============================================================================
# 5  STUDY 5 (European Social Survey, wave 9, 2018)
# =============================================================================

## 5.1 Read data & codebook ----
ds5_raw <- read_sav(file.path(data_dir, "european_social_survey_wave_9_2018.sav")) %>%
  janitor::clean_names()
cdbk_5  <- read_codebook(5)

## 5.2 Select, labels, rename ----
ds5 <- ds5_raw %>%
  select(any_of(cdbk_5$ess_variable_name)) %>%
  rename_from_codebook(cdbk_5, from = "ess_variable_name", to = "variable_label") %>%
  mutate(region_lbl = as_factor(region, levels = "labels"),
         region_cde = as_factor(region, levels = "values"),
         cntry_lbl  = as_factor(cntry,  levels = "labels"),
         cntry_cde  = as_factor(cntry,  levels = "values"),
         gender       = as_factor(gender,   levels = "labels"),
         .keep = "unused") 

## 5.3 Variable types, IDs ----
num_vars_5 <- cdbk_5$variable_label[cdbk_5$var_type == "numeric"]

ds5 <- ds5 %>%
  mutate(
    across(any_of(num_vars_5), as.numeric),
    across(starts_with(c("trad_", "prj_")), as.numeric),
    polid              = as.numeric(polid),
    education_num = as_factor(education, levels = "values"),
    education     = as_factor(education, levels = "labels"),
    region_lbl = clean_region(region_lbl),
    case       = paste0(cntry_cde, "_", case)
  )

## 5.4 Reverse-code (high = more traditionalism / prejudice) ----
trad_items_5     <- paste0("trad_", 1:5)
anti_gay_items_5 <- paste0("prj_gay_", 1:3)
anti_mig_items_5 <- paste0("prj_immi_", 1:3)

ds5 <- ds5 %>%
  mutate(
    across(all_of(trad_items_5),     ~ reverse(.x, 1, 6)),
    across(all_of(anti_mig_items_5), ~ reverse(.x, 0, 10)),
    prj_gay_3 = reverse(prj_gay_3, 1, 5)
  )

## 5.5 Scales ----
if (run_diagnostics) {
  check_scale(ds5, trad_items_5,     "Study 5: traditionalism")
  check_scale(ds5, anti_gay_items_5, "Study 5: anti-gay attitudes")
  check_scale(ds5, anti_mig_items_5, "Study 5: anti-immigrant attitudes")
}

ds5 <- ds5 %>%
  mutate(trad     = rowMeans(pick(all_of(trad_items_5)),     na.rm = TRUE),
         anti_gay = rowMeans(pick(all_of(anti_gay_items_5)), na.rm = TRUE),
         anti_mig = rowMeans(pick(all_of(anti_mig_items_5)), na.rm = TRUE))

## 5.6 Context level: region and country means, centering ----
# rgn_lvl_* = region mean (within country); cwc_* = centred within region;
# gmc_* = grand-mean centred; cntry_lvl_* = country mean
ds5 <- ds5 %>%
  group_by(cntry_lbl, region_lbl) %>%
  mutate(rgn_lvl_trad     = mean(trad,     na.rm = TRUE),
         rgn_lvl_anti_gay = mean(anti_gay, na.rm = TRUE),
         rgn_lvl_anti_mig = mean(anti_mig, na.rm = TRUE),
         cwc_trad         = trad     - rgn_lvl_trad,
         cwc_anti_gay     = anti_gay - rgn_lvl_anti_gay,
         cwc_anti_mig     = anti_mig - rgn_lvl_anti_mig) %>%
  group_by(cntry_lbl) %>%
  mutate(cntry_lvl_trad     = mean(trad,     na.rm = TRUE),
         cntry_lvl_anti_gay = mean(anti_gay, na.rm = TRUE),
         cntry_lvl_anti_mig = mean(anti_mig, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gmc_anti_gay         = anti_gay - mean(anti_gay, na.rm = TRUE),
         gmc_anti_mig         = anti_mig - mean(anti_mig, na.rm = TRUE),
         rgn_lvl_anti_gay.gmc = rgn_lvl_anti_gay - mean(anti_gay, na.rm = TRUE),
         rgn_lvl_anti_mig.gmc = rgn_lvl_anti_mig - mean(anti_mig, na.rm = TRUE))

## 5.7 Analysis data set (items dropped) ----
ds5_wde <- ds5 %>% select(!matches("_[0-9]$"))


# =============================================================================
# 6  SAVE ANALYSIS DATA SETS
# =============================================================================

analysis_data <- list(
  ds1_wde = ds1_wde, ds1_lng = ds1_lng,
  ds2_wde = ds2_wde, ds2_lng = ds2_lng,
  ds3_wde = ds3_wde, ds3_lng = ds3_lng,
  ds4_wde = ds4_wde,
  ds5_wde = ds5_wde
)

iwalk(analysis_data, ~ saveRDS(.x, file.path(out_dir, paste0(.y, ".rds"))))
