# HELPER FUNCTION -----

lin_stretch <- function(x, old_min, old_max, new_min = 0, new_max = 1) {
  if (old_max == old_min) {
    stop("Linear stretching impossible: values are equal.")
  }
  new_min + ((x - old_min) * (new_max - new_min)) / (old_max - old_min)
}

# LIBRARIES ----

library(tidyverse)
library(lavaan)
library(haven)

# DATA WRANGLING ####

#wave 4 (2018)
ess4_raw <- read_sav('./0_data/european_social_survey_wave_4_2008_4.6.sav')
cdbk_2 <- openxlsx::read.xlsx("./0_data/collective_nature_of_pejudice_codebook.xlsx", sheet = 4)


## Clean Variables ----

# merge region variables into one variable
ess4_raw <- 
  ess4_raw %>%
  select(any_of(cdbk_2$ess_variable_name),starts_with("region"))  %>%
  mutate(across(starts_with("regi"),
                ~as_factor(.,levels = "label"))) %>%
  tidyr::unite("region",starts_with("regio"),na.rm = TRUE) %>%
  mutate(cntry_lbl  = as_factor(cntry,  levels = "label"),
         cntry_cde  = as_factor(cntry,  levels = "value"),
         gndr       = as_factor(gndr,   levels = "label"), .keep = "unused")

# rename variables according to codebook
ess4_raw <- 
  ess4_raw %>%
  rename(any_of(
    setNames(
      cdbk_2$ess_variable_name,
      cdbk_2$variable_label
    ))
  )

# merge education variables into one variables
# code eisced 
ess4_raw <- 
  ess4_raw %>%
  mutate(
    lrscale = as.numeric(lrscale),
    education_eisced_num = as_factor(education_eisced, levels = "value"),
    education_eisced     = as_factor(education_eisced, levels = "label"),
    age                  = case_when(age > (median(ess4_raw$age,na.rm = T) + 3*sd(age,na.rm = T)) ~ NA,
                                     TRUE ~ age),
    across(starts_with(c("trad_","prj_")),~as.numeric(.)),
    
    region = as.factor(region),
    region = forcats::fct_relabel(
      region,
      janitor::make_clean_names
    ),
    region = case_when(region == "x" ~ NA,
                       TRUE ~ region),
    idno = paste0(cntry_cde,"_",idno)
    
  )

#variable type numeric

ess4_raw %>%
  mutate(across(cdbk_2[cdbk_2$var_type == "numeric","variable_label"],as.numeric))

## Scales ----

### Recoding ----

ess4_raw <- 
  ess4_raw %>%
  mutate(
    trad_1       = 7  - trad_1,
    trad_2       = 7  - trad_2,
    trad_3       = 7  - trad_3,
    trad_4       = 7  - trad_4,
    trad_5       = 7  - trad_5,
    
    prj_immi_1   = 10 - prj_immi_1,
    prj_immi_2   = 10 - prj_immi_2,
    prj_immi_3   = 10 - prj_immi_3,
    
    prj_unempl_1 = 6  - prj_unempl_1,
    
    prj_wmn_1    = 6  - prj_wmn_1,
    prj_wmn_2    = 6  - prj_wmn_2,
    
    prj_age20    = 10 - prj_age20,
    prj_age70    = 10 - prj_age70
  )

### rescaling ----

# dokumentierte Skalenendpunkte pro Item — bitte aus deinem Codebook/Table 1 eintragen
scale_endpoints <- list(
  trad_1            = c(min = 1, max = 6),
  trad_2            = c(min = 1, max = 6),
  trad_3            = c(min = 1, max = 6),
  trad_4            = c(min = 1, max = 6),
  trad_5            = c(min = 1, max = 6),
  prj_immi_1        = c(min = 0, max = 10),
  prj_immi_2        = c(min = 0, max = 10), 
  prj_immi_3        = c(min = 0, max = 10), 
  prj_gay_1         = c(min = 1, max = 5),   
  prj_unempl_1      = c(min = 1, max = 5),   
  prj_wmn_1        = c(min = 1, max = 5),
  prj_wmn_2        = c(min = 1, max = 5),
  prj_age20         = c(min = 0, max = 10),   
  prj_age70         = c(min = 0, max = 10)    
)

ess4_raw <- ess4_raw %>%
  mutate(
    across(
      all_of(names(scale_endpoints)),
      ~ lin_stretch(
        .x,
        old_min = scale_endpoints[[cur_column()]]["min"],
        old_max = scale_endpoints[[cur_column()]]["max"]
      ),
      .names = "{.col}_lin"
    )
  )

### alphas ----

#traditionalism
psych::alpha(ess4_raw[c("trad_1","trad_2","trad_3","trad_4","trad_5")])

#anti-immigrant attitudes
psych::alpha(ess4_raw[c("prj_immi_1","prj_immi_2","prj_immi_3")])
psych::alpha(ess4_raw[c("prj_immi_1_lin","prj_immi_2_lin","prj_immi_3_lin")])

#prejudice against women
psych::alpha(ess4_raw[c("prj_wmn_1","prj_wmn_2")])
psych::alpha(ess4_raw[c("prj_wmn_1_lin","prj_wmn_2_lin")])


### scale means ----
ess4_ana <- 
  ess4_raw %>%
  mutate(
    #traditionalism
    trad_raw = rowMeans(select(.,c("trad_1","trad_2","trad_3","trad_4","trad_5")),na.rm = TRUE),
    #anti-immigrant
    prj_immigrants_raw = rowMeans(select(.,c("prj_immi_1","prj_immi_2","prj_immi_3")),na.rm = TRUE),
    prj_immigrants_lin = rowMeans(select(.,c("prj_immi_1_lin","prj_immi_2_lin","prj_immi_3_lin")),na.rm = TRUE),
    #anti-gay
    prj_gay_raw = prj_gay_1,
    prj_gay_lin = prj_gay_1_lin,
    #unemployed
    prj_unemployed_raw = prj_unempl_1,
    prj_unemployed_lin = prj_unempl_1_lin,
    #women
    prj_women_raw = rowMeans(select(.,c("prj_wmn_1","prj_wmn_2")),na.rm = TRUE),
    prj_women_lin = rowMeans(select(.,c("prj_wmn_1_lin","prj_wmn_2_lin")),na.rm = TRUE),
    #people in their 20s
    prj_age20_raw = prj_age20,
    prj_age20_lin = prj_age20_lin,
    #people over 70
    prj_age70_raw = prj_age70,
    prj_age70_lin = prj_age70_lin,
    
    .keep = "unused"
  )

#standardize scales
ess4_ana <- 
  ess4_ana %>%
  mutate(
    across(
      c("trad_raw","prj_immigrants_raw","prj_gay_raw","prj_unemployed_raw","prj_women_raw","prj_age20_raw","prj_age70_raw"),
      ~as.numeric(scale(.)),.names = "{sub('raw','scl',col)}")
  )
psych::describe(ess4_ana[c("trad_raw","prj_immigrants_lin","prj_gay_lin","prj_unemployed_lin","prj_women_lin","prj_age20_lin","prj_age70_lin")])



prj_targets <- c("prj_immigrants","prj_gay","prj_unemployed","prj_women","prj_age20","prj_age70")

