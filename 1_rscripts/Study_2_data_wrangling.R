# LIBRARIES ----

library(tidyverse)
library(lavaan)
library(haven)

# DATA WRANGLING ####

#wave 4 (2018)
ess4_raw <- read_sav('./0_data/european_social_survey_wave_4_2008_4.6.sav')
cdbk_2 <- openxlsx::read.xlsx("./0_data/collective_nature_of_pejudice_codebook.xlsx", sheet = 3)


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

### alphas ----

#traditionalism
psych::alpha(ess4_raw[c("trad_1","trad_2","trad_3","trad_4","trad_5")])

#anti-immigrant attitudes
psych::alpha(ess4_raw[c("prj_immi_1","prj_immi_2","prj_immi_3")])

#prejudice against women
psych::alpha(ess4_raw[c("prj_wmn_1","prj_wmn_2")])


### scale means ----
ess4_ana <- 
  ess4_raw %>%
  mutate(
    #traditionalism
    trad_raw = rowMeans(select(.,c("trad_1","trad_2","trad_3","trad_4","trad_5")),na.rm = TRUE),
    #anti-immigrant
    prj_immigrants_raw = rowMeans(select(.,c("prj_immi_1","prj_immi_2","prj_immi_3")),na.rm = TRUE),
    #anti-gay
    prj_gay_raw = prj_gay_1,
    #unemployed
    prj_unemployed_raw = prj_unempl_1,
    #women
    prj_women_raw = rowMeans(select(.,c("prj_wmn_1","prj_wmn_2")),na.rm = TRUE),
    #people in their 20s
    prj_age20_raw = prj_age20,
    #people over 70
    prj_age70_raw = prj_age70,
    
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


