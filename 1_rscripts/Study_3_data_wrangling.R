library(tidyverse)

# DATA WRANGLING ####

#wave 9 (2018)
ess9_raw <- haven::read_sav('./0_data/european_social_survey_wave_9_2018.sav')
cdbk_3   <- openxlsx::read.xlsx("./0_data/collective_nature_of_pejudice_codebook.xlsx", sheet = 4)


ess9_raw <- ess9_raw %>%
  janitor::clean_names()


ess9_raw <- 
  ess9_raw %>%
  select(any_of(cdbk_3$ess_variable_name)) %>%
  mutate(
    region_lbl  = as_factor(region,  levels = "label"),
    region_cde  = as_factor(region,  levels = "value"),
    cntry_lbl  = as_factor(cntry,  levels = "label"),
    cntry_cde  = as_factor(cntry,  levels = "value"),
    gndr       = as_factor(gndr,   levels = "label"), .keep = "unused")

# rename variables according to codebook
ess9_raw <- 
  ess9_raw %>%
  rename(any_of(
    setNames(
      cdbk_3$ess_variable_name,
      cdbk_3$variable_label
    ))
  )

# merge education variables into one variables
# code eisced 
ess9_raw <- 
  ess9_raw %>%
  mutate(
    lrscale = as.numeric(lrscale),
    education_eisced_num = as_factor(education_eisced, levels = "value"),
    education_eisced     = as_factor(education_eisced, levels = "label"),
    across(starts_with(c("trad_","prj_")),~as.numeric(.)),
    
    region_cde = as.factor(region_cde),
    region_lbl = as.factor(region_lbl),
    region_lbl = forcats::fct_relabel(
      region_lbl,
      janitor::make_clean_names
    ),
    region_lbl = case_when(region_lbl == "x" ~ NA,
                           TRUE ~ region_lbl),
    idno = paste0(cntry_cde,"_",idno)
    
  )

#variable type numeric

ess9_raw <- 
  ess9_raw %>%
  mutate(across(cdbk_3[cdbk_3$var_type == "numeric","variable_label"],as.numeric))

## Scales ----

### Recoding ----

ess9_raw <- 
  ess9_raw %>%
  mutate(
    trad_1       = 7  - trad_1,
    trad_2       = 7  - trad_2,
    trad_3       = 7  - trad_3,
    trad_4       = 7  - trad_4,
    trad_5       = 7  - trad_5,
    
    prj_immi_1   = 10 - prj_immi_1,
    prj_immi_2   = 10 - prj_immi_2,
    prj_immi_3   = 10 - prj_immi_3,
    
    prj_gay_3    = 6 - prj_gay_3,
  )

# SCALES ----

## Traditionalism ----

cdbk_3 %>% filter(scale == "Traditionalism") %>% select(variable_label,short_item,response_scale,coding)

trad.itms  <- paste0("trad_",seq(1:5))


psych::fa.parallel(ess9_raw %>%
                     select(all_of(trad.itms)))

fa_trad <- psych::fa(ess9_raw %>%
                       select(all_of(trad.itms)), 
                     nfactors = 1, 
                     rotate = "oblimin", 
                     fm = "ml")

print(fa_trad$loadings, cutoff = 0.3)

psych::alpha(ess9_raw[trad.itms])

ess9_raw$trad     <- rowMeans(ess9_raw[trad.itms],na.rm = T)

## Anti-Gay Attitudes ----

#item documetation
cdbk_3 %>% filter(scale == "Prejudice_gay_people") %>% select(variable_label,short_item,response_scale,coding)

anti_gay.itms <- paste0("prj_gay_",seq(1:3))

psych::fa.parallel(ess9_raw %>%
                     select(all_of(anti_gay.itms)))

fa_anti_gay <- 
  psych::fa(ess9_raw %>%
              select(all_of(anti_gay.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")

print(fa_anti_gay$loadings, cutoff = 0.3)

psych::alpha(ess9_raw[anti_gay.itms])

ess9_raw$anti_gay     <- rowMeans(ess9_raw[anti_gay.itms], na.rm = T)

## Anti-Immigrant Attitudes ----

#item documentation
cdbk_3 %>% filter(scale == "Prejudice_immigrants") %>% select(variable_label,short_item,response_scale,coding)

### anti_immigrant ----
anti_mig.itms     <- paste0("prj_immi_",seq(1:3))

psych::fa.parallel(ess9_raw %>%
                     select(all_of(anti_mig.itms)))

fa_anti_mig <- 
  psych::fa(ess9_raw %>%
              select(all_of(anti_mig.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")

print(fa_anti_mig$loadings, cutoff = 0.3)

psych::alpha(ess9_raw[anti_mig.itms])

ess9_raw$anti_mig     <- rowMeans(ess9_raw[anti_mig.itms],na.rm = T)


## Context level Attitudes -----

ess9_raw <- 
  ess9_raw %>%
  group_by(cntry_lbl, region_lbl) %>%
  mutate(rgn_lvl_trad      = mean(trad, na.rm = T),
         cwc_trad          = trad - rgn_lvl_trad,
         rgn_lvl_anti_gay  = mean(anti_gay, na.rm = T),
         cwc_anti_gay     = anti_gay  - rgn_lvl_anti_gay,
         rgn_lvl_anti_mig = mean(anti_mig, na.rm = T),
         cwc_anti_mig     = anti_mig - rgn_lvl_anti_mig) %>%
  ungroup()%>%
  mutate(gmc_anti_gay  = anti_gay  - mean(anti_gay, na.rm = T),
         gmc_anti_mig = anti_mig - mean(anti_mig, na.rm = T))%>%
  group_by(cntry_lbl) %>%
  mutate(cntry_lvl_anti_gay  = mean(anti_gay, na.rm = T),
         cntry_lvl_anti_mig = mean(anti_mig,na.rm = T),
         cntry_lvl_trad      = mean(trad, na.rm = T)) %>%
  ungroup()

# CENTERING ----
ess9_raw <- 
  ess9_raw %>%
  mutate(rgn_lvl_anti_mig.gmc = rgn_lvl_anti_mig - mean(anti_mig,na.rm = T),
         rgn_lvl_anti_gay.gmc  = rgn_lvl_anti_gay  - mean(anti_gay, na.rm = T))

ess9_ana <-
  ess9_raw %>% select(!matches('_[0-9]$'))


