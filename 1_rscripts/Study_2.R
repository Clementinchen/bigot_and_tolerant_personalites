library(tidyverse)
library(lavaan)
library(haven)

# FUNCTION ####

add_scale_mean_alpha <- function(data, scale_name, items) {
  
  # Alpha berechnen und ausgeben
  alpha_res <- psych::alpha(
    data %>% dplyr::select(any_of(items))
  )
  print(alpha_res$total)
  
  # Skalenmittelwert berechnen
  data[[scale_name]] <- rowMeans(
    data[, items],
    na.rm = TRUE
  )
  
  return(data)
}

# DATA WRANGLING ####

#wave 4 (2018)
ess4 <- read_sav('./0_data/european_social_survey_wave_4_2008_4.6.sav')
cdbk_2 <- openxlsx::read.xlsx("./0_data/collective_nature_of_pejudice_codebook.xlsx", sheet = 3)


ess4 <- 
  ess4 %>%
  select(any_of(cdbk_2$ess_variable_name),starts_with("region"))  %>%
  mutate(across(starts_with("regi"),
            ~as_factor(.,levels = "label"))) %>%
  tidyr::unite("region",starts_with("regio"),na.rm = TRUE) %>%
  mutate(cntry_lbl  = as_factor(cntry,  levels = "label"),
         cntry_cde  = as_factor(cntry,  levels = "value"),
         gndr       = as_factor(gndr,   levels = "label"), .keep = "unused")

ess4 <- 
  ess4 %>%
  rename(any_of(
    setNames(
      cdbk_2$ess_variable_name,
      cdbk_2$variable_label
    ))
  )

ess4 <- 
  ess4 %>%
  mutate(
    lrscale = as.numeric(lrscale),
    education_eisced_num = as_factor(education_eisced, levels = "value"),
    education_eisced     = as_factor(education_eisced, levels = "label"),
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


# SCALES ----

## Recoding ----

ess4 <- 
  ess4 %>%
  mutate(
    trad_1       = 7  - trad_1,
    trad_2       = 7  - trad_2,
    trad_3       = 7  - trad_3,
    trad_4       = 7  - trad_4,
    trad_5       = 7  - trad_5,
    
    prj_immi_1   = 11 - prj_immi_1,
    prj_immi_2   = 11 - prj_immi_2,
    prj_immi_3   = 11 - prj_immi_3,
    
    prj_unempl_1 = 6  - prj_unempl_1,
    
    prj_wmn_1    = 6  - prj_wmn_1,
    prj_wmn_2    = 6  - prj_wmn_2,
    
    prj_age20    = 11 - prj_age20,
    prj_age70    = 11 - prj_age70
  )
  
## Rescaling ----

rescale_linear <- function(x, old_min, old_max, new_min, new_max) {
  ( (x - old_min) / (old_max - old_min) ) * (new_max - new_min) + new_min
}

ess4 <- 
  ess4 %>%
  mutate(
    across(starts_with("prj_"),~as.numeric(scale(.)),.names = "{.col}_scl"),
   
    prj_immi_1_resc   = rescale_linear(prj_immi_1, old_min = 1, old_max = 11, new_min = 0, new_max = 1),
    prj_immi_2_resc   = rescale_linear(prj_immi_2, old_min = 1, old_max = 11, new_min = 0, new_max = 1),
    prj_immi_3_resc   = rescale_linear(prj_immi_3, old_min = 1, old_max = 11, new_min = 0, new_max = 1),
    
    prj_gay_1_resc    = rescale_linear(prj_gay_1,    old_min = 1, old_max = 5 , new_min = 0, new_max = 1),
    
    prj_unempl_1_resc = rescale_linear(prj_unempl_1, old_min = 1, old_max = 5 , new_min = 0, new_max = 1),
    
    prj_wmn_1_resc    = rescale_linear(prj_wmn_1,    old_min = 1, old_max = 5 , new_min = 0, new_max = 1),
    prj_wmn_2_resc    = rescale_linear(prj_wmn_2,    old_min = 1, old_max = 5 , new_min = 0, new_max = 1),
    
    prj_age20_resc    = rescale_linear(prj_age20,    old_min = 1, old_max = 11 , new_min = 0, new_max = 1),
    prj_age70_resc    = rescale_linear(prj_age70,    old_min = 1, old_max = 11 , new_min = 0, new_max = 1)
  )

## ALPHAS ----

psych::alpha(ess4[c("trad_1","trad_2","trad_3","trad_4","trad_5")])

psych::alpha(ess4[c("prj_immi_1","prj_immi_2","prj_immi_3")])
psych::alpha(ess4[c("prj_immi_1_scl","prj_immi_2_scl","prj_immi_3_scl")])

psych::alpha(ess4[c("prj_wmn_1","prj_wmn_2")])
psych::alpha(ess4[c("prj_wmn_1_scl","prj_wmn_2_scl")])
psych::alpha(ess4[c("prj_wmn_1_resc","prj_wmn_2_resc")])

## SCALE MEANS ----

ess4 <- 
  ess4 %>%
  mutate(
    trd = rowMeans(select(.,c("trad_1","trad_2","trad_3","trad_4","trad_5")),na.rm = TRUE),
    
    prj_immigrants_raw = rowMeans(select(.,c("prj_immi_1","prj_immi_2","prj_immi_3")),na.rm = TRUE),
    prj_immigrants_scl = rowMeans(select(.,c("prj_immi_1_scl","prj_immi_2_scl","prj_immi_3_scl")),na.rm = TRUE),
    prj_immigrants_rsc = rowMeans(select(.,c("prj_immi_1_resc","prj_immi_2_resc","prj_immi_3_resc")),na.rm = TRUE),
    
    prj_gay_raw = prj_gay_1,
    prj_gay_scl = prj_gay_1_scl,
    prj_gay_rsc = prj_gay_1_resc,
    
    prj_unemployed_raw = prj_unempl_1,
    prj_unemployed_scl = prj_unempl_1_scl,
    prj_unemployed_rsc = prj_unempl_1_resc,
    
    prj_women_raw = rowMeans(select(.,c("prj_wmn_1","prj_wmn_2")),na.rm = TRUE),
    prj_women_scl = rowMeans(select(.,c("prj_wmn_1_scl","prj_wmn_2_scl")),na.rm = TRUE),
    prj_women_rsc = rowMeans(select(.,c("prj_wmn_1_resc","prj_wmn_2_resc")),na.rm = TRUE),
    
    prj_age20_raw = prj_age20,
    prj_age20_scl = prj_age20_scl,
    prj_age20_rsc = prj_age20_resc,
    
    prj_age70_raw = prj_age70,
    prj_age70_scl = prj_age70_scl,
    prj_age70_rsc = prj_age70_resc,
    .keep = "unused"
  )

ess4 <- 
  ess4 %>%
  select(-matches("_[0-9]"))

## CORRELATIONS ----

ess4 %>%
  select(trd,ends_with("_rsc")) %>%
  correlation::correlation()


ess4_long <- 
  ess4 %>%
  select(-ends_with("_scl"),
           -ends_with("_rsc")) %>%
  pivot_longer(
    cols = starts_with("prj"),
    names_to = "prj_target",
    values_to = "prj_rating_raw"
  ) %>%
  mutate(
    prj_target = str_remove_all(prj_target,"_raw")
    )

ess4_long <- 
  ess4 %>%
  select(idno,
         ends_with("_scl")) %>%
  pivot_longer(
    cols = starts_with("prj"),
    names_to = "prj_target",
    values_to = "prj_rating_scl"
  ) %>%
  mutate(
    prj_target = str_remove_all(prj_target,"_scl")
  ) %>%
    left_join(
      ess4_long,
      .,
      by = c("idno","prj_target")
    )

ess4_long <- 
  ess4 %>%
  select(idno,
         ends_with("_rsc")) %>%
  pivot_longer(
    cols = starts_with("prj"),
    names_to = "prj_target",
    values_to = "prj_rating_rsc"
  ) %>%
  mutate(
    prj_target = str_remove_all(prj_target,"_rsc")
  ) %>%
  left_join(
    ess4_long,
    .,
    by = c("idno","prj_target")
  )

ess4_long %>%
  select(starts_with("prj_rating")) %>%
  corrr::correlate()


head(ess4_long)

ess4 <- 
  ess4 %>%
  select(idno,trd,cntry_lbl,cntry_cde,region,
         prj_immigrants,prj_gay,prj_unemployed,prj_women,prj_age20,prj_age70,
         prj_immigrants_resc,prj_gay_resc,prj_unemployed_resc,prj_women_resc,prj_age20_resc,prj_age70_resc) %>%
  mutate(
    region = as.factor(region),
    region = forcats::fct_relabel(
      region,
      janitor::make_clean_names
    ),
    region = case_when(region == "x" ~ NA,
                       TRUE ~ region),
    idno = paste0(cntry_cde,"_",idno)
  )

ess4 %>%
  select(-ends_with("resc"))

# ANALYZE ---- 

#* ICCs ----
#Traditionalism
mlm_trd_0 <- lmerTest::lmer(trd_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_trd_0)

#Anti-Gay
mlm_gay_0 <- lmerTest::lmer(anti_gay_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_gay_0)

#Anti-Immigrant
mlm_immigrant_0 <- lmerTest::lmer(anti_immigrant_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_immigrant_0)

#Unemployed
mlm_unemployed_0 <- lmerTest::lmer(unemployed_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_unemployed_0)

#Sexism
mlm_women_0 <- lmerTest::lmer(sexist_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_women_0)

#Ageism
mlm_20s_0 <- lmerTest::lmer(ppl_20s_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_20s_0)

mlm_70s_0 <- lmerTest::lmer(ppl_70s_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_70s_0)

#* MEDIAN SPLIT ----

ess4 <- 
  ess4 %>%
  mutate(lft_rght.gm = case_when(lrscale < 5 ~ "lft",
                             lrscale > 5 ~ "rght"),
         trd_hilo.gm = case_when(trd < median(trd,na.rm = T) ~ "low",
                              trd > median(trd,na.rm = T) ~ "high")) %>%
  group_by(cntry_lbl) %>%
  mutate(trd_cntry_mean         = mean(trd, na.rm = T),
         trd_cntry_median       = median(trd, na.rm = T),
         lftrght_cntry_mean     = mean(lrscale, na.rm = T)) %>%
  mutate(trd_hilo_cntry_mean    = case_when(trd < trd_cntry_mean         ~ "low",
                                            trd >= trd_cntry_mean        ~ "high"),
         trd_hilo_cntry_median  = case_when(trd < trd_cntry_median       ~ "low",
                                            trd >= trd_cntry_median      ~ "high"),
         lftrght_cntry          = case_when(lrscale < lftrght_cntry_mean ~ "lft",
                                            lrscale > lftrght_cntry_mean ~ "rght")) %>%
  ungroup()

# CORRELATIONS ----

## Traditionalism and Prejudice Targets ----

#correlation table
ess4 %>%
  select("trd","lrscale",all_of(target.itms_scl)) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant()

#correlation with CI and N
ess4 %>%
  select("trd","lrscale",all_of(target.itms_scl)) %>%
  correlation::correlation(., method = "pearson",missing = "keep_pairwise",p_adjust = "none") %>%
  as.data.frame() %>%
  mutate(p = p.adjust(p,"bonferroni"),
         p = case_when(
           p < 0.001 ~ "p < .001",
           p < 0.01  ~ "p < .01",
           p < 0.05  ~ "p < .05",
           TRUE      ~ as.character(round(p,3))
         ),
         "95% CI" = paste0("[",
                           format(round(CI_low,2),nsmall = 2),
                           ", ",
                           format(round(CI_high,2),nsmall = 2),
                           "]"),
         r = format(round(r,2),nsmall = 2),
         Parameter1 = stringr::str_to_upper(Parameter1),
         Parameter2 = stringr::str_remove_all(Parameter2,"prj_"),
         Parameter2 = stringr::str_replace_all(Parameter2,"_"," "),
         Parameter2 = stringr::str_to_title(Parameter2)
  ) %>%
  rename("Variable 1" = "Parameter1","Variable 2" = "Parameter2","N" = "n_Obs") %>%
  select('Variable 1','Variable 2', N, r , '95% CI',p)
  
## Country Median Split ----

### AVERAGE CORRELATIONS BY COUNTRY ----

#* within country ####
within_cntry <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  ungroup() %>%
  mutate(trd_hilo_cntry_median= factor(trd_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo_cntry_median, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  mutate(within_cntry = paste0(
    format(round(within_cntry,2),nsmall = 2),
    " [",
    format(round(CI95_low,2),nsmall = 2),
    ", ",
    format(round(CI95_upp,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Whole Sample Across Countries ####
btwn_cntry_whole <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_whole <- 
  btwn_cntry_whole %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(whole_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Low Sample Across Countries ####
btwn_cntry_low <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo_cntry_median != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_low <- 
  btwn_cntry_low %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(low_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Low Sample Across Countries ####
btwn_cntry_high <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo_cntry_median != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long format: one row per country
btwn_cntry_high <- 
  btwn_cntry_high %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(high_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")


#* Results table ####
within_cntry %>%
  full_join(.,btwn_cntry_whole, by = "cntry_lbl") %>%
  full_join(.,btwn_cntry_low, by = "cntry_lbl") %>%
  full_join(.,btwn_cntry_high, by = "cntry_lbl") 

### AVERAGE CORRELATIONS ACROSS ALL COUNTRIES ####

#* within country ####

# Step 1: Fisher-z-Transformation
z_withincntry <-
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  ungroup() %>%
  mutate(trd_hilo_cntry_median= factor(trd_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo_cntry_median, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  mutate(z = atanh(within_cntry)) %>%
  pull(z)


# Step 2: Mean and SE
z_withincntry_mean <- mean(z_withincntry, na.rm = TRUE)
z_withincntry_se   <- sd(z_withincntry, na.rm = TRUE) / sqrt(length(z_withincntry))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_withincntry <- z_withincntry_mean - z_crit * z_withincntry_se
ci_upper_z_withincntry <- z_withincntry_mean + z_crit * z_withincntry_se

# Step 4: Correlation from z to r
r_mean_withincntry <- tanh(z_withincntry_mean)
ci_lower_r_withincntry <- tanh(ci_lower_z_withincntry)
ci_upper_r_withincntry <- tanh(ci_upper_z_withincntry)

# Result
cat("Average correlation within country [95% CI]:",
    round(r_mean_withincntry,2),
    paste0("[",round(ci_lower_r_withincntry,2),", ",round(ci_upper_r_withincntry,2),"]"))

#* Between Country Whole Sample ####

z_whlsmpl <-
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE) %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_whlsmpl_mean <- mean(z_whlsmpl, na.rm = TRUE)
z_whlsmpl_se   <- sd(z_whlsmpl, na.rm = TRUE) / sqrt(length(z_whlsmpl))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_whlsmpl <- z_whlsmpl_mean - z_crit * z_whlsmpl_se
ci_upper_z_whlsmpl <- z_whlsmpl_mean + z_crit * z_whlsmpl_se

# Step 4: Correlation from z to r
r_mean_whlesmpl <- tanh(z_whlsmpl_mean)
ci_lower_r_whlesmpl  <- tanh(ci_lower_z_whlsmpl)
ci_upper_r_whlesmpl  <- tanh(ci_upper_z_whlsmpl)

# Result
cat("Average correlation r [95% CI] across countries without sample split:",
    round(r_mean_whlesmpl,2),
    paste0("[",round(ci_lower_r_whlesmpl,2),", ",round(ci_upper_r_whlesmpl,2),"]"))

#* Between Country Low Sample ####

z_lowsmpl <-
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo_cntry_median != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_lowsmpl_mean <- mean(z_lowsmpl, na.rm = TRUE)
z_lowsmpl_se   <- sd(z_lowsmpl, na.rm = TRUE) / sqrt(length(z_lowsmpl))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_lowsmpl <- z_lowsmpl_mean - z_crit * z_lowsmpl_se
ci_upper_z_lowsmpl <- z_lowsmpl_mean + z_crit * z_lowsmpl_se

# Step 4: Correlation from z to r
r_mean_lowsmpl <- tanh(z_lowsmpl_mean)
ci_lower_r_lowsmpl  <- tanh(ci_lower_z_lowsmpl)
ci_upper_r_lowsmpl  <- tanh(ci_upper_z_lowsmpl)

# Result
cat("Average correlation r [95% CI] across countries low traditionalism subsample:",
    round(r_mean_lowsmpl,2),
    paste0("[",round(ci_lower_r_lowsmpl,2),", ",round(ci_upper_r_lowsmpl,2),"]"))

#* Between Country High Sample ####

z_highsmpl <-
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo_cntry_median != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_highsmpl_mean <- mean(z_highsmpl, na.rm = TRUE)
z_highsmpl_se   <- sd(z_highsmpl, na.rm = TRUE) / sqrt(length(z_highsmpl))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_highsmpl <- z_highsmpl_mean - z_crit * z_highsmpl_se
ci_upper_z_highsmpl <- z_highsmpl_mean + z_crit * z_highsmpl_se

# Step 4: Correlation from z to r
r_mean_highsmpl <- tanh(z_highsmpl_mean)
ci_lower_r_highsmpl  <- tanh(ci_lower_z_highsmpl)
ci_upper_r_highsmpl  <- tanh(ci_upper_z_highsmpl)

# Result
cat("Average correlation r [95% CI] across countries high traditionalism subsample:",
    round(r_mean_highsmpl,2),
    paste0("[",round(ci_lower_r_highsmpl,2),", ",round(ci_upper_r_highsmpl,2),"]"))


#* Traditionalism Grand Median Split ----

### AVERAGE CORRELATIONS BY COUNTRY ----

#* within country ####
within_cntry.gm <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  ungroup() %>%
  mutate(trd_hilo.gm= factor(trd_hilo.gm, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo.gm, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  mutate(within_cntry = paste0(
    format(round(within_cntry,2),nsmall = 2),
    " [",
    format(round(CI95_low,2),nsmall = 2),
    ", ",
    format(round(CI95_upp,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Whole Sample Across Countries ####
btwn_cntry_whole.gm <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_whole.gm <- 
  btwn_cntry_whole.gm %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(whole_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Low Sample Across Countries ####
btwn_cntry_low.gm <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo.gm != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_low.gm <- 
  btwn_cntry_low.gm %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(low_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")

#* Low Sample Across Countries ####
btwn_cntry_high.gm <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo.gm != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long format: one row per country
btwn_cntry_high.gm <- 
  btwn_cntry_high.gm %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(high_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")


#* Results table ####
within_cntry.gm %>%
  full_join(.,btwn_cntry_whole.gm, by = "cntry_lbl") %>%
  full_join(.,btwn_cntry_low.gm, by = "cntry_lbl") %>%
  full_join(.,btwn_cntry_high.gm, by = "cntry_lbl") 

### AVERAGE CORRELATIONS ACROSS ALL COUNTRIES ####

#* within country ####

# Step 1: Fisher-z-Transformation
z_withincntry.gm <-
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  ungroup() %>%
  mutate(trd_hilo.gm= factor(trd_hilo.gm, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo.gm, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  mutate(z = atanh(within_cntry)) %>%
  pull(z)


# Step 2: Mean and SE
z_withincntry.gm_mean <- mean(z_withincntry.gm, na.rm = TRUE)
z_withincntry.gm_se   <- sd(z_withincntry.gm, na.rm = TRUE) / sqrt(length(z_withincntry.gm))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_withincntry.gm <- z_withincntry.gm_mean - z_crit * z_withincntry.gm_se
ci_upper_z_withincntry.gm <- z_withincntry.gm_mean + z_crit * z_withincntry.gm_se

# Step 4: Correlation from z to r
r_mean_withincntry.gm <- tanh(z_withincntry.gm_mean)
ci_lower_r_withincntry.gm <- tanh(ci_lower_z_withincntry.gm)
ci_upper_r_withincntry.gm <- tanh(ci_upper_z_withincntry.gm)

# Result
cat("Average correlation within country [95% CI]:",
    round(r_mean_withincntry.gm,2),
    paste0("[",round(ci_lower_r_withincntry.gm,2),", ",round(ci_upper_r_withincntry.gm,2),"]"))

#* Between Country Whole Sample ####

z_whlsmpl.gm <-
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE) %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_whlsmpl.gm_mean <- mean(z_whlsmpl.gm, na.rm = TRUE)
z_whlsmpl.gm_se   <- sd(z_whlsmpl.gm, na.rm = TRUE) / sqrt(length(z_whlsmpl.gm))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_whlsmpl.gm <- z_whlsmpl.gm_mean - z_crit * z_whlsmpl.gm_se
ci_upper_z_whlsmpl.gm <- z_whlsmpl.gm_mean + z_crit * z_whlsmpl.gm_se

# Step 4: Correlation from z to r
r_mean_whlesmpl.gm <- tanh(z_whlsmpl.gm_mean)
ci_lower_r_whlesmpl.gm  <- tanh(ci_lower_z_whlsmpl.gm)
ci_upper_r_whlesmpl.gm  <- tanh(ci_upper_z_whlsmpl.gm)

# Result
cat("Average correlation r [95% CI] across countries without sample split:",
    round(r_mean_whlesmpl.gm,2),
    paste0("[",round(ci_lower_r_whlesmpl.gm,2),", ",round(ci_upper_r_whlesmpl.gm,2),"]"))

#* Between Country Low Sample ####

z_lowsmpl.gm <-
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo.gm != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_lowsmpl.gm_mean <- mean(z_lowsmpl.gm, na.rm = TRUE)
z_lowsmpl.gm_se   <- sd(z_lowsmpl.gm, na.rm = TRUE) / sqrt(length(z_lowsmpl.gm))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_lowsmpl.gm <- z_lowsmpl.gm_mean - z_crit * z_lowsmpl.gm_se
ci_upper_z_lowsmpl.gm <- z_lowsmpl.gm_mean + z_crit * z_lowsmpl.gm_se

# Step 4: Correlation from z to r
r_mean_lowsmpl.gm <- tanh(z_lowsmpl.gm_mean)
ci_lower_r_lowsmpl.gm  <- tanh(ci_lower_z_lowsmpl.gm)
ci_upper_r_lowsmpl.gm  <- tanh(ci_upper_z_lowsmpl.gm)

# Result
cat("Average correlation r [95% CI] across countries low traditionalism subsample:",
    round(r_mean_lowsmpl.gm,2),
    paste0("[",round(ci_lower_r_lowsmpl.gm,2),", ",round(ci_upper_r_lowsmpl.gm,2),"]"))

#* Between Country High Sample ####

z_highsmpl.gm <-
  ess4 %>%
  select(cntry_lbl,trd_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo.gm) %>%
  summarise(across(anti_gay_scl:ppl_70s_scl, ~ mean(.x, na.rm = TRUE))) %>%
  filter(!is.na(trd_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo.gm != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  mutate(z = atanh(r)) %>%
  pull(z)

# Step 2: Mean and SE
z_highsmpl.gm_mean <- mean(z_highsmpl.gm, na.rm = TRUE)
z_highsmpl.gm_se   <- sd(z_highsmpl.gm, na.rm = TRUE) / sqrt(length(z_highsmpl.gm))

# Step 3: 95%CI (for z-transformed correlations)
z_crit <- qnorm(0.975)
ci_lower_z_highsmpl.gm <- z_highsmpl.gm_mean - z_crit * z_highsmpl.gm_se
ci_upper_z_highsmpl.gm <- z_highsmpl.gm_mean + z_crit * z_highsmpl.gm_se

# Step 4: Correlation from z to r
r_mean_highsmpl.gm <- tanh(z_highsmpl.gm_mean)
ci_lower_r_highsmpl.gm  <- tanh(ci_lower_z_highsmpl.gm)
ci_upper_r_highsmpl.gm  <- tanh(ci_upper_z_highsmpl.gm)

# Result
cat("Average correlation r [95% CI] across countries high traditionalism subsample:",
    round(r_mean_highsmpl.gm,2),
    paste0("[",round(ci_lower_r_highsmpl.gm,2),", ",round(ci_upper_r_highsmpl.gm,2),"]"))


# DESCRIPTIVES COUNTRY ----

ess4 %>%
  select(cntry_lbl,trd,all_of(target.itms)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(1:7, mean, na.rm = TRUE)) %>%
  rename("trd_Mean"                 = trd,
         "Anti-Gay"            = anti_gay,
         "Anti-Immigrant"      = anti_immigrant,
         "Unemployed"          = unemployed,
         "Sexist attitudes"    = sexist,
         "People in their 20s" = ppl_20s,
         "People in their 70s" = ppl_70s) %>%
  full_join(.,
            ess4 %>%
              select(cntry_lbl,trd) %>%
              group_by(cntry_lbl) %>%
              summarise(trd_Median = median(trd,na.rm = TRUE)),
            by = "cntry_lbl"
            ) 
ess4 %>%
  select(cntry_lbl,trd,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(1:7, mean, na.rm = TRUE)) %>%
  rename("trd"                 = trd,
         "Anti-Gay"            = anti_gay_scl,
         "Anti-Immigrant"      = anti_immigrant_scl,
         "Unemployed"          = unemployed_scl,
         "Sexist attitudes"    = sexist_scl,
         "People in their 20s" = ppl_20s_scl,
         "People in their 70s" = ppl_70s_scl) %>%
  full_join(.,
            ess4 %>%
              select(cntry_lbl,trd) %>%
              group_by(cntry_lbl) %>%
              summarise(trd_Median = median(trd,na.rm = TRUE)),
            by = "cntry_lbl"
  )

# DEMOGRAFICS BY COUNTRY ----

ess4 %>%
  select(cntry_lbl) %>%
  count(cntry_lbl) %>%
  full_join(.,
            ess4 %>%
              select(cntry_lbl,agea,lrscale) %>%
              group_by(cntry_lbl) %>%
              summarise_if(is.numeric,mean,na.rm = T),
            by = "cntry_lbl"
  ) %>%
  full_join(.,
            ess4 %>%
              mutate(cntry_lbl = forcats::fct_drop(cntry_lbl)) %>%
              janitor::tabyl(cntry_lbl,gndr) %>%
              janitor::adorn_percentages() %>%
              select(1:3),
            by = "cntry_lbl"
  ) %>%
  full_join(.,
            ess4 %>%
              mutate(cntry_lbl = forcats::fct_drop(cntry_lbl),
                     eisced = case_when(eisced == 1 ~ "es_isced_1",
                                        eisced == 2 ~ "es_isced_2",
                                        eisced == 3 ~ "es_isced_3",
                                        eisced == 4 ~ "es_isced_4",
                                        eisced == 5 ~ "es_isced_5",
                                        eisced == 6 ~ "es_isced_6",
                                        eisced == 7 ~ "es_isced_7",
                                        TRUE ~ NA)) %>%
              janitor::tabyl(cntry_lbl,eisced) %>%
              janitor::adorn_percentages() %>%
              mutate_if(is.numeric,round,2),
            by = "cntry_lbl"
  )
















































































ess4$prj_immigrants <- rowMeans(ess4[,c("prj_immi_1","prj_immi_2","prj_immi_3")],na.rm = TRUE)
ess4$prj_women      <- rowMeans(ess4[,c("prj_wmn_1","prj_wmn_2")],na.rm = TRUE)

ess4 %>%
  select(starts_with("prj_")&matches("_[0-9]$"))

prj_trgts <-
  ess4 %>%
  select(starts_with("prj_")& !(starts_with("prj_")&matches("_[0-9]$"))) %>%
  names()

ess4 <- 
  ess4 %>%
  mutate(across(all_of(prj_trgts),~as.numeric(scale(.)),.names = "{.col}_scl"))

prj_trgts <- 
  ess4 %>%
  select(ends_with("_scl")) %>%
  names()

within_cntry <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(prj_trgts)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(prj_trgts), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  ungroup() %>%
  mutate(trd_hilo_cntry_median= factor(trd_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo_cntry_median, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  mutate(within_cntry = paste0(
    format(round(within_cntry,2),nsmall = 2),
    " [",
    format(round(CI95_low,2),nsmall = 2),
    ", ",
    format(round(CI95_upp,2),nsmall = 2),
    "]"
  ),.keep = "unused")

ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(prj_trgts)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(prj_trgts), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  ungroup() %>%
  mutate(trd_hilo_cntry_median= factor(trd_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = trd_hilo_cntry_median, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor.test(high,low)$estimate,
            CI95_low = cor.test(high,low)$conf.int[1],
            CI95_upp = cor.test(high,low)$conf.int[2]) %>%
  pull(within_cntry) %>%mean()

#* Whole Sample Across Countries ####
btwn_cntry_whole <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(prj_trgts)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(all_of(prj_trgts)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(prj_trgts), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_whole <- 
  btwn_cntry_whole %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(whole_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")



#* Low Sample Across Countries ####
btwn_cntry_low <- 
  ess4 %>%
  select(cntry_lbl,trd_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,trd_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(trd_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(trd_hilo_cntry_median != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_low <- 
  btwn_cntry_low %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(low_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")













btwn_cntry_whole %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  pull(mean_r) %>% mean()
