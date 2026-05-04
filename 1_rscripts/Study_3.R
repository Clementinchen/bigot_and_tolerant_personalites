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

#Authoritarianism
#ipfrule = Important to do what is told and follow rules
#ipstrgv = Important that government is strong and ensures safety
#ipbhprp = Important to behave properly
#imptrad = Important to follow traditions and customs
#impsafe = Important to live in secure and safe surroundings


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

#freehms  = Gays and lesbians free to live life as they wish (reversed)
#hmsacld  = Gay and lesbian couples right to adopt children (reversed)
#hmsfmlsh = Ashamed if close family member gay or lesbian

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

#imbgeco = Immigration bad or good for country's economy
#imueclt = Country's cultural life undermined or enriched by immigrants
#imwbcnt = Immigrants make country worse or better place to live


### anti_immigrant ----
anti_mig.itms     <- c("imbgeco","imueclt","imwbcnt")

psych::fa.parallel(ess9 %>%
                     select(all_of(anti_mig.itms)))

fa_anti_mig <- 
  psych::fa(ess9 %>%
              select(all_of(anti_mig.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")

print(fa_anti_mig$loadings, cutoff = 0.3)

psych::alpha(ess9[anti_mig.itms])

ess9$anti_mig     <- rowMeans(ess9[anti_mig.itms],na.rm = T)


## Context level Attitudes -----

ess9 <- 
  ess9 %>%
  group_by(cntry_lbl, region_id) %>%
  mutate(rgn_lvl_trad      = mean(trad, na.rm = T),
         cwc_trad          = trad - rgn_lvl_trad,
         rgn_lvl_gay_att  = mean(anti_gay, na.rm = T),
         cwc_anti_gay     = anti_gay  - rgn_lvl_gay_att,
         #rgn_lvl_mig_maj  = mean(mig_maj, na.rm = T),
         #cwc_mig_maj      = mig_maj  - rgn_lvl_mig_maj,
         #rgn_lvl_mig_enr  = mean(mig_enr, na.rm = T),
         #cwc_mig_enr      = mig_enr  - rgn_lvl_mig_enr,
         rgn_lvl_anti_mig = mean(anti_mig, na.rm = T),
         cwc_anti_mig     = anti_mig - rgn_lvl_anti_mig) %>%
  ungroup()%>%
  mutate(gmc_gay_att  = anti_gay  - mean(anti_gay, na.rm = T),
         #gmc_mig_maj  = mig_maj  - mean(mig_maj, na.rm = T),
         #gmc_mig_enr  = mig_enr  - mean(mig_enr, na.rm = T),
         gmc_anti_mig = anti_mig - mean(anti_mig, na.rm = T))%>%
  group_by(cntry_lbl) %>%
  mutate(cntry_lvl_anti_gay  = mean(anti_gay, na.rm = T),
         #cntry_lvl_mig_maj  = mean(mig_maj, na.rm = T),
         #cntry_lvl_mig_enr  = mean(mig_enr, na.rm = T),
         cntry_lvl_anti_mig = mean(anti_mig,na.rm = T),
         cntry_lvl_trad      = mean(trad, na.rm = T)) %>%
  ungroup()

# CENTERING ----
ess9 <- 
   ess9 %>%
   mutate(rgn_lvl_anti_mig.gmc = rgn_lvl_anti_mig - mean(anti_mig,na.rm = T),
          rgn_lvl_gay_att.gmc  = rgn_lvl_gay_att  - mean(anti_gay, na.rm = T))
 
# CORRELATIONS ####

ess9 %>%
  select(trad,lrscale,anti_gay,anti_mig,
         rgn_lvl_gay_att,rgn_lvl_anti_mig) %>%
  correlation::correlation(p_adjust = "bonferroni") %>%
  data.frame() %>%
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

# ICCs ----

null_trad <- lmerTest::lmer(trad ~ 1 + (1|region_id), data = ess9, REML = F)
summary(null_trad)
performance::icc(null_trad)

null_anti_gay <- lmerTest::lmer(anti_gay ~ 1 + (1|region_id), data = ess9, REML = F)
summary(null_anti_gay)
performance::icc(null_anti_gay)

null_mig <- lmerTest::lmer(anti_mig ~ 1 + (1|region_id), data = ess9, REML = F)
summary(null_mig)
performance::icc(null_mig)

# FINAL MODELS -----

## Anti-Gay Prejudice ----

mlm_anti_gay_5_no_cntrls <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig + #Individual level
                   rgn_lvl_anti_mig.gmc +       #Context level
                   (cwc_trad + cwc_anti_mig|region_id), #Random effects
                 data = ess9, REML = F)

summary(mlm_anti_gay_5_no_cntrls)

mlm_anti_gay_5_cntrls <- 
  lme4::lmer(anti_gay ~ 
               cwc_trad + cwc_anti_mig + #Individual level
               rgn_lvl_anti_mig.gmc +       #Context level
               (cwc_trad + cwc_anti_mig|region_id) + #Random effects
               lrscale + agea + gndr + eisced, #controls
             data = ess9, REML = F)

summary(mlm_anti_gay_5_cntrls)

sjPlot::tab_model(mlm_anti_gay_5_cntrls,mlm_anti_gay_5_no_cntrls,
                  show.est = F,
                  show.std = T,
                  show.intercept = F,
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = F,
                  show.ngroups = TRUE,
                  collapse.se = T
                  #terms = c("cwc_trad","cwc_anti_mig","rgn_lvl_anti_mig.gmc"),
                  #pred.labels = c("trad","Anti-Immigrant: Individual","Anti-Immigrant: Regional"),
                  #dv.labels = "Model: Anti-Gay Attitudes"
                  ,file = "./2_tables/25_03_06_anti_gay.html"
                  )

performance::check_collinearity(mlm_anti_gay_5_no_cntrls)
performance::check_collinearity(mlm_anti_gay_5_cntrls)

performance::check_convergence(mlm_anti_gay_5_no_cntrls)
performance::check_convergence(mlm_anti_gay_5_cntrls)

## Anti-Immigrant Prejudice ----
mlm_anti_mig_5_no_cntrls <- 
  lmerTest::lmer(anti_mig ~ 
                   cwc_trad + cwc_anti_gay + #Individual level
                   rgn_lvl_gay_att.gmc +        #Context level
                   (cwc_trad + cwc_anti_gay|region_id), #random effects
                 data = ess9, REML = F)

summary(mlm_anti_mig_5_no_cntrls)

mlm_anti_mig_5_cntrls <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_id) + #Random effects
               lrscale + agea + eisced + gndr, #controls
             data = ess9)

summary(mlm_anti_mig_5_cntrls)


sjPlot::tab_model(mlm_anti_mig_5_cntrls,mlm_anti_mig_5_no_cntrls,
                  show.intercept = FALSE,
                  show.est = F,
                  show.std = TRUE,
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = F,
                  show.ngroups = TRUE,
                  collapse.se = T,
                  #terms = c("cwc_trad",
                  #"cwc_anti_gay",
                  #"rgn_lvl_gay_att.gmc",
                  #"lrscale",
                  #"agea",
                  #"edu_num",
                  #"gndr"),
                  #pred.labels = c("trad",
                  #                "Anti-Gay: Individual",
                  #                "Anti-Gay: Regional",
                  #                "Political Self-Placement\n[0(left) - 10(right)]",
                  #                "Age",
                  #                "Education [1(low) - 7(high)]",
                  #                "Gender [Female]"),
                  dv.labels = "Model: Anti-Immigrant Attitudes"
                  ,file = "./2_tables/25_03_06_anti_mig.html"
                  )

performance::check_collinearity(mlm_anti_mig_5_no_cntrls)
performance::check_collinearity(mlm_anti_mig_5_cntrls)


performance::check_convergence(mlm_anti_mig_5_cntrls)
performance::check_convergence(mlm_anti_mig_5_no_cntrls)


# MODEL BUILDING ----

## ANti-Gay Prejudice ----

mlm_anti_gay_0 <- 
  lmerTest::lmer(anti_gay ~ 
                   (1|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_1 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad +
                   (1|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_2 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   (1|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_3 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_trad + cwc_anti_mig|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_4 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_trad + cwc_anti_mig|region_id) + #Random effects
                   lrscale + agea + eisced + gndr, #controls
                 data = ess9, REML = F)

sjPlot::tab_model(mlm_anti_gay_0,
                  mlm_anti_gay_1,
                  mlm_anti_gay_2,
                  mlm_anti_gay_3,
                  mlm_anti_gay_4,
                  show.ci = F,
                  show.std = T,
                  show.est = F,
                  show.se = T,
                  collapse.se = T,
                  p.style = "stars",
                  show.re.var = F,
                  file = "./2_tables/2025_03_06_model_building_anti_gay.html")


## Anti-Immigrant Prejudice ----

mlm_anti_mig_0 <- 
  lme4::lmer(anti_mig ~ 
               (1|region_id), #Random effects
             data = ess9, REML = F)

mlm_anti_mig_1 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + 
               (1|region_id),#Random effects
             data = ess9, REML = F)

mlm_anti_mig_2 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               (1|region_id), #Random effects
             data = ess9, REML = F)

mlm_anti_mig_3 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_id),#Random effects
             data = ess9, REML = F)

mlm_anti_mig_4 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_id) +#Random effects
               lrscale + agea + eisced + gndr, #controls
             data = ess9, REML = F)

sjPlot::tab_model(mlm_anti_mig_0,
                  mlm_anti_mig_1,
                  mlm_anti_mig_2,
                  mlm_anti_mig_3,
                  mlm_anti_mig_4,
                  show.ci = F,
                  show.std = T,
                  show.est = F,
                  show.se = T,
                  collapse.se = T,
                  p.style = "stars",
                  show.re.var = F,
                  file = "./2_tables/2025_03_06_model_building_anti_mig.html")


performance::check_collinearity(mlm_anti_mig_2)
performance::check_collinearity(mlm_anti_mig_3)
performance::check_collinearity(mlm_anti_mig_4)

performance::check_convergence(mlm_anti_mig_0)
performance::check_convergence(mlm_anti_mig_1)
performance::check_convergence(mlm_anti_mig_2)
performance::check_convergence(mlm_anti_mig_3)
performance::check_convergence(mlm_anti_mig_4)
