rm(list = ls())

library(dplyr)

# DATA WRANGLING ####

#wave 9 (2018)
ess9 <- haven::read_sav('./0_data/european_social_survey_wave_9_2018.sav')
cdbk_3 <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 4)


ess9 <- ess9 %>%
  janitor::clean_names()


#select wave 9

ess9 <- 
  ess9 %>%
  select(all_of(
    cdbk_3 %>% pull(variable_name))
  )


## Clean Variables ----

ess9 <- 
  ess9 %>%
  mutate(cntry_id   = haven::as_factor(cntry,  levels = "value"),
         region_id  = haven::as_factor(region, levels = "value"),
         cntry_lbl  = haven::as_factor(cntry,  levels = "label"),
         region_lbl = haven::as_factor(region, levels = "label"),
         gndr       = haven::as_factor(gndr,   levels = "label"),
         eisced     = as.numeric(haven::as_factor(eisced, levels = "value")),
         eisced     = case_when(eisced > 7 | eisced < 1 ~ NA,
                                TRUE ~ eisced),
         across(c(lrscale,
                  ipfrule,ipstrgv,ipbhprp,imptrad,impsafe,
                  freehms,hmsacld,hmsfmlsh,
                  imdfetn,impcntr,imbgeco,imwbcnt,imueclt), ~ as.numeric(.)),
         ipfrule  =  7 - ipfrule,
         ipstrgv  =  7 - ipstrgv,
         ipbhprp  =  7 - ipbhprp,
         imptrad  =  7 - imptrad,
         impsafe  =  7 - impsafe,
         hmsfmlsh =  6 - hmsfmlsh,
         imbgeco  = 11 - imbgeco,
         imueclt  = 11 - imueclt,
         imwbcnt  = 11 - imwbcnt,
         .keep = "unused")

# SCALES ----

## RWA ----

#Authoritarianism
#ipfrule = Important to do what is told and follow rules
#ipstrgv = Important that government is strong and ensures safety
#ipbhprp = Important to behave properly
#imptrad = Important to follow traditions and customs
#impsafe = Important to live in secure and safe surroundings

rwa.itms  <- c("ipfrule","ipstrgv","ipbhprp","imptrad","impsafe")


psych::fa.parallel(ess9 %>%
                     select(all_of(rwa.itms)))

fa_rwa <- psych::fa(ess9 %>%
                    select(all_of(rwa.itms)), 
                    nfactors = 1, 
                    rotate = "oblimin", 
                    fm = "ml")

print(fa_rwa$loadings, cutoff = 0.3)

psych::alpha(ess9[rwa.itms])

ess9$rwa     <- rowMeans(ess9[rwa.itms],na.rm = T)

## Anti-Gay Attitudes ----

#freehms  = Gays and lesbians free to live life as they wish (reversed)
#hmsacld  = Gay and lesbian couples right to adopt children (reversed)
#hmsfmlsh = Ashamed if close family member gay or lesbian

anti_gay.itms     <- c("freehms","hmsacld","hmsfmlsh")

psych::fa.parallel(ess9 %>%
                     select(all_of(anti_gay.itms)))

fa_anti_gay <- 
  psych::fa(ess9 %>%
              select(all_of(anti_gay.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")

print(fa_anti_gay$loadings, cutoff = 0.3)

psych::alpha(ess9[anti_gay.itms])

ess9$anti_gay     <- rowMeans(ess9[anti_gay.itms], na.rm = T)

## Anti-Immigrant Attitudes ----

#imdfetn = Allow many/few immigrants of different race/ethnic group from majority
#impcntr = Allow many/few immigrants from poorer countries outside Europe

#imbgeco = Immigration bad or good for country's economy
#imueclt = Country's cultural life undermined or enriched by immigrants
#imwbcnt = Immigrants make country worse or better place to live


### anti_immigrant ----
anti_mig.itms     <- c("imbgeco","imueclt","imwbcnt"
                       #,"imdfetn","impcntr"
                       )

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
  mutate(rgn_lvl_rwa      = mean(rwa, na.rm = T),
         cwc_rwa          = rwa - rgn_lvl_rwa,
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
         cntry_lvl_rwa      = mean(rwa, na.rm = T)) %>%
  ungroup()

# CENTERING ----
ess9 <- 
   ess9 %>%
   mutate(rgn_lvl_anti_mig.gmc = rgn_lvl_anti_mig - mean(anti_mig,na.rm = T),
          rgn_lvl_gay_att.gmc  = rgn_lvl_gay_att  - mean(anti_gay, na.rm = T))
 

# ICCs ----

null_rwa <- lmerTest::lmer(rwa ~ 1 + (1|region_id), data = ess9, REML = F)
summary(null_rwa)
performance::icc(null_rwa)

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
                   cwc_rwa + cwc_anti_mig + #Individual level
                   rgn_lvl_anti_mig.gmc +       #Context level
                   (cwc_rwa + cwc_anti_mig|region_id), #Random effects
                 data = ess9, REML = F)

summary(mlm_anti_gay_5_no_cntrls)

mlm_anti_gay_5_cntrls <- 
  lme4::lmer(anti_gay ~ 
               cwc_rwa + cwc_anti_mig + #Individual level
               rgn_lvl_anti_mig.gmc +       #Context level
               (cwc_rwa + cwc_anti_mig|region_id) + #Random effects
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
                  #terms = c("cwc_rwa","cwc_anti_mig","rgn_lvl_anti_mig.gmc"),
                  #pred.labels = c("RWA","Anti-Immigrant: Individual","Anti-Immigrant: Regional"),
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
                   cwc_rwa + cwc_anti_gay + #Individual level
                   rgn_lvl_gay_att.gmc +        #Context level
                   (cwc_rwa + cwc_anti_gay|region_id), #random effects
                 data = ess9, REML = F)

summary(mlm_anti_mig_5_no_cntrls)

mlm_anti_mig_5_cntrls <- 
  lme4::lmer(anti_mig ~ 
               cwc_rwa + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_rwa + cwc_anti_gay|region_id) + #Random effects
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
                  #terms = c("cwc_rwa",
                  #"cwc_anti_gay",
                  #"rgn_lvl_gay_att.gmc",
                  #"lrscale",
                  #"agea",
                  #"edu_num",
                  #"gndr"),
                  #pred.labels = c("RWA",
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
                   cwc_rwa +
                   (1|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_2 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_rwa + cwc_anti_mig +
                   (1|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_3 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_rwa + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_rwa + cwc_anti_mig|region_id), #Random effects
                 data = ess9, REML = F)

mlm_anti_gay_4 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_rwa + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_rwa + cwc_anti_mig|region_id) + #Random effects
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
               cwc_rwa + 
               (1|region_id),#Random effects
             data = ess9, REML = F)

mlm_anti_mig_2 <- 
  lme4::lmer(anti_mig ~ 
               cwc_rwa + cwc_anti_gay + #Individual level
               (1|region_id), #Random effects
             data = ess9, REML = F)

mlm_anti_mig_3 <- 
  lme4::lmer(anti_mig ~ 
               cwc_rwa + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_rwa + cwc_anti_gay|region_id),#Random effects
             data = ess9, REML = F)

mlm_anti_mig_4 <- 
  lme4::lmer(anti_mig ~ 
               cwc_rwa + cwc_anti_gay + #Individual level
               rgn_lvl_gay_att.gmc +        #Context level
               (cwc_rwa + cwc_anti_gay|region_id) +#Random effects
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
