library(easystats)
library(sjPlot)
library(tidyverse)

# CORRELATIONS ####

ess9_ana %>%
  select(trad,lrscale,anti_gay,anti_mig,
         rgn_lvl_anti_gay,rgn_lvl_anti_mig) %>%
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
         Parameter1 = stringr::str_replace_all(Parameter1,"_"," "),
         Parameter1 = stringr::str_to_upper(Parameter1),
         Parameter2 = stringr::str_replace_all(Parameter2,"_"," "),
         Parameter2 = stringr::str_to_upper(Parameter2)
  ) %>%
  rename("Variable 1" = "Parameter1","Variable 2" = "Parameter2","N" = "n_Obs") %>%
  select('Variable 1','Variable 2', N, r , '95% CI',p) %>%
  print() %>%
  tab_df()

# ICCs ----

null_trad <- lmerTest::lmer(trad ~ 1 + (1|region_cde), data = ess9_ana, REML = F)
summary(null_trad)
performance::icc(null_trad)

null_anti_gay <- lmerTest::lmer(anti_gay ~ 1 + (1|region_cde), data = ess9_ana, REML = F)
summary(null_anti_gay)
performance::icc(null_anti_gay)

null_mig <- lmerTest::lmer(anti_mig ~ 1 + (1|region_cde), data = ess9_ana, REML = F)
summary(null_mig)
performance::icc(null_mig)

# FINAL MODELS -----

## Anti-Gay Prejudice ----

mlm_anti_gay_5_no_cntrls <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig + #Individual level
                   rgn_lvl_anti_mig.gmc +       #Context level
                   (cwc_trad + cwc_anti_mig|region_cde), #Random effects
                 data = ess9_ana, REML = F)

summary(mlm_anti_gay_5_no_cntrls)

mlm_anti_gay_5_cntrls <- 
  lme4::lmer(anti_gay ~ 
               cwc_trad + cwc_anti_mig + #Individual level
               rgn_lvl_anti_mig.gmc +       #Context level
               (cwc_trad + cwc_anti_mig|region_cde) + #Random effects
               lrscale + age + gndr + education_eisced, #controls
             data = ess9_ana, REML = F)

summary(mlm_anti_gay_5_cntrls)

sjPlot::tab_model(mlm_anti_gay_5_cntrls,mlm_anti_gay_5_no_cntrls,
                  show.est = F,
                  show.p = T,
                  show.std = T,
                  show.intercept = F,
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = F,
                  show.ngroups = TRUE,
                  collapse.se = T
                  #,terms = c("cwc_trad","cwc_anti_mig","rgn_lvl_anti_mig.gmc")
                  #,pred.labels = c("Traditionalism","Anti-Immigrant: Individual","Anti-Immigrant: Regional")
                  #,dv.labels = "Model: Anti-Gay Attitudes"
                  #,file = "./2_tables/25_03_06_anti_gay.html"
)

performance::check_collinearity(mlm_anti_gay_5_no_cntrls)
performance::check_collinearity(mlm_anti_gay_5_cntrls)

performance::check_convergence(mlm_anti_gay_5_no_cntrls)
performance::check_convergence(mlm_anti_gay_5_cntrls)

## Anti-Immigrant Prejudice ----
mlm_anti_mig_5_no_cntrls <- 
  lmerTest::lmer(anti_mig ~ 
                   cwc_trad + cwc_anti_gay + #Individual level
                   rgn_lvl_anti_gay.gmc +        #Context level
                   (cwc_trad + cwc_anti_gay|region_cde), #random effects
                 data = ess9_ana, REML = F)

summary(mlm_anti_mig_5_no_cntrls)

mlm_anti_mig_5_cntrls <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_anti_gay.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_cde) + #Random effects
               lrscale + age + education_eisced + gndr, #controls
             data = ess9_ana)

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
                  #"rgn_lvl_anti_gay.gmc",
                  #"lrscale",
                  #"age",
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
                  #,file = "./2_tables/25_03_06_anti_mig.html"
)

performance::check_collinearity(mlm_anti_mig_5_no_cntrls)
performance::check_collinearity(mlm_anti_mig_5_cntrls)


performance::check_convergence(mlm_anti_mig_5_cntrls)
performance::check_convergence(mlm_anti_mig_5_no_cntrls)


# MODEL BUILDING ----

## ANti-Gay Prejudice ----

mlm_anti_gay_0 <- 
  lmerTest::lmer(anti_gay ~ 
                   (1|region_cde), #Random effects
                 data = ess9_ana, REML = F)

mlm_anti_gay_1 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad +
                   (1|region_cde), #Random effects
                 data = ess9_ana, REML = F)

mlm_anti_gay_2 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   (1|region_cde), #Random effects
                 data = ess9_ana, REML = F)

mlm_anti_gay_3 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_trad + cwc_anti_mig|region_cde), #Random effects
                 data = ess9_ana, REML = F)

mlm_anti_gay_4 <- 
  lmerTest::lmer(anti_gay ~ 
                   cwc_trad + cwc_anti_mig +
                   rgn_lvl_anti_mig.gmc + 
                   (cwc_trad + cwc_anti_mig|region_cde) + #Random effects
                   lrscale + age + education_eisced + gndr, #controls
                 data = ess9_ana, REML = F)

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
               (1|region_cde), #Random effects
             data = ess9_ana, REML = F)

mlm_anti_mig_1 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + 
               (1|region_cde),#Random effects
             data = ess9_ana, REML = F)

mlm_anti_mig_2 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               (1|region_cde), #Random effects
             data = ess9_ana, REML = F)

mlm_anti_mig_3 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_anti_gay.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_cde),#Random effects
             data = ess9_ana, REML = F)

mlm_anti_mig_4 <- 
  lme4::lmer(anti_mig ~ 
               cwc_trad + cwc_anti_gay + #Individual level
               rgn_lvl_anti_gay.gmc +        #Context level
               (cwc_trad + cwc_anti_gay|region_cde) +#Random effects
               lrscale + age + education_eisced + gndr, #controls
             data = ess9_ana, REML = F)

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
