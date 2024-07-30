rm(list = ls())

# libraries ####
library(dplyr)

# get data ####

#wave 9 (2018)
ess9 <- haven::read_sav('./0_data/european_social_survey_wave_9_2018.sav')

ess9 <- ess9 %>%
  janitor::clean_names()

#select wave 9
ess9 <-
  ess9 %>%
  select(
    #socio-demographics
    agea,    #Age of respondent, calculated
    gndr,    #Gender
    edulvlb, #Highest level of education
    lrscale, #Placement on left right scale
    atchctr, #How emotionally attached to [country]
    cntry,   #Country
    region,  #Region
    #RWA
    ipfrule, #Important to do what is told and follow rules
    ipstrgv, #Important that government is strong and ensures safety
    ipbhprp, #Important to behave properly
    imptrad, #Important to follow traditions and customs
    impsafe, #Important to live in secure and safe surroundings
    #Anti-Gay
    freehms, #Gays and lesbians free to live life as they wish
    hmsacld, #Gay and lesbian couples right to adopt children
    hmsfmlsh,#Ashamed if close family member gay or lesbian
    #Anti-Immigrant
    imdfetn, #Allow many/few immigrants of different race/ethnic group from majority
    impcntr, #Allow many/few immigrants from poorer countries outside Europe
    imbgeco, #Immigration bad or good for country's economy
    imwbcnt, #Immigrants make country worse or better place to live
    imueclt  #Country's cultural life undermined or enriched by immigrants
  )


## Region and Country ----

ess9 <- 
  ess9 %>%
  mutate(cntry_id  = haven::as_factor(ess9$cntry,  levels = "value"),
         region_id = haven::as_factor(ess9$region, levels = "value"),
         cntry_lbl = haven::as_factor(ess9$cntry,  levels = "label"),
         region_lbl= haven::as_factor(ess9$region, levels = "label"),
         .keep = "unused")
## Demographics ----

#ctzcntr  = Citizen of country
#ctzshipd = Citizenship
#cntbrthd = Country of birth
#blgetmg  = Belong to minority ethnic group in country
#gndr     = Gender
#agea     = Age

psych::describe(ess9$agea)
## gender
ess9$gndr     <- haven::as_factor(ess9$gndr,  levels = "label")


#cntry    = country
#region   = region


ess9 %>%
  select(region_lbl) %>%
  filter(complete.cases(.)) %>%
  group_by(region_lbl) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(mean = mean(n,na.rm = T),
            sd = sd(n, na.rm = T),
            min = min(n),
            max = max(n))


## RWA ----

#Authoritarianism
#ipfrule = Important to do what is told and follow rules
#ipstrgv = Important that government is strong and ensures safety
#ipbhprp = Important to behave properly
#imptrad = Important to follow traditions and customs
#impsafe = Important to live in secure and safe surroundings

ess9$ipfrule <- 7-ess9$ipfrule
ess9$ipstrgv <- 7-ess9$ipstrgv
ess9$ipbhprp <- 7-ess9$ipbhprp
ess9$imptrad <- 7-ess9$imptrad
ess9$impsafe <- 7-ess9$impsafe

ess9$ipfrule_scl <- scale(ess9$ipfrule)
ess9$ipstrgv_scl <- scale(ess9$ipstrgv)
ess9$ipbhprp_scl <- scale(ess9$ipbhprp)
ess9$imptrad_scl <- scale(ess9$imptrad)
ess9$impsafe_scl <- scale(ess9$impsafe)

rwa.itms      <- c("ipfrule","ipstrgv","ipbhprp","imptrad","impsafe")
rwa.itms_scl  <- c("ipfrule_scl","ipstrgv_scl","ipbhprp_scl","imptrad_scl","impsafe_scl")


rwa_pca <- cor(ess9[rwa.itms],use = "pairwise")
rwa_pca <- princomp(covmat = rwa_pca)
plot(rwa_pca, type = "lines", main = "PCA RWA Items")

rwa_fa <-factanal(na.omit(ess9[rwa.itms]),
                  factors = 1, rotation = "varimax")
rwa_fa


psych::alpha(ess9[rwa.itms])
psych::alpha(ess9[rwa.itms_scl])


ess9$rwa     <- rowMeans(ess9[rwa.itms],na.rm = T)
ess9$rwa_scl <- rowMeans(ess9[rwa.itms_scl],na.rm = T)

## Anti-Gay Attitudes ----

#freehms  = Gays and lesbians free to live life as they wish (reversed)
#hmsacld  = Gay and lesbian couples right to adopt children (reversed)
#hmsfmlsh = Ashamed if close family member gay or lesbian

ess9$hmsfmlsh     <- 6-ess9$hmsfmlsh

ess9$freehms_scl  <- scale(ess9$freehms)
ess9$hmsacld_scl  <- scale(ess9$hmsacld)
ess9$hmsfmlsh_scl <- scale(ess9$hmsfmlsh)

anti_gay.itms     <- c("freehms","hmsacld","hmsfmlsh")
anti_gay.itms_scl <- c("freehms_scl","hmsacld_scl","hmsfmlsh_scl")

anti_gay_pca <- cor(ess9[anti_gay.itms],use = "pairwise")
anti_gay_pca <- princomp(covmat = anti_gay_pca)
plot(anti_gay_pca, type = "lines", main = "PCA Anti-Gay Items")

anti_gay_fa <-factanal(na.omit(ess9[anti_gay.itms]),
                  factors = 1, rotation = "varimax")
anti_gay_fa

psych::alpha(ess9[anti_gay.itms])
psych::alpha(ess9[anti_gay.itms_scl])

ess9$anti_gay     <- rowMeans(ess9[anti_gay.itms], na.rm = T)
ess9$anti_gay_scl <- rowMeans(ess9[anti_gay.itms_scl], na.rm = T)

## Anti-Immigrant Attitudes ----

#imdfetn = Allow many/few immigrants of different race/ethnic group from majority
#impcntr = Allow many/few immigrants from poorer countries outside Europe

#imbgeco = Immigration bad or good for country's economy
#imueclt = Country's cultural life undermined or enriched by immigrants
#imwbcnt = Immigrants make country worse or better place to live

### majority ---
mig_maj.itms     <- c("imdfetn","impcntr")
mig_maj.itms_scl <- c("imdfetn_scl","impcntr_scl")

ess9$imdfetn_scl  <- scale(ess9$imdfetn)
ess9$impcntr_scl  <- scale(ess9$impcntr)

psych::alpha(ess9[mig_maj.itms])
psych::alpha(ess9[mig_maj.itms_scl])

ess9$mig_maj     <- rowMeans(ess9[mig_maj.itms], na.rm = T)
ess9$mig_maj_scl <- rowMeans(ess9[mig_maj.itms_scl], na.rm = T)


### enriching ----
mig_enr.itms     <- c("imbgeco","imueclt","imwbcnt")
mig_enr.itms_scl <- c("imbgeco_scl","imueclt_scl","imwbcnt_scl")

ess9$imbgeco <- 11-ess9$imbgeco
ess9$imueclt <- 11-ess9$imueclt
ess9$imwbcnt <- 11-ess9$imwbcnt

ess9$imbgeco_scl  <- scale(ess9$imbgeco)
ess9$imueclt_scl  <- scale(ess9$imueclt)
ess9$imwbcnt_scl  <- scale(ess9$imwbcnt)

psych::alpha(ess9[mig_enr.itms])
psych::alpha(ess9[mig_enr.itms_scl])

ess9$mig_enr     <- rowMeans(ess9[mig_enr.itms], na.rm = T)
ess9$mig_enr_scl <- rowMeans(ess9[mig_enr.itms_scl], na.rm = T)


### anti_immigrant ----
anti_mig.itms     <- c("imdfetn","impcntr","imbgeco","imueclt","imwbcnt")
anti_mig.itms_scl <- c("imdfetn_scl","impcntr_scl","imbgeco_scl","imueclt_scl","imwbcnt_scl")

anti_mig_pca <- cor(ess9[anti_mig.itms],use = "pairwise")
anti_mig_pca <- princomp(covmat = anti_mig_pca)
plot(anti_mig_pca, type = "lines", main = "PCA Anti-Immigrant Items")

anti_mig_fa <- factanal(na.omit(ess9[anti_mig.itms]),
                        factors = 1, rotation = "varimax")
anti_mig_fa


psych::alpha(ess9[anti_mig.itms],na.rm = T)
psych::alpha(ess9[anti_mig.itms_scl],na.rm = T)

ess9$anti_mig     <- rowMeans(ess9[anti_mig.itms],na.rm = T)
ess9$anti_mig_scl <- rowMeans(ess9[anti_mig.itms_scl],na.rm = T)

## left-right scale ####

ess9$lrscale_scl <- scale(ess9$lrscale)

## Context level Attitudes -----

ess9 <- 
  ess9 %>%
  group_by(cntry, region) %>%
  mutate(rgn_lvl_rwa      = mean(rwa, na.rm = T),
         cwc_rwa          = rwa - rgn_lvl_rwa,
         rgn_lvl_gay_att  = mean(anti_gay, na.rm = T),
         cwc_anti_gay     = anti_gay  - rgn_lvl_gay_att,
         rgn_lvl_mig_maj  = mean(mig_maj, na.rm = T),
         cwc_mig_maj      = mig_maj  - rgn_lvl_mig_maj,
         rgn_lvl_mig_enr  = mean(mig_enr, na.rm = T),
         cwc_mig_enr      = mig_enr  - rgn_lvl_mig_enr,
         rgn_lvl_anti_mig = mean(anti_mig, na.rm = T),
         cwc_anti_mig     = anti_mig - rgn_lvl_anti_mig) %>%
  ungroup()%>%
  mutate(gmc_gay_att  = anti_gay  - mean(anti_gay, na.rm = T),
         gmc_mig_maj  = mig_maj  - mean(mig_maj, na.rm = T),
         gmc_mig_enr  = mig_enr  - mean(mig_enr, na.rm = T),
         gmc_anti_mig = anti_mig - mean(anti_mig, na.rm = T))%>%
  group_by(cntry) %>%
  mutate(cntry_lvl_anti_gay  = mean(anti_gay, na.rm = T),
         cntry_lvl_mig_maj  = mean(mig_maj, na.rm = T),
         cntry_lvl_mig_enr  = mean(mig_enr, na.rm = T),
         cntry_lvl_anti_mig = mean(anti_mig,na.rm = T),
         cntry_lvl_rwa      = mean(rwa, na.rm = T)) %>%
  ungroup()

# CENTERING ----

ess9 <- 
  ess9 %>%
  group_by(cntry, region) %>%
  mutate(rgn_lvl_rwa_scl      = mean(rwa_scl, na.rm = T),
         cwc_rwa_scl          = rwa_scl - rgn_lvl_rwa_scl,
         rgn_lvl_gay_att_scl  = mean(anti_gay_scl, na.rm = T),
         cwc_anti_gay_scl      = anti_gay_scl  - rgn_lvl_gay_att_scl,
         rgn_lvl_mig_maj_scl  = mean(mig_maj_scl, na.rm = T),
         cwc_mig_maj_scl      = mig_maj_scl  - rgn_lvl_mig_maj_scl,
         rgn_lvl_mig_enr_scl  = mean(mig_enr_scl, na.rm = T),
         cwc_mig_enr_scl      = mig_enr_scl  - rgn_lvl_mig_enr_scl,
         rgn_lvl_anti_mig_scl = mean(anti_mig_scl, na.rm = T),
         cwc_anti_mig_scl     = anti_mig_scl - rgn_lvl_anti_mig_scl) %>%
  ungroup()

# MLM ----

## ICCs ----
null_rwa <- lmerTest::lmer(rwa ~ 1 + (1|region), data = ess9, REML = F)
summary(null_rwa)
performance::icc(null_rwa)

null_anti_gay <- lmerTest::lmer(anti_gay_scl ~ 1 + (1|region), data = ess9, REML = F)
summary(null_anti_gay)
performance::icc(null_anti_gay)

null_mig <- lmerTest::lmer(anti_mig_scl ~ 1 + (1|region), data = ess9, REML = F)
summary(null_mig)
performance::icc(null_mig)

## Model Anti-Immigrant Attitudes ----

##multilevel

null_anti_mig <- lmerTest::lmer(anti_mig_scl ~ (1|region), data = ess9, REML = F)
summary(null_anti_mig)
performance::icc(null_anti_mig)

mlm_anti_mig_1 <- lmerTest::lmer(anti_mig_scl ~ cwc_rwa + (1|region), data = ess9, REML = F)
summary(mlm_anti_mig_1)

mlm_anti_mig_2 <- lmerTest::lmer(anti_mig_scl ~ cwc_rwa + (cwc_rwa|region), data = ess9, REML = F)
summary(mlm_anti_mig_2)

anova(mlm_anti_mig_1,mlm_anti_mig_2)

mlm_anti_mig_3 <- lmerTest::lmer(anti_mig_scl ~ cwc_rwa + cwc_anti_gay + (1|region), data = ess9, REML = F)
summary(mlm_anti_mig_3)

mlm_anti_mig_4 <- lmerTest::lmer(anti_mig_scl ~ cwc_rwa + cwc_anti_gay + (cwc_rwa + cwc_anti_gay|region), data = ess9, REML = F)
summary(mlm_anti_mig_4)

anova(mlm_anti_mig_3,mlm_anti_mig_4)

### FINAL MODELS -----

mlm_anti_mig_5_no_cntrls <- 
  lmerTest::lmer(anti_mig_scl ~ 
                   cwc_rwa + cwc_anti_gay + #Individual level
                   rgn_lvl_gay_att +        #Context level
                   (cwc_rwa + cwc_anti_gay|region), #random effects
                 data = ess9, REML = F)

summary(mlm_anti_mig_5_no_cntrls)

sjPlot::tab_model(mlm_anti_mig_5_no_cntrls)


mlm_anti_mig_5_cntrls <- 
  lmerTest::lmer(anti_mig_scl ~ 
                   cwc_rwa + cwc_anti_gay + #Individual level
                   rgn_lvl_gay_att +        #Context level
                   (cwc_rwa + cwc_anti_gay|region) + #Random effects
                   (1|lrscale) + (1|agea) + (1|edulvlb) + gndr, #controls
                 data = ess9, REML = F)

summary(mlm_anti_mig_5_cntrls)
performance::icc(mlm_anti_mig_5_cntrls)

sjPlot::tab_model(mlm_anti_mig_5_cntrls, 
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = TRUE,
                  show.ngroups = TRUE)

## Model Anti-Gay Attitudes ---- 

null_anti_gay <- lmerTest::lmer(anti_gay_scl ~ (1|region), data = ess9, REML = F)
summary(null_anti_gay)
performance::icc(null_anti_gay)

mlm_anti_gay_1 <- lmerTest::lmer(anti_gay_scl ~ cwc_rwa + (1|region), data = ess9, REML = F)
summary(mlm_anti_gay_1)

mlm_anti_gay_2 <- lmerTest::lmer(anti_gay_scl ~ cwc_rwa + (cwc_rwa|region), data = ess9, REML = F)
summary(mlm_anti_gay_2)

anova(mlm_anti_gay_1,mlm_anti_gay_2)

mlm_anti_gay_3 <- lmerTest::lmer(anti_gay_scl ~ cwc_rwa + cwc_anti_mig + (1|region), data = ess9, REML = F)
summary(mlm_anti_gay_3)

mlm_anti_gay_4 <- lmerTest::lmer(anti_gay_scl ~ cwc_rwa + cwc_anti_mig + (cwc_rwa + cwc_anti_mig|region), data = ess9, REML = F)
summary(mlm_anti_gay_4)

anova(mlm_anti_gay_3,mlm_anti_gay_4)

### FINAL MODELS -----

mlm_anti_gay_5_no_cntrls <- 
  lmerTest::lmer(anti_gay_scl ~ 
                   cwc_rwa + cwc_anti_mig + #Individual level
                   rgn_lvl_anti_mig +       #Context level
                   (cwc_rwa + cwc_anti_mig|region), #Random effects
                 data = ess9, REML = F)

summary(mlm_anti_gay_5_no_cntrls)
sjPlot::tab_model(mlm_anti_gay_5_no_cntrls, 
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = TRUE,
                  show.ngroups = TRUE)


mlm_anti_gay_5_cntrls <- lmerTest::lmer(anti_gay_scl ~ 
                                          cwc_rwa + cwc_anti_mig + #Individual level
                                          rgn_lvl_anti_mig +       #Context level
                                          (cwc_rwa + cwc_anti_mig|region) + #Random effects
                                          (1|lrscale) + (1|agea) + (1|edulvlb) + gndr, #Controls
                                        data = ess9, REML = F)
summary(mlm_anti_gay_5_cntrls)
sjPlot::tab_model(mlm_anti_gay_5_cntrls, 
                  show.se = TRUE,
                  show.icc = FALSE,
                  show.re.var = TRUE,
                  show.ngroups = TRUE)
