rm(list = ls())

library(dplyr)

# get data ####

#wave 4 (2018)
ess4 <- haven::read_sav('./0_data/european_social_survey_wave_4_2008.sav')


ess4 <- 
  ess4 %>%
  select(
    #socio-demographics
    agea,    #Age of respondent, calculated
    gndr,    #Gender
    edulvla, #Highest level of education
    lrscale, #Placement on left right scale
    cntry_lbl,   #Country
    starts_with("regi"),  #Region
    #rwa
    ipfrule, #Important to do what is told and follow rules
    ipstrgv, #Important that government is strong and ensures safety
    ipbhprp, #Important to behave properly
    imptrad, #Important to follow traditions and customs
    impsafe, #Important to live in secure and safe surroundings
    hrshsnt, #People who break the law much harsher sentences
    #anti-immigrant
    imdfetn, #Allow many/few immigrants of different race/ethnic group from majority
    impcntr, #Allow many/few immigrants from poorer countries outside Europe
    imbgeco, #Immigration bad or good for country's economy
    imueclt, #Country's cultural life undermined or enriched by immigrants
    imwbcnt, #Immigrants make country worse or better place to live
    #anti-gay
    freehms, #Gays and lesbians free to live life as they wish
    #women
    wmcpwrk, #Women should be prepared to cut down on paid work for sake of family
    mnrgtjb, #Men should have more right to job than women when jobs are scarce
    #unemployed
    uentrjb, #Most unemployed people do not really try to find a job
    #ageism
    oafl20, #Overall how negative or positive feel towards people in their 20s
    oafl70  #Overall how negative or positive feel towards people over 70
  )  %>%
  mutate(region     = coalesce(!!!select(., starts_with("regi"))),
         cntry_lbl  = haven::as_factor(cntry_lbl,  levels = "label"),
         cntry_lbl  = haven::as_factor(cntry_lbl,  levels = "value"),
         region_lbl = haven::as_factor(region, levels = "label"),
         region_cde = haven::as_factor(region, levels = "value"),
         gndr       = haven::as_factor(gndr,   levels = "label"), .keep = "unused") %>%
  select(-starts_with("regi"),region_lbl,region_cde)

## Demo ----

ess4 %>%
  janitor::tabyl(gndr)



## RWA ----

rwa.itms     <- c("ipfrule","ipstrgv","ipbhprp","imptrad","impsafe","hrshsnt")
rwa.itms_scl <- c("ipfrule_scl","ipstrgv_scl","ipbhprp_scl","imptrad_scl","impsafe_scl","hrshsnt_scl")

ess4$ipfrule <- 7-ess4$ipfrule
ess4$ipstrgv <- 7-ess4$ipstrgv
ess4$ipbhprp <- 7-ess4$ipbhprp
ess4$imptrad <- 7-ess4$imptrad
ess4$impsafe <- 7-ess4$impsafe
ess4$hrshsnt <- 6-ess4$hrshsnt

ess4$ipfrule_scl <- scale(ess4$ipfrule)
ess4$ipstrgv_scl <- scale(ess4$ipstrgv)
ess4$ipbhprp_scl <- scale(ess4$ipbhprp)
ess4$imptrad_scl <- scale(ess4$imptrad)
ess4$impsafe_scl <- scale(ess4$impsafe)
ess4$hrshsnt_scl <- scale(ess4$hrshsnt)

rwa_fa <- factanal(na.omit(ess4[rwa.itms_scl]),
                   factors = 3, rotation = "varimax")
rwa_fa
loadings(rwa_fa)[] %>%
  data.frame() %>%
  rowwise() %>%
  mutate(across(everything(), ~ if_else(. == max(c_across(everything()), na.rm = TRUE), ., NA_real_)))


psych::alpha(ess4[rwa.itms])
psych::alpha(ess4[rwa.itms_scl])

ess4$rwa     <- rowMeans(ess4[rwa.itms],na.rm = T)
ess4$rwa_scl <- rowMeans(ess4[rwa.itms_scl],na.rm = T)

ess4 %>%
  group_by(cntry_lbl) %>%
  summarise(m = mean(rwa, na.rm = T),
            m_scl = mean(rwa_scl,na.rm = T)) %>%
  arrange(desc(m)) %>%
  print(n=30)


## Anti-Gay Attitudes ----

ess4 <- 
  ess4 %>%
  mutate(anti_gay     = freehms,
         anti_gay_scl = as.numeric(scale(freehms)),.keep = "unused")


## Anti-Immigrant Attitudes ----

### majority ---- 
mig_maj.itms     <- c("imdfetn","impcntr")
mig_maj.itms_scl <- c("imdfetn_scl","impcntr_scl")

ess4$imdfetn_scl <- scale(ess4$imdfetn)
ess4$impcntr_scl <- scale(ess4$impcntr)

psych::alpha(ess4[mig_maj.itms])
psych::alpha(ess4[mig_maj.itms_scl])

ess4$mig_maj     <- rowMeans(ess4[mig_maj.itms], na.rm = T)
ess4$mig_maj_scl <- rowMeans(ess4[mig_maj.itms_scl], na.rm = T)

### enriching ---
mig_enr.itms     <- c("imbgeco","imueclt","imwbcnt")
mig_enr.itms_scl <- c("imbgeco_scl","imueclt_scl","imwbcnt_scl")

ess4$imbgeco <- 11-ess4$imbgeco
ess4$imueclt <- 11-ess4$imueclt
ess4$imwbcnt <- 11-ess4$imwbcnt

ess4$imbgeco_scl <- scale(ess4$imbgeco)
ess4$imueclt_scl <- scale(ess4$imueclt)
ess4$imwbcnt_scl <- scale(ess4$imwbcnt)

psych::alpha(ess4[mig_enr.itms])
psych::alpha(ess4[mig_enr.itms_scl])

ess4$mig_enr     <- rowMeans(ess4[mig_enr.itms], na.rm = T)
ess4$mig_enr_scl <- rowMeans(ess4[mig_enr.itms_scl], na.rm = T)

anti_mig.itms     <- c("imbgeco","imueclt","imwbcnt","imdfetn","impcntr")
anti_mig.itms_scl <- c("imbgeco_scl","imueclt_scl","imwbcnt_scl","imdfetn_scl","impcntr_scl")


### aggregated ----
psych::alpha(ess4[anti_mig.itms])
psych::alpha(ess4[anti_mig.itms_scl])


ess4$anti_immigrant     <- rowMeans(ess4[anti_mig.itms],na.rm = T)
ess4$anti_immigrant_scl <- rowMeans(ess4[anti_mig.itms_scl],na.rm = T)


## Unemployed ----
ess4 <- 
  ess4 %>%
  mutate(unemployed     = uentrjb,
         unemployed_scl = as.numeric(scale(uentrjb)),.keep = "unused")


## Sexism ----
anti_wmn.itms     <- c("wmcpwrk","mnrgtjb")
anti_wmn.itms_scl <- c("wmcpwrk_scl","mnrgtjb_scl")


ess4$wmcpwrk <- 6-ess4$wmcpwrk
ess4$mnrgtjb <- 6-ess4$mnrgtjb

ess4$wmcpwrk_scl <- scale(ess4$wmcpwrk)
ess4$mnrgtjb_scl <- scale(ess4$mnrgtjb)

psych::alpha(ess4[anti_wmn.itms])
psych::alpha(ess4[anti_wmn.itms_scl])

ess4$sexist     <- rowMeans(ess4[anti_wmn.itms],na.rm = T)
ess4$sexist_scl <- rowMeans(ess4[anti_wmn.itms_scl],na.rm = T)

## Ageism ----

ess4 <- 
  ess4 %>%
  mutate(ppl_20s     = 11 - oafl20,
         ppl_70s     = 11 - oafl70,
         ppl_20s_scl = as.numeric(scale(ppl_20s)),
         ppl_70s_scl = as.numeric(scale(ppl_70s)), .keep = "unused")


target.itms     <- c("anti_gay","anti_immigrant","unemployed","sexist","ppl_20s","ppl_70s")
target.itms_scl <- c("anti_gay_scl","anti_immigrant_scl","unemployed_scl","sexist_scl","ppl_20s_scl","ppl_70s_scl")

# ANALYZE ---- 

## ICCs
#RWA
mlm_rwa_0 <- lmerTest::lmer(rwa_scl ~ (1|cntry_lbl), data = ess4)
performance::icc(mlm_rwa_0)

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

## Median Mean Splits ----

ess4 <- 
  ess4 %>%
  mutate(lftrght = case_when(lrscale < 5 ~ "lft",
                             lrscale > 5 ~ "rght"),
         rwa_hilo = case_when(rwa < median(rwa,na.rm = T) ~ "low",
                              rwa > median(rwa,na.rm = T) ~ "high")) %>%
  group_by(cntry_lbl) %>%
  mutate(rwa_cntry_mean         = mean(rwa, na.rm = T),
         rwa_cntry_median       = median(rwa, na.rm = T),
         lftrght_cntry_mean     = mean(lrscale, na.rm = T)) %>%
  mutate(rwa_hilo_cntry_mean    = case_when(rwa < rwa_cntry_mean         ~ "low",
                                            rwa >= rwa_cntry_mean        ~ "high"),
         rwa_hilo_cntry_median  = case_when(rwa < rwa_cntry_median       ~ "low",
                                            rwa >= rwa_cntry_median      ~ "high"),
         lftrght_cntry          = case_when(lrscale < lftrght_cntry_mean ~ "lft",
                                            lrscale > lftrght_cntry_mean ~ "rght")) %>%
  ungroup()

## Correlations

ess4 %>%
  select("rwa","lrscale",all_of(target.itms_scl)) %>%
  rstatix::cor_mat() %>%
  rstatix::pull_lower_triangle()
  

## Desc by country ----

ess4 %>%
  select(cntry_lbl,rwa,all_of(target.itms)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(1:7, mean, na.rm = TRUE)) %>%
  mutate(across(2:7, round, 2)) %>%
  rename("RWA"                 = rwa,
         "Anti-Gay"            = anti_gay,
         "Anti-Immigrant"      = anti_immigrant,
         "Unemployed"          = unemployed,
         "Sexist attitudes"    = sexist,
         "People in their 20s" = ppl_20s,
         "People in their 70s" = ppl_70s)


# CORRELATION TABLES ----

## Country Median Split ----

#within country
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo_cntry_median,anti_gay_scl,anti_immigrant_scl,unemployed_scl,sexist_scl,ppl_20s_scl,ppl_70s_scl) %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  ungroup() %>%
  mutate(rwa_hilo_cntry_median= factor(rwa_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo_cntry_median, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor(high,low)) %>%
  rename("Country"=cntry_lbl) %>%
  tibble::add_row(Country = 'Average', !!! colMeans(.[-1])) %>%
  print(n=30)


## Whole Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,anti_gay_scl,anti_immigrant_scl,unemployed_scl,sexist_scl,ppl_20s_scl,ppl_70s_scl) %>%
  group_by(cntry_lbl) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "cntry_lbl",
                      values_to = "Whole_Sample_btwn_cntry") %>%
  select(-term) %>%
  rename("Country"=cntry_lbl) %>%
  tibble::add_row(Country = 'Average', !!! colMeans(.[-1])) %>%
  full_join(tbl_corr,.,by = "Country")

## Low Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo_cntry_median,anti_gay_scl,anti_immigrant_scl,unemployed_scl,sexist_scl,ppl_20s_scl,ppl_70s_scl) %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  mutate(rwa_hilo_cntry_median = factor(rwa_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo_cntry_median, values_from = Rating) %>%
  select(!high) %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = low) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "Country",
                      values_to = "low_sample_btwn_cntry") %>%
  select(-term)  %>%
  tibble::add_row(Country = 'Average', !!! colMeans(.[-1])) %>%
  full_join(tbl_corr,.,by = "Country")

## High Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo_cntry_median,anti_gay_scl,anti_immigrant_scl,unemployed_scl,sexist_scl,ppl_20s_scl,ppl_70s_scl) %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  mutate(rwa_hilo_cntry_median = factor(rwa_hilo_cntry_median, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo_cntry_median, values_from = Rating) %>%
  select(!low) %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = high) %>%
  corrr::correlate(method = "pearson") %>% summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "Country",
                      values_to = "high_sample_btwn_cntry") %>%
  select(-term) %>%
  tibble::add_row(Country = 'Average', !!! colMeans(.[-1])) %>%
  full_join(tbl_corr,.,by = "Country")

## Grand Median Split ----

## within country
corr_df_gm <- 
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo) %>%
  summarise_at(all_of(target.itms_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo)) %>%
  ungroup() %>%
  mutate(rwa_hilo= factor(rwa_hilo, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = round(cor(high,low),2)) %>%
  tibble::add_row(cntry_lbl = 'Average', !!! colMeans(.[-1])) %>%
  print(n=30)

## Whole Sample Across Countries
corr_df_gm <- 
  ess4 %>%
  select(cntry_lbl,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "cntry_lbl",
                      values_to = "Whole_Sample_btwn_cntry") %>%
  select(-term) %>%
  tibble::add_row(cntry_lbl = 'Average', !!! colMeans(.[-1])) %>%
  full_join(corr_df_gm,.,by = "cntry_lbl")


## Low Sample Across Countries
corr_df_gm <-
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo)) %>%
  mutate(rwa_hilo = factor(rwa_hilo, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo, values_from = Rating) %>%
  select(!high) %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = low) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "cntry_lbl",
                      values_to = "low_sample_btwn_cntry") %>%
  select(-term)  %>%
  tibble::add_row(cntry_lbl = 'Average', !!! colMeans(.[-1])) %>%
  full_join(corr_df_gm,.,by = "cntry_lbl")

## High Sample Across Countries
corr_df_gm <-
  ess4 %>%
  select(cntry_lbl,rwa,rwa_hilo,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo)) %>%
  mutate(rwa_hilo = factor(rwa_hilo, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo, values_from = Rating) %>%
  select(!low) %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = high) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_all(mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 2:30, 
                      names_to  = "cntry_lbl",
                      values_to = "high_sample_btwn_cntry") %>%
  select(-term)  %>%
  tibble::add_row(cntry_lbl = 'Average', !!! colMeans(.[-1])) %>%
  full_join(corr_df_gm,.,by = "cntry_lbl")
