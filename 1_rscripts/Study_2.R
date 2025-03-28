rm(list = ls())

library(dplyr)

# DATA WRANGLING ####

#wave 4 (2018)
ess4 <- haven::read_sav('./0_data/european_social_survey_wave_4_2008.sav')
cdbk_2 <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 3)


ess4 <- 
  ess4 %>%
  select(matches(cdbk_2$variable_name),-matches("^gndr([1-9]|1[0-6])$"))  %>%
  mutate_at(vars(contains("regi")),~haven::as_factor(.,levels = "label")) %>%
  tidyr::unite("region",starts_with("regio"),na.rm = TRUE) %>%
  mutate(cntry_lbl = haven::as_factor(cntry,  levels = "label"),
         cntry_cde  = haven::as_factor(cntry,  levels = "value"),
         gndr       = haven::as_factor(gndr,   levels = "label"), .keep = "unused")


# SCALES ----

## RWA ----

rwa.itms     <- c("ipfrule","ipstrgv","ipbhprp","imptrad","impsafe","hrshsnt")
rwa.itms_scl <- c("ipfrule_scl","ipstrgv_scl","ipbhprp_scl","imptrad_scl","impsafe_scl","hrshsnt_scl")

ess4$ipfrule <- 7-ess4$ipfrule
ess4$ipstrgv <- 7-ess4$ipstrgv
ess4$ipbhprp <- 7-ess4$ipbhprp
ess4$imptrad <- 7-ess4$imptrad
ess4$impsafe <- 7-ess4$impsafe
ess4$hrshsnt <- 6-ess4$hrshsnt

ess4 <- 
  ess4 %>%
  mutate(across(all_of(rwa.itms), ~as.numeric(scale(.)),.names ="{.col}_scl"))


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


## ANTI-GAY ----

ess4 <- 
  ess4 %>%
  mutate(anti_gay     = freehms,
         anti_gay_scl = as.numeric(scale(freehms)),.keep = "unused")


## ANTI-IMMIGRANT ----
anti_mig.itms     <- c("imbgeco","imueclt","imwbcnt")
anti_mig.itms_scl <- c("imbgeco_scl","imueclt_scl","imwbcnt_scl")


ess4 <- 
  ess4 %>%
  mutate(across(c("imbgeco","imueclt","imwbcnt"), ~ 11 - .),
         across(all_of(anti_mig.itms), ~as.numeric(scale(.)),.names ="{.col}_scl"))

psych::alpha(ess4[anti_mig.itms])
psych::alpha(ess4[anti_mig.itms_scl])

ess4$anti_immigrant     <- rowMeans(ess4[anti_mig.itms],na.rm = T)
ess4$anti_immigrant_scl <- rowMeans(ess4[anti_mig.itms_scl],na.rm = T)


## UNEMPLOYED ----
ess4 <- 
  ess4 %>%
  mutate(unemployed     = 6 - uentrjb,
         unemployed_scl = as.numeric(scale(unemployed)),.keep = "unused")


## WOMEN ----
anti_wmn.itms     <- c("wmcpwrk","mnrgtjb")
anti_wmn.itms_scl <- c("wmcpwrk_scl","mnrgtjb_scl")

ess4 <- 
  ess4 %>%
  mutate(across(all_of(anti_wmn.itms),~6-.),
         across(all_of(anti_wmn.itms), ~as.numeric(scale(.)),.names = "{.col}_scl"))

psych::alpha(ess4[anti_wmn.itms])
psych::alpha(ess4[anti_wmn.itms_scl])

ess4$sexist     <- rowMeans(ess4[anti_wmn.itms],na.rm = T)
ess4$sexist_scl <- rowMeans(ess4[anti_wmn.itms_scl],na.rm = T)

## AGEISM ----

ess4 <- 
  ess4 %>%
  mutate(ppl_20s     = 11 - oafl20,
         ppl_70s     = 11 - oafl70,
         ppl_20s_scl = as.numeric(scale(ppl_20s)),
         ppl_70s_scl = as.numeric(scale(ppl_70s)), .keep = "unused")


target.itms     <- c("anti_gay","anti_immigrant","unemployed","sexist","ppl_20s","ppl_70s")
target.itms_scl <- c("anti_gay_scl","anti_immigrant_scl","unemployed_scl","sexist_scl","ppl_20s_scl","ppl_70s_scl")

# ANALYZE ---- 

## ICCs ----
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

## MEDIAN SPLIT ----

ess4 <- 
  ess4 %>%
  mutate(lft_rght.gm = case_when(lrscale < 5 ~ "lft",
                             lrscale > 5 ~ "rght"),
         rwa_hilo.gm = case_when(rwa < median(rwa,na.rm = T) ~ "low",
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

# ANALYZES ----

## Correlations

ess4 %>%
  select("rwa","lrscale",all_of(target.itms_scl)) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant()
  
## CORRELATION TABLES ----

### Country Median Split ----

#within country
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
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
  summarise(within_cntry = cor(high,low))


## Whole Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "Whole_Sample_btwn_cntry") %>%
  full_join(tbl_corr,.,by = "cntry_lbl")

## Low Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(rwa_hilo_cntry_median != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "low_sample_btwn_cntry") %>%
  full_join(tbl_corr,.,by = "cntry_lbl")

## High Sample Across Countries
tbl_corr <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(rwa_hilo_cntry_median != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "high_sample_btwn_cntry") %>%
  full_join(tbl_corr,.,by = "cntry_lbl")

tbl_corr %>%
  arrange(desc(within_cntry)) %>%
  print(n = nrow(.))

list(data = tbl_corr,
       means = tbl_corr %>%
         summarise_if(is.numeric,mean, na.rm = TRUE),
       sd = tbl_corr %>%
         summarise_if(is.numeric,sd, na.rm = TRUE)
  )

tbl_corr %>%
  arrange(desc(-within_cntry))

### Grand Median Split ----


#within country
tbl_corr.gm <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo.gm) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo.gm)) %>%
  ungroup() %>%
  mutate(rwa_hilo.gm= factor(rwa_hilo.gm, levels = c("low","high"))) %>%
  tidyr::pivot_longer(cols      = 3:8, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = rwa_hilo.gm, values_from = Rating) %>%
  group_by(cntry_lbl) %>%
  summarise(within_cntry = cor(high,low))


## Whole Sample Across Countries
tbl_corr.gm <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise_at(vars(anti_gay_scl:ppl_70s_scl), mean, na.rm = TRUE) %>%
  tidyr::pivot_longer(cols = 2:7, 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "Whole_Sample_btwn_cntry") %>%
  full_join(tbl_corr.gm,.,by = "cntry_lbl")

## Low Sample Across Countries
tbl_corr.gm <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo.gm) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(rwa_hilo.gm != "high") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "low_sample_btwn_cntry") %>%
  full_join(tbl_corr.gm,.,by = "cntry_lbl")

## High Sample Across Countries
tbl_corr.gm <- 
  ess4 %>%
  select(cntry_lbl,rwa_hilo.gm,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl,rwa_hilo.gm) %>%
  summarise_at(vars(all_of(target.itms_scl)), mean, na.rm = TRUE) %>%
  filter(!is.na(rwa_hilo.gm)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl), 
                      names_to  = "Target",
                      values_to = "Rating") %>%
  filter(rwa_hilo.gm != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = Rating) %>%
  corrr::correlate(method = "pearson") %>% 
  summarise_if(is.numeric,mean, na.rm = T) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), 
                      names_to  = "cntry_lbl",
                      values_to = "high_sample_btwn_cntry") %>%
  full_join(tbl_corr.gm,.,by = "cntry_lbl")


list(data = tbl_corr.gm,
     means = tbl_corr.gm %>%
       summarise_if(is.numeric,mean, na.rm = TRUE),
     sd = tbl_corr.gm %>%
       summarise_if(is.numeric,sd, na.rm = TRUE)
)


# DESCRIPTIVES COUNTRY ----

ess4 %>%
  select(cntry_lbl,rwa,all_of(target.itms)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(1:7, mean, na.rm = TRUE)) %>%
  rename("RWA"                 = rwa,
         "Anti-Gay"            = anti_gay,
         "Anti-Immigrant"      = anti_immigrant,
         "Unemployed"          = unemployed,
         "Sexist attitudes"    = sexist,
         "People in their 20s" = ppl_20s,
         "People in their 70s" = ppl_70s)

ess4 %>%
  select(cntry_lbl,rwa,all_of(target.itms_scl)) %>%
  group_by(cntry_lbl) %>%
  summarise(across(1:7, mean, na.rm = TRUE)) %>%
  rename("RWA"                 = rwa,
         "Anti-Gay"            = anti_gay_scl,
         "Anti-Immigrant"      = anti_immigrant_scl,
         "Unemployed"          = unemployed_scl,
         "Sexist attitudes"    = sexist_scl,
         "People in their 20s" = ppl_20s_scl,
         "People in their 70s" = ppl_70s_scl)

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

