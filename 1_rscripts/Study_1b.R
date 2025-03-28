rm(list = ls())

library(dplyr)
library(ggplot2)

# DATA WRANGLING ----

ds2 <- read.csv('https://osf.io/ywpq4/download', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-9",NA)) %>%
  janitor::clean_names(.)

ds2 <- read.csv(file = './0_data/rwa_sdo_revisited_study_1b.csv', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-1","-9",NA))  %>%
  janitor::clean_names(.)


#get codebook
cdbk_1b <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 2) %>%
  mutate(variable = tolower(variable))


rename_vars.1b <- setNames(cdbk_1b %>%
                             pull(variable_label),
                           cdbk_1b %>%
                             pull(variable))

ds2 <- ds2 %>%
  rename_with(~ rename_vars.1b[.],.cols = all_of(names(rename_vars.1b)))


## PREDICTORS ----

prdctrs <- c("rwa","sdo","con","mi","polid")

### RWA ----

#RWA items
rwa.itms <- 
  ds2 %>% select(starts_with("rwa_")) %>% names()

#factor analysis
fa_rwa.1b <- factanal(na.omit(ds2[,rwa.itms]), factors = 1, rotation = "varimax")
fa_rwa.1b


#alpha
psych::alpha(ds2[,rwa.itms])

#score
ds2$rwa <- rowMeans(ds2[,rwa.itms], na.rm = TRUE)


### SDO ----

#sdo items
sdo.itms <- 
  ds2 %>% select(starts_with("sdo_")) %>% names()

#factor analysis
fa_sdo.1b <- factanal(na.omit(ds2[,sdo.itms]), factors = 2, rotation = "varimax")
fa_sdo.1b

#alpha
psych::alpha(ds2[,sdo.itms])

#score
ds2$sdo <- rowMeans(ds2[,sdo.itms], na.rm = TRUE)

### CONSERVATISM ----

#recode items

ds2 <- 
  ds2 %>% 
  mutate(across(c(cdbk_1b %>%
                    filter(scale == "Conservatism" & coding == "reversed") %>%
                    select(variable_label) %>% pull()), ~ 101 - .))


#conservatism items
con.itms <- 
  ds2 %>% select(starts_with("con_")) %>% names()

#alpha
psych::alpha(ds2[,con.itms], check.keys = T)

#score
ds2$con <- rowMeans(ds2[,con.itms],na.rm = TRUE)

### MERITOCRACY ----

#meritocracy items
mi.itms <- 
  ds2 %>% select(starts_with("mi_")) %>% names()

fa_mi.1b <- factanal(na.omit(ds2[,mi.itms]), factors = 2, rotation = "varimax")
fa_mi.1b

#alpha
psych::alpha(ds2[,mi.itms])

#score
ds2$mi<-rowMeans(ds2[,mi.itms], na.rm = TRUE)


## DV: TARGETS ----

#recode
ds2 <- ds2 %>% mutate(across(starts_with("prj_"), ~ 102 - .))


#Targets
trgt.itms.1b <- 
  ds2 %>%
  select(starts_with("prj_")) %>%
  names()

### FACTOR ANALYSIS -----

psych::fa.parallel(ds2 %>% select(starts_with("prj_")))

fa_trgts.1b <- 
  psych::fa(ds2 %>%
              select(starts_with("prj_")), 
            nfactors = 2, 
            rotate = "oblimin", 
            fm = "ml")

fa_trgts.1b <- 
  loadings(fa_trgts.1b)[] %>%
  data.frame() %>%
  mutate(ML2     = case_when(ML2 > ML1 ~ ML2),
         ML1     = case_when(is.na(ML2) ~ ML1),
         trgt_factor = as.factor(case_when(is.na(ML2) ~ "liberal",
                                           TRUE ~ "conservative")))

sjPlot::tab_fa(ds2 %>%
                 select(starts_with("prj_")), 
               nmbr.fctr = 2, 
               rotation = "oblimin",
               method = "ml",
               file = "./2_tables/fa_study_1b.html")

### AGGREGATE ----

#LIBERAL TARGETS

#item index
ta_lib.itms.1b <- 
  fa_trgts.1b %>%
  filter(trgt_factor == "liberal") %>%
  row.names()

#alpha
psych::alpha(ds2[ta_lib.itms.1b])

#mean
ds2$ta_lib  <- rowMeans(ds2[ta_lib.itms.1b], na.rm = T)

#CONSERVATIVE TARGETS

#item index
ta_con.itms.1b <-
  fa_trgts.1b %>%
  filter(trgt_factor == "conservative") %>%
  row.names()

#alpha
psych::alpha(ds2[,ta_con.itms.1b])

#mean
ds2$ta_con <- rowMeans(ds2[ta_con.itms.1b], na.rm = T)


# ANALYSES ----

## INCONSISTENT PREDICTION ----

### BIVARIATE CORRELATIONS ----

ds2 %>%
  select(all_of(prdctrs)) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                symbols = c("***", "**", "*", ""))


biv_r <- matrix(nrow = length(trgt.itms.1b), ncol = 4)
rownames(biv_r) <- trgt.itms.1b
colnames(biv_r) <- c("rwa","rwa.p","sdo","sdo.p")
rwa_sdo <- c("rwa","sdo")

# Loop through each pair of predictor and target variable
for (tgt in trgt.itms.1b) {
  for (prd in rwa_sdo) {
    # Perform correlation test
    test_result <- cor.test(ds2[[prd]], ds2[[tgt]])
    # Store the p-value in the results matrix
    biv_r[tgt, prd] <- round(test_result$estimate,2)
    biv_r[tgt, paste0(prd,".p")] <- round(test_result$p.value,3)
    
  }
}

biv_r %>%
  data.frame() %>%
  tibble::rownames_to_column("target") %>%
  tibble::as_tibble() %>%
  arrange(rwa) %>% print(n = 40)

### Correlations Target Factors

ds2 %>%
  select(all_of(rwa_sdo),ta_lib,ta_con) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                symbols = c("***", "**", "*", ""))

### PARTIAL CORRELATIONS -----

rbind(
  ds2 %>%
    select(rwa,sdo,ta_con,ta_lib) %>% 
    cor(use = "pairwise") %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "Bivariate"),
  ds2 %>%
    select(rwa,sdo,ta_con,ta_lib,polid) %>% 
    psych::partial.r(.,1:4,5) %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "pol_slf_plcmnt")
) %>%
  filter(target_scale == "ta_lib"|target_scale == "ta_con") %>%
  select(target_scale,rwa,sdo,partial) %>%
  tidyr::pivot_wider(names_from = "partial", values_from = c("rwa","sdo"))


## IDEOLOGICAL CONFLICT ----

### Left-right interaction ----

ds2 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1b)) %>%
  tidyr::pivot_longer(cols = c(rwa,sdo),
                      names_to  = "predictor",
                      values_to = "score") %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms.1b),
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(trgt_fct = case_when(target %in% ta_con.itms.1b ~ "right",
                              target %in% ta_lib.itms.1b ~ "left")) %>%
  group_by(predictor) %>%
  mutate(predictor_scl = as.numeric(scale(score))) %>%
  group_modify(
    # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
    ~ broom::tidy(lm(rating ~ predictor_scl*trgt_fct, data = .))
  )

## Plot

ds2 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1b)) %>%
  tidyr::pivot_longer(cols = c(rwa,sdo),
                      names_to  = "predictor",
                      values_to = "score") %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms.1b),
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(trgt_fct = case_when(target %in% ta_con.itms.1b ~ "right",
                              target %in% ta_lib.itms.1b ~ "left")) %>%
  group_by(predictor) %>%
  mutate(predictor_scl = as.numeric(scale(score))) %>%
  ggplot(aes(y = rating, x = predictor_scl,color = trgt_fct)) +
  geom_smooth(se = T, method = lm) +
  facet_wrap(~predictor)

### Centered Targets ----


prj_cent.1b <- 
  ds2 %>%
  select(case,rwa,sdo,starts_with("prj_")) %>%
  tidyr::pivot_longer(cols = starts_with("prj_"),
                      names_to = "target",
                      values_to = "rating") %>%
  full_join(.,
            ds2 %>%
              select(case,starts_with("prj_")) %>%
              mutate(across(starts_with("prj_"), 
                            ~ as.numeric(scale(.)))) %>%
              tidyr::pivot_longer(cols = -case,
                                  names_to = "target",
                                  values_to = "rating_scl"),
            by = c("case","target")
  ) %>%
  full_join(.,
            ds2 %>%
              select(case,starts_with("prj_")) %>%
              mutate(across(all_of(trgt.itms.1b), 
                            ~ . - mean(.,na.rm = T))) %>%
              tidyr::pivot_longer(cols = -case,
                                  names_to = "target",
                                  values_to = "rating_gmc"),
            by = c("case","target")
  )


prj_cent.1b <- 
  prj_cent.1b %>%
  tidyr::pivot_longer(cols = c(rwa,sdo),
                      names_to = "scale",
                      values_to = "score") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(score > median(score,na.rm = T) ~ "high",
                          TRUE ~ "low")) %>%
  ungroup() %>%
  group_by(target,scale,hilo) %>%
  summarise(raw = mean(rating,na.rm = T),
            gmc = mean(rating_gmc,na.rm = T),
            scl = mean(rating_scl,na.rm = T)) %>%
  tidyr::pivot_longer(cols = c(raw,gmc,scl),
                      names_to = "prj_cent.1ber",
                      values_to = "prj_rating") %>%
  tidyr::pivot_wider(names_from = c("hilo","prj_cent.1ber"),
                     values_from = prj_rating) %>%
  group_by(scale) %>%
  mutate(diff_raw = abs(high_raw-low_raw),
         diff_gmc = abs(high_gmc-low_gmc),
         diff_scl = abs(high_scl-low_scl)) %>%
  select(target,scale,
         low_raw,high_raw,diff_raw,
         low_gmc,high_gmc,diff_gmc,
         low_scl,high_scl,diff_scl)

list(data = prj_cent.1b,
     means = prj_cent.1b %>%
       summarise_if(is.numeric,mean, na.rm = TRUE),
     sd = prj_cent.1b %>%
       summarise_if(is.numeric,sd, na.rm = TRUE),
     corr = prj_cent.1b %>% summarise(r_raw = cor(low_raw,high_raw),
                                   r_gmc = cor(low_gmc,high_gmc),
                                   r_scl = cor(low_scl,high_scl))
)

## SHARED PREJUDICE ----


### SAMPLE SPLIT ----

split.1b <- 
  ds2 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1b)) %>%
  tidyr::pivot_longer(cols = 4:ncol(.),names_to = "target",values_to = "rating") %>%
  tidyr::pivot_longer(cols = 2:3,names_to = "scale",values_to = "value") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(value > median(value,na.rm = T) ~ "high",
                          TRUE ~ "low")) %>%
  ungroup() %>%
  group_by(target,scale,hilo) %>%
  summarise(m = mean(rating,na.rm = T)) %>%
  tidyr::pivot_wider(names_from = "hilo",  values_from = m) %>%
  mutate(diff = abs(high-low)) %>%
  tidyr::pivot_wider(names_from = "scale",
                     values_from = c("high","low","diff"),
                     names_glue = "{scale}_{.value}") %>%
  ungroup()


list(data = split.1b,
     means = split.1b %>%
       summarise_if(is.numeric,mean, na.rm = TRUE),
     sd = split.1b %>%
       summarise_if(is.numeric,sd, na.rm = TRUE),
     corr = split.1b %>% select(!ends_with("_diff")) %>% corrr::correlate())




# DESCRIPTIVES ----

ds2 %>%
  select(all_of(prdctrs),starts_with("prj"),ta_lib,ta_con) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  #group_by(variable) %>%
  reframe(Mean = mean(value, na.rm = T),
          SD = sd(value, na.rm = T),
          Median = median(value, na.rm = T),
          Min = min(value,na.rm = T),
          Max = max(value, na.rm = T),
          range = abs(Max-Min),
          .by = variable) %>%
  print(n = nrow(.))

# DEMOGRAFICS ----

ds2 %>%
  select(age,polid) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  #group_by(variable) %>%
  reframe(Mean = mean(value, na.rm = T),
          SD = sd(value, na.rm = T),
          Median = median(value, na.rm = T),
          Min = min(value,na.rm = T),
          Max = max(value, na.rm = T),
          range = abs(Max-Min),
          n = n(),
          .by = variable) %>%
  data.frame() %>%
  plyr::rbind.fill(.,
                   ds2 %>%
                     mutate(gender = case_when(gender == 1 ~ "female",
                                               gender == 2 ~ "male",
                                               TRUE~ "other")) %>%
                     janitor::tabyl(gender) %>%
                     rename("variable"="gender") %>%
                     data.frame()
  ) %>% 
  plyr::rbind.fill(
    ds2 %>%
      mutate(education = case_when(education == 1 ~ "es_isced_1",
                                   education == 2 ~ "es_isced_2",
                                   education == 3 ~ "es_isced_3",
                                   education == 4 ~ "es_isced_4",
                                   education == 5 ~ "es_isced_5",
                                   education == 6 ~ "es_isced_6",
                                   education == 7 ~ "es_isced_7",
                                   TRUE ~ NA)) %>%
      janitor::tabyl(education) %>%
      rename("variable"="education") %>%
      select(-valid_percent) %>%
      data.frame()
  )

