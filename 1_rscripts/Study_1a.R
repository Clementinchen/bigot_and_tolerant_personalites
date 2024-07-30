rm(list = ls())

library(dplyr)
library(ggplot2)

#get data----
ds1 <- read.csv('https://osf.io/ywpq4/download', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-9",NA)) %>%
  janitor::clean_names(.)

ds1 <- read.csv(file = './0_data/rwa_sdo_revisited_study_1a.csv', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-1","-9",NA))  %>%
  janitor::clean_names(.)


#get codebook
cdbk_1a <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 1) %>%
  mutate(variable = tolower(variable))


# IV: SCALES ----

## RWA ----

#RWA items
rwa.itms <- 
  ds1 %>% select(starts_with("au01")) %>% names()

#factor analysis
fa_rwa <- factanal(na.omit(ds1[,rwa.itms]), factors = 1, rotation = "varimax")
fa_rwa


#alpha
psych::alpha(ds1[,rwa.itms])

#score
ds1$rwa <- rowMeans(ds1[,rwa.itms], na.rm = TRUE)


## SDO ----

#sdo items
sdo.itms <- 
  ds1 %>% select(starts_with("sd01")) %>% names()

#factor analysis
fa_sdo <- factanal(na.omit(ds1[,sdo.itms]), factors = 2, rotation = "varimax")
fa_sdo

#alpha
psych::alpha(ds1[,sdo.itms])

#score
ds1$sdo <- rowMeans(ds1[,sdo.itms], na.rm = TRUE)

## CONSERVATISM ----

#recode items

ds1 <- 
  ds1 %>% 
  mutate(across(c(cdbk_1a %>%
                    filter(scale == "Conservatism" & coding == "reversed") %>%
                    select(variable) %>% pull()), ~ 12 - .))


#conservatism items
con.itms <- 
  ds1 %>% select(starts_with("ko01")) %>% names()

#alpha
psych::alpha(ds1[,con.itms], check.keys = T)

#score
ds1$con <- rowMeans(ds1[,con.itms],na.rm = TRUE)

## MERITOCRACY ----

#meritocracy items
mi.itms <- 
  ds1 %>% select(starts_with("mi01")) %>% names()

fa_mi <- factanal(na.omit(ds1[,mi.itms]), factors = 2, rotation = "varimax")
fa_mi

#alpha
psych::alpha(ds1[,mi.itms])

#score
ds1$mi<-rowMeans(ds1[,mi.itms], na.rm = TRUE)


## POLITICAL SELF-PLACEMENT ----

ds1 <- ds1 %>%
  rename("polid" = "de03_01")


# DV: TARGETS ----

#recode
ds1 <- ds1 %>% mutate(across(matches("ta01_"), ~ 12 - .))

#rename
rename_trgts <- setNames(cdbk_1a %>%
                           filter(scale == "Prejudice") %>%
                           pull(english_item_translation) %>% janitor::make_clean_names(),
                         cdbk_1a %>%
                           filter(scale == "Prejudice") %>%
                           pull(variable))

ds1 <- ds1 %>%
  rename_with(~ rename_trgts[.],.cols = all_of(names(rename_trgts)))

#Targets
trgt.itms <- 
  cdbk_1a %>%
  filter(scale == "Prejudice") %>%
  pull(english_item_translation) %>% 
  janitor::make_clean_names()

### PCA & FACTOR ANALYSIS -----

## PCA

pca_targets <- cor(ds1[trgt.itms], use = "pairwise", method = "pearson")
pca_targets <- princomp(covmat = pca_targets)
plot(pca_targets, type = "lines", main = "PCA prejudice targets")
summary(pca_targets)

## Factor analysis
fa_trgts <- factanal(na.omit(ds1[trgt.itms]), factors = 2, rotation = "varimax")
fa_trgts

fa_trgts <-
  loadings(fa_trgts)[] %>%
  data.frame() %>%
  mutate(Factor1     = case_when(Factor1 > Factor2 ~ Factor1),
         Factor2     = case_when(is.na(Factor1) ~ Factor2),
         trgt_factor = as.factor(case_when(is.na(Factor1) ~ "liberal",
                                           TRUE ~ "conservative")))

table(fa_trgts$trgt_factor)

### AGGREGATE ----

#LIBERAL TARGETS

#item index
ta_lft.itms <- 
  fa_trgts %>%
  filter(trgt_factor == "liberal") %>%
  row.names()

#alpha
psych::alpha(ds1[ta_lft.itms])

#mean
ds1$ta_lft  <- rowMeans(ds1[ta_lft.itms], na.rm = T)

#CONSERVATIVE TARGETS

#item index
ta_rght.itms <-
  fa_trgts %>%
  filter(trgt_factor == "conservative") %>%
  row.names()

#alpha
psych::alpha(ds1[,ta_rght.itms])

#mean
ds1$ta_rght <- rowMeans(ds1[ta_rght.itms], na.rm = T)


# ANALYSES ----

## predictor descriptives ----

prdctrs <- c("rwa","sdo","con","mi","polid")

psych::describe(ds1$rwa)   # descriptives rwa
psych::describe(ds1$sdo)   # descriptives sdo
psych::describe(ds1$con)   # descriptives conservatism
psych::describe(ds1$mi)    # descriptives meritocracy
psych::describe(ds1$polid) # descriptives political self-placement

##predictor bivariate rs ----

ds1 %>%
  select(all_of(prdctrs)) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                symbols = c("***", "**", "*", ""))
  
## single targets rs ----

biv_r <- matrix(nrow = length(trgt.itms), ncol = length(prdctrs))
rownames(biv_r) <- trgt.itms
colnames(biv_r) <- prdctrs

# Loop through each pair of predictor and target variable
for (tgt in trgt.itms) {
  for (prd in prdctrs) {
    # Perform correlation test
    test_result <- cor.test(ds1[[prd]], ds1[[tgt]])
    # Store the p-value in the results matrix
    biv_r[tgt, prd] <- round(test_result$estimate,2)
  }
}

biv_r %>%
  data.frame() %>%
  tibble::rownames_to_column("target") %>%
  tibble::as_tibble() %>%
  arrange(rwa) %>% print(n = 40)


## aggregated targets rs ----

ds1 %>%
  select(all_of(prdctrs),ta_lft,ta_rght) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                symbols = c("***", "**", "*", ""))

## partial correlations -----

rbind(
  ds1 %>%
    select(rwa,sdo,ta_rght,ta_lft) %>% 
    cor(use = "pairwise") %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "Bivariate"),
  ds1 %>%
    select(rwa,sdo,ta_rght,ta_lft,con) %>% 
    psych::partial.r(.,1:4,5) %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "Conservatism"),
  ds1 %>%
    select(rwa,sdo,ta_rght,ta_lft,mi) %>% 
    psych::partial.r(.,1:4,5) %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "Meritocracy"),
  ds1 %>%
    select(rwa,sdo,ta_rght,ta_lft,polid) %>% 
    psych::partial.r(.,1:4,5) %>% 
    data.frame() %>%
    tibble::rownames_to_column("target_scale") %>%
    mutate(partial = "ideologica_slf_plcmnt")
) %>%
  filter(target_scale == "ta_lft"|target_scale == "ta_rght") %>%
  select(target_scale,rwa,sdo,partial) %>%
  tidyr::pivot_wider(names_from = "target_scale", values_from = c("rwa","sdo"))


## SAMPLE SPLIT ----

ds1 %>%
  select(case,all_of(prdctrs),all_of(trgt.itms)) %>%
  tidyr::pivot_longer(cols = all_of(prdctrs), names_to = "scale",values_to = "score") %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms), names_to = "target",values_to = "rating") %>%
  group_by(scale) %>%
  mutate(
    mean   = mean(score,   na.rm = T),
    median = median(score, na.rm = T),
    sd     = sd(score,     na.rm = T),
    mean_splt = case_when(score > mean ~ "high",
                          score < mean ~ "low"),
    median_splt = case_when(score > median ~ "high",
                            score < median ~ "low"),
    sd1 = case_when(score > mean+sd ~ "high",
                    score < mean-sd ~ "low"),
    sd2 = case_when(score > mean+(2*sd) ~ "high",
                    score < mean-(2*sd) ~ "low")) %>%
  ungroup() %>%
  ### aggregate prejudice target means by scale
  tidyr::pivot_longer(cols = c(mean_splt,median_splt,sd1,sd2),
                      names_to = "splt_scale", values_to = "hgh_low") %>%
  group_by(scale,target,splt_scale,hgh_low) %>%
  summarise(pjdce_splt = mean(rating, na.rm = T)) %>%
  filter(!is.na(hgh_low)) %>%
  tidyr::pivot_wider(names_from = hgh_low, values_from = "pjdce_splt") %>%
  mutate(splt_scale = stringr::str_remove_all(splt_scale,"_splt")) %>%
  ungroup() %>%
  ### correlate
  group_by(scale,splt_scale) %>% 
  summarize(cor=cor(high, low)) 
  
  

## Left-right interaction ----

lm_int <- 
ds1 %>%
  select(case,rwa,sdo,all_of(trgt.itms)) %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms),
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(rwa_scl  = as.numeric(scale(rwa)),
         sdo_scl  = as.numeric(scale(sdo)),
         trgt_fct = case_when(target %in% ta_rght.itms ~ "right",
                              target %in% ta_lft.itms ~ "left")) 

m1 <- lm(rating ~ rwa_scl*trgt_fct, data = lm_int)
summary(m1)

m2 <- lm(rating ~ sdo_scl*trgt_fct, data = lm_int)
summary(m2)

lm_int %>%
  tidyr::pivot_longer(cols = c(rwa,rwa_scl,sdo,sdo_scl), 
                      names_to = "scale",
                      values_to = "score") %>%
  filter(scale == "rwa_scl"|scale == "sdo_scl") %>%
  ggplot(aes(y = rating, x = score,color = trgt_fct)) +
  geom_smooth(se = FALSE, method = lm) +
  facet_wrap(~scale)

