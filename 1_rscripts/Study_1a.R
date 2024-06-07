rm(list = ls())

library(dplyr)
library(ggplot2)

#get data----
ds1 <- read.csv('C:/Users/Clemens Lindner/Documents/github/RWA_SDO_revisited/data/rwa_sdo_revisited_study_1.csv', 
                header = TRUE, sep = ";", as.is = T, na.strings = c("-9",NA))

names(ds1) <- tolower(names(ds1))


#demographics----


# SCALES ----

## RWA ----

#item index
rwa.itm.index <- match(c("au01_01","au01_02","au01_03",
                         "au01_04","au01_05","au01_06",
                         "au01_07","au01_08","au01_09"),
                       names(ds1))
#alpha
psych::alpha(ds1[,rwa.itm.index])

#score
ds1$rwa<-rowMeans(ds1[,rwa.itm.index], na.rm = TRUE)


fit<-factanal(na.omit(ds1[,rwa.itm.index]), factors = 3, rotation = "varimax")
fit

fit$loadings[] %>%
  tibble::as.tibble() %>%
  rowwise() %>%
  mutate(across(1:3, ~ if_else(. == max(c_across(1:3), na.rm = TRUE), ., NA_real_)))
  

## NGA ----

#item index
nga.itm.index <- match(c("au02_01","au02_02","au02_03","au02_04","au02_05","au02_06",
                         "au02_07","au02_08","au02_09","au02_10","au02_11","au02_12"),
                       names(ds1))

#alpha
psych::alpha(ds1[,nga.itm.index])

#score
ds1$nga<-rowMeans(ds1[,nga.itm.index], na.rm = TRUE)


## SDO ----

#item index
sdo.itm.index <- match(c("sd01_01","sd01_02","sd01_03","sd01_04","sd01_05","sd01_06","sd01_07","sd01_08",
                         "sd01_09","sd01_10","sd01_11","sd01_12","sd01_13","sd01_14","sd01_15","sd01_16"),
                       names(ds1))

#alpha
psych::alpha(ds1[,sdo.itm.index])

#score
ds1$sdo<-rowMeans(ds1[,sdo.itm.index], na.rm = TRUE)

## CONSERVATISM ----

#recode items
ds1 <- ds1 %>% mutate(across(c(ko01_03,ko01_05,ko01_07,ko01_09,ko01_12,ko01_13,ko01_14,ko01_16,
                               ko01_17,ko01_18,ko01_19,ko01_22,ko01_24,ko01_25,ko01_29,ko01_30), ~ 12 - .))

#item index
con.itm.index <- match(c("ko01_02","ko01_03","ko01_04","ko01_05","ko01_06","ko01_07",
                         "ko01_08","ko01_09","ko01_10","ko01_11","ko01_12","ko01_13",
                         "ko01_14","ko01_15","ko01_16","ko01_17","ko01_18","ko01_19",
                         "ko01_20","ko01_21","ko01_22","ko01_23","ko01_24","ko01_25",
                         "ko01_26","ko01_27","ko01_28","ko01_29","ko01_30","ko01_31"),
                       names(ds1))

#alpha
psych::alpha(ds1[,con.itm.index], check.keys = T)

#score
ds1$con<-rowMeans(ds1[,con.itm.index],na.rm = TRUE)


ds1$ko01_30

## MERITOCRACY ----

#item index
mi.itm.index <- match(c("mi01_01","mi01_02","mi01_03","mi01_04","mi01_05","mi01_06","mi01_07","mi01_08"),
                      names(ds1))

#alpha
psych::alpha(ds1[,mi.itm.index])

#score
ds1$mi<-rowMeans(ds1[,mi.itm.index], na.rm = TRUE)

pca_mi <- princomp(covmat = cor(ds1[,mi.itm.index],use = "pairwise", method = "pearson"))
plot(pca_mi, type = "lines", main = "PCA prejudice targets")
fit<-factanal(na.omit(ds1[,mi.itm.index]), factors = 1, rotation = "varimax")
fit

## POLITICAL SELF-PLACEMENT ----

ds1$polid <- ds1$de03_01


# TARGETS ----

#recode
ds1 <- ds1 %>% mutate(across(matches("ta01_"), ~ 12 - .))

#rename
names(ds1)

ds1 <- rename(ds1,
              "rich"         = ta01_01,"fundamentalists" = ta01_02,"catholics"    = ta01_03,"fascists"      = ta01_04,"lawyers"                = ta01_05,
              "soldiers"     = ta01_06,"elderly"         = ta01_07,"provincially" = ta01_08,"heirs"         = ta01_09,"nobles"                 = ta01_10,
              "homosexuals"  = ta01_11,"refugees"        = ta01_12,"pacifists"    = ta01_13,"feminists"     = ta01_14,"human_rights_advocates" = ta01_15,
              "aids"         = ta01_16,"muslims"         = ta01_17,"intersexuals" = ta01_18,"vegans"        = ta01_19,"sinti"                  = ta01_20,
              "antifa"       = ta01_21,"homeless"        = ta01_22,"communists"   = ta01_23,"animal_rights" = ta01_24,"environmentalists"      = ta01_25,
              "drug_addicts" = ta01_26,"unemployed"      = ta01_27,"unionists"    = ta01_28,"welfare"       = ta01_29,"cyclists"               = ta01_30,
              "businessmen"  = ta01_31,"manager"         = ta01_32,"industrials"  = ta01_33,"lobbyists"     = ta01_34,"bankers"                = ta01_35,
              "shareholders" = ta01_36,"polluters"       = ta01_37,"millionaires" = ta01_38,"gentrify"      = ta01_39,"officials"              = ta01_40)

#item index

trgt.index <- match(c("rich"        ,"fundamentalists","catholics"   ,"fascists"      ,"lawyers",
                      "soldiers"    ,"elderly"        ,"provincially","heirs"         ,"nobles",
                      "homosexuals" ,"refugees"       ,"pacifists"   ,"feminists"     ,"human_rights_advocates",
                      "aids"        ,"muslims"        ,"intersexuals","vegans","sinti",
                      "antifa"      ,"homeless"       ,"communists"  ,"animal_rights" ,"environmentalists",
                      "drug_addicts","unemployed"     ,"unionists"   ,"welfare"       ,"cyclists",
                      "businessmen" ,"manager"        ,"industrials" ,"lobbyists"     ,"bankers",
                      "shareholders","polluters"      ,"millionaires","gentrify"      ,"officials"),
                    names(ds1))

psych::describe(ds1$rich)

### PCA Factor and cluster -----

## PCA
targets <- ds1[trgt.index]

targets <- cor(targets, use = "pairwise", method = "pearson")
PCA1 <- princomp(covmat = targets)
plot(PCA1, type = "lines", main = "PCA prejudice targets")
summary(PCA1)

## Factor analysis
targets <- ds1[trgt.index]
fit<-factanal(na.omit(targets), factors = 2, rotation = "varimax")
fit
fit_load<-as.matrix(round(loadings(fit)[], digits = 2))

fit_load <-
  fit_load %>%
  as.data.frame() %>%
  mutate(Factor1 = case_when(Factor1 > Factor2 ~ Factor1),
         Factor2 = case_when(is.na(Factor1) ~ Factor2))

table(!is.na(data.frame(fit_load)$Factor2))
table(!is.na(data.frame(fit_load)$Factor1))

write.table(fit_load, sep = ";",quote = FALSE)


### aggregate ----

#LIBERAL TARGETS

#item index
ta_lft.itm.index <- match(row.names(data.frame(fit_load)[!is.na(data.frame(fit_load)$Factor2),]),names(ds1))

#alpha
psych::alpha(ds1[ta_lft.itm.index])

#mean
ds1$ta_lft  <- rowMeans(ds1[ta_lft.itm.index], na.rm = T)

#CONSERVATIVE TARGETS

#item index
ta_rght.itm.index <-match(row.names(data.frame(fit_load)[!is.na(data.frame(fit_load)$Factor1),]),names(ds1))

#alpha
psych::alpha(ds1[,ta_rght.itm.index])

#mean
ds1$ta_rght <- rowMeans(ds1[ta_rght.itm.index], na.rm = T)


# ANALYSES ----

## predictor descriptives ----

ds1$polid <- ds1$de03_01

psych::describe(ds1$rwa)
psych::describe(ds1$nga)
psych::describe(ds1$sdo)
psych::describe(ds1$con)
psych::describe(ds1$mi)
psych::describe(ds1$polid)

##predictor bivariate rs ----

#write.table(
round(
  cor(ds1[,c("rwa","nga","sdo","con","mi","polid")], use = "pairwise", method = "pearson")
  ,2)
#,quote = FALSE, sep = ";")

## single targets rs ----
round(cor(ds1[trgt.index],ds1$rwa, use = "pairwise", method = "pearson"),2)
round(cor(ds1[trgt.index],ds1$nga, use = "pairwise", method = "pearson"),2)
round(cor(ds1[trgt.index],ds1$sdo, use = "pairwise", method = "pearson"),2)
round(cor(ds1[trgt.index],ds1$con, use = "pairwise", method = "pearson"),2)
round(cor(ds1[trgt.index],ds1$mi, use = "pairwise", method = "pearson"),2)
round(cor(ds1[trgt.index],ds1$polid, use = "pairwise", method = "pearson"),2)

## aggregated targets rs ----

cor(ds1$rwa,ds1$ta_rght, use = "pairwise", method = "pearson")
cor(ds1$rwa,ds1$ta_lft,  use = "pairwise", method = "pearson")

cor(ds1$nga,ds1$ta_rght, use = "pairwise", method = "pearson")
cor(ds1$nga,ds1$ta_lft,  use = "pairwise", method = "pearson")

cor(ds1$sdo,ds1$ta_rght, use = "pairwise", method = "pearson")
cor(ds1$sdo,ds1$ta_lft,  use = "pairwise", method = "pearson")

cor(ds1$ta_rght,ds1$ta_lft,  use = "pairwise", method = "pearson")

## partial correlations -----

#indices
rwa.index          <- which(names(ds1) == "rwa")
nga.index          <- which(names(ds1) == "nga")
con.index          <- which(names(ds1) == "con")
sdo.index          <- which(names(ds1) == "sdo")
mi.index           <- which(names(ds1) == "mi")
polid.index        <- which(names(ds1) == "polid")
ta_lft.index       <- which(names(ds1) == "ta_lft")
ta_rght.index      <- which(names(ds1) == "ta_rght")


### rwa|con ----

partCorr <- rep(NA,length(trgt.index))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.index ){
  m <- ds1[,c(rwa.index, i, con.index)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.index)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds1[trgt.index])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

### sdo|con ----

partCorr <- rep(NA,length(trgt.index))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.index ){
  m <- ds1[,c(sdo.index, i, con.index)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.index)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds1[trgt.index])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

### sdo|mi ----

partCorr <- rep(NA,length(trgt.index))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.index ){
  m <- ds1[,c(sdo.index, i, mi.index)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.index)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds1[trgt.index])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

## SAMPLE SPLIT ----

pred_splt <- ds1 %>%
  select(case,rwa,nga,sdo,polid)%>%
  mutate(rwa_splt = case_when(rwa <= median(rwa, na.rm = T) ~ "low",
                              rwa >  median(rwa, na.rm = T) ~ "hgh"),
         rwa_1sd  = case_when(rwa <= (mean(rwa,na.rm = T) -   sd(rwa, na.rm = T)) ~ "-1sd",
                              rwa >  (mean(rwa,na.rm = T) +   sd(rwa, na.rm = T)) ~ "+1sd"),
         rwa_2sd  = case_when(rwa <= (mean(rwa,na.rm = T) - 2*sd(rwa, na.rm = T)) ~ "-2sd",
                              rwa >  (mean(rwa,na.rm = T) + 2*sd(rwa, na.rm = T)) ~ "+2sd"),
         nga_splt = case_when(nga <= median(nga, na.rm = T) ~ "low",
                              nga >  median(nga, na.rm = T) ~ "hgh"),
         nga_1sd  = case_when(nga <= (mean(nga,na.rm = T) -   sd(nga, na.rm = T)) ~ "-1sd",
                              nga >  (mean(nga,na.rm = T) +   sd(nga, na.rm = T)) ~ "+1sd"),
         nga_2sd  = case_when(nga <= (mean(nga,na.rm = T) - 2*sd(nga, na.rm = T)) ~ "-2sd",
                              nga >  (mean(nga,na.rm = T) + 2*sd(nga, na.rm = T)) ~ "+2sd"),
         sdo_splt = case_when(sdo <= median(sdo, na.rm = T) ~ "low",
                              sdo >  median(sdo, na.rm = T) ~ "hgh"),
         sdo_1sd  = case_when(sdo <= (mean(sdo,na.rm = T) -   sd(sdo, na.rm = T)) ~ "-1sd",
                              sdo >  (mean(sdo,na.rm = T) +   sd(sdo, na.rm = T)) ~ "+1sd"),
         sdo_2sd  = case_when(sdo <= (mean(sdo,na.rm = T) - 2*sd(sdo, na.rm = T)) ~ "-2sd",
                              sdo >  (mean(sdo,na.rm = T) + 2*sd(sdo, na.rm = T)) ~ "+2sd"),
         polid_splt = case_when(polid <= median(polid, na.rm = T) ~ "low",
                                polid >  median(polid, na.rm = T) ~ "hgh"),
         polid_1sd  = case_when(polid <= (mean(polid,na.rm = T) -   sd(polid, na.rm = T)) ~ "-1sd",
                                polid >  (mean(polid,na.rm = T) +   sd(polid, na.rm = T)) ~ "+1sd"),
         polid_2sd  = case_when(polid <= (mean(polid,na.rm = T) - 2*sd(polid, na.rm = T)) ~ "-2sd",
                                polid >  (mean(polid,na.rm = T) + 2*sd(polid, na.rm = T)) ~ "+2sd")) 



pred_splt <- ds1 %>%
  select(case,rwa,nga,sdo,polid)%>%
  tidyr::pivot_longer(cols = 2:5, names_to = "scale",values_to = "score") %>%
  group_by(scale) %>%
  mutate(median = median(score, na.rm = T),
         mean   = mean(score, na.rm = T),
         sd     = sd(score, na.rm = T),
         sd2    = 2*sd(score,na.rm = T)) %>%
  ungroup() %>%
  mutate(splt = case_when(score <= median   ~ "low",
                          score >  median   ~ "hgh"),
         sd1  = case_when(score <= mean-sd  ~ "-1sd",
                          score >  mean+sd  ~ "+1sd"),
         sd2  = case_when(score <= mean-sd2 ~ "-2sd",
                          score >  mean+sd2 ~ "+2sd"))# %>%
#select(case,scale,splt,sd1,sd2) %>%
#tidyr::pivot_longer(cols = 3:5,names_to = "labels",values_to = "splt") %>% select(-labels)

ta_lng <- ds1 %>%
  select(case,all_of(trgt.index)) %>%
  select(-c("ta_lft","ta_rght")) %>%
  tidyr::pivot_longer(cols = 2:41, names_to = "target", values_to = "rating")

ta_lng <- left_join(ta_lng,pred_splt, by = "case")


med_splt <- ta_lng %>%
  group_by(target,scale,splt) %>%
  filter(!is.na(splt)) %>%
  summarise(mean = mean(rating, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = splt, values_from = mean) %>%
  mutate(splt = "median") %>%
  ungroup()

sd1_splt <- ta_lng %>%
  group_by(target,scale,sd1) %>%
  filter(!is.na(sd1)) %>%
  summarise(mean = mean(rating, na.rm = TRUE))  %>%
  tidyr::pivot_wider(names_from = sd1, values_from = mean) %>%
  rename("low"="-1sd","hgh"="+1sd") %>%
  mutate(splt = "sd1") %>%
  ungroup()

sd2_splt <- ta_lng %>%
  group_by(target,scale,sd2) %>%
  filter(!is.na(sd2)) %>%
  summarise(mean = mean(rating, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = sd2, values_from = mean) %>%
  rename("low"="-2sd","hgh"="+2sd") %>%
  mutate(splt = "sd2") %>%
  ungroup()

splt <- rbind(med_splt,sd1_splt)
splt <- rbind(splt,sd2_splt)

splt %>%
  group_by(scale,splt) %>%
  summarise(r = cor(low,hgh, use = "pairwise", method = "pearson"))

## Left-right interaction ----

lm_int <- ds1 %>%
  select(case,rwa,sdo,all_of(trgt.index)) %>%
  tidyr::pivot_longer(cols = 4:43,
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(lft_rght = case_when(target %in% names(na.omit(fit_load[,"Factor1"])) ~ "right",
                              target %in% names(na.omit(fit_load[,"Factor2"])) ~ "left")) %>%
  filter(!is.na(lft_rght))

m1 <- lm(rating ~ rwa*lft_rght, data = lm_int)
summary(m1)

m2 <- lm(rating ~ sdo*lft_rght, data = lm_int)
summary(m2)

## MLM intrapersonal variation ----

mlm <- ds1 %>%
  select(case,rwa,con,all_of(trgt.index)) %>%
  tidyr::pivot_longer(cols      = 4:43,
                      names_to  = "target",
                      values_to = "rating")  %>%
  group_by(target) %>%
  mutate(cwc_rwa   = rwa - mean(rwa, na.rm = T),
         trgt_mean = mean(rating,na.rm = T)) %>%
  print(n = 80)


icc_ta <- lmerTest::lmer(rating ~ (1|target), data = mlm, REML = F)
summary(icc_ta)
performance::icc(icc_ta)


mlm_cwc_rwa <- lmerTest::lmer(rating ~ (1|cwc_rwa), data = mlm, REML = F)
summary(mlm_cwc_rwa)
performance::icc(mlm_cwc_rwa)

mlm_rwa_ta <- lmerTest::lmer(rating ~ cwc_rwa  + (cwc_rwa|target), data = mlm, REML = F)
summary(mlm_rwa_ta)
performance::icc(mlm_rwa_ta)

anova(mlm_cwc_rwa,mlm_rwa_ta)

mlm_rwa_ta_cntrl <- lmerTest::lmer(rating ~ cwc_rwa  + (cwc_rwa|target) + (1|con), data = mlm, REML = F)
summary(mlm_rwa_ta_cntrl)
performance::icc(mlm_rwa_ta_cntrl)

anova(mlm_rwa_ta,mlm_rwa_ta_cntrl)