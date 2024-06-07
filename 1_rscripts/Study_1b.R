rm(list = ls())
getwd()
library(dplyr)
library(ggplot2)
# READ DATA ----
ds2 <- read.csv(file = 'C:/Users/Clemens Lindner/Documents/github/RWA_SDO_revisited/data/rwa_sdo_revisited_study_2.csv', 
                header = TRUE, sep = ";", as.is = T, na.strings = c("-1","-9",NA))


names(ds2) <- tolower(names(ds2))

# DEMOGRAPHICS ----
janitor::tabyl(ds2$de04)

#SCALES----

## RWA ----
#item idx
rwa.itm.idx <- match(c("au01_01","au01_02","au01_03",
                       "au01_04","au01_05","au01_06",
                       "au01_07","au01_08","au01_09"),
                     names(ds2))
#alpha
psych::alpha(ds2[rwa.itm.idx])

#score
ds2$rwa<-rowMeans(ds2[rwa.itm.idx],na.rm = TRUE)


fit<-factanal(na.omit(ds2[,rwa.itm.idx]), factors = 3, rotation = "varimax")
fit

fit$loadings[] %>%
  tibble::as.tibble() %>%
  rowwise() %>%
  mutate(across(1:3, ~ if_else(. == max(c_across(1:3), na.rm = TRUE), ., NA_real_)))


## NGA ----
#item idx
nga.itm.idx <- match(c("ga01_01","ga01_02","ga01_03","ga01_04","ga01_05","ga01_06",
                       "ga01_07","ga01_08","ga01_09","ga01_10","ga01_11","ga01_12"),
                     names(ds2))

#alpha
psych::alpha(ds2[nga.itm.idx])

#score
ds2$nga<-rowMeans(ds2[nga.itm.idx],na.rm = TRUE)


## SDO ----
#item idx
sdo.itm.idx <- match(c("sd01_01","sd01_02","sd01_03","sd01_04","sd01_05","sd01_06","sd01_07","sd01_08",
                       "sd01_09","sd01_10","sd01_11","sd01_12","sd01_13","sd01_14","sd01_15","sd01_16"),
                     names(ds2))

#alpha
psych::alpha(ds2[sdo.itm.idx])

#score
ds2$sdo<-rowMeans(ds2[sdo.itm.idx], na.rm = TRUE)

## CONSERVATISM ----


psych::describe(ds2$ko01_02)
#recode
ds2 <- ds2 %>% mutate(across(c(ko01_02,ko01_04,ko01_06,ko01_08,ko01_11,ko01_12,ko01_13,ko01_15,
                               ko01_16,ko01_17,ko01_18,ko01_21,ko01_23,ko01_24,ko01_28,ko01_29), ~ 101 - .))

#item idx
con.itm.idx <-    match(c("ko01_01","ko01_02","ko01_03","ko01_04","ko01_05","ko01_06",
                          "ko01_07","ko01_08","ko01_09","ko01_10","ko01_11","ko01_12",
                          "ko01_13","ko01_14","ko01_15","ko01_16","ko01_17","ko01_18",
                          "ko01_19","ko01_20","ko01_21","ko01_22","ko01_23","ko01_24",
                          "ko01_25","ko01_26","ko01_27","ko01_28","ko01_29","ko01_30"),
                        names(ds2))

#alpha
psych::alpha(ds2[con.itm.idx])

#score
ds2$con <- rowMeans(ds2[con.itm.idx], na.rm = TRUE)

## MERITOCRACY ----

#item idx
mi.itm.idx <- match(c("me01_01","me01_02","me01_03","me01_04",
                      "me01_05","me01_06","me01_07","me01_08"),
                    names(ds2))

#alpha
psych::alpha(ds2[mi.itm.idx])

#score
ds2$mi<-rowMeans(ds2[mi.itm.idx], na.rm = TRUE)


pca_mi <- princomp(covmat = cor(ds2[,mi.itm.idx],use = "pairwise", method = "pearson"))
plot(pca_mi, type = "lines", main = "PCA prejudice targets")
fit<-factanal(na.omit(ds2[,mi.itm.idx]), factors = 1, rotation = "varimax")
fit

fit$loadings[] %>%
  tibble::as.tibble() %>%
  rowwise() %>%
  mutate(across(1:3, ~ if_else(. == max(c_across(1:3), na.rm = TRUE), ., NA_real_)))

## NATIONALISM-PATRIOTISM ----

#item idx
pana.itm.idx <- match(c("pa01_01","pa01_02","pa01_03","na01_01","na01_02","na01_03","na02_01"),
                      names(ds2))

#item idx
psych::alpha(ds2[pana.itm.idx])

#score
ds2$pana<-rowMeans(ds2[pana.itm.idx], na.rm = TRUE)

## IDENTIFICATION ----

#item idx
id.itm.idx <- match(c("id01_01","id01_02","id01_03"),names(ds2))

#alpha
psych::alpha(ds2[id.itm.idx])

#score
ds2$id<-rowMeans(ds2[id.itm.idx], na.rm = TRUE)

## HUMANITARIANISM-EGALITARIANISM

#item idx
hueg.itm.idx <- match(c("he01_01","he01_02","he01_03","he01_04","he01_05",
                        "he01_06","he01_07","he01_08","he01_09","he01_10"),names(ds2))


psych::describe(ds2$he01_01)
#alpha
psych::alpha(ds2[hueg.itm.idx])

#score
ds2$hueg<-rowMeans(ds2[hueg.itm.idx], na.rm = TRUE)

## POLITICAL SELF-PLACEMENT ----

ds2$polid <- ds2$de03_01
psych::describe(ds2$polid)

# TARGETS ----

#recode
ds2 <- ds2 %>% mutate(across(matches("ta01_"), ~ 102 - .))

#rename
names(ds2)

ds2 <- rename(ds2,
              "businessmen" = ta01_01,"manager"      = ta01_02,"soldiers"     = ta01_03,"bankers"   = ta01_04,"lobbyists"    = ta01_05,
              "lawyers"     = ta01_06,"homosexuals"  = ta01_07,"pacifists"    = ta01_08,"feminists" = ta01_09,"unemployed"   = ta01_10,
              "welfare"     = ta01_11,"shareholders" = ta01_12,"aids"         = ta01_13,"homeless"  = ta01_14,"refugees"     = ta01_15,
              "antifa"      = ta01_16,"nobles"       = ta01_17,"intersexuals" = ta01_18,"rich"      = ta01_19,"drug_addicts" = ta01_20)

#item idx
trgt.idx <- match(c("businessmen","manager","soldiers","bankers","lobbyists",
                    "lawyers","homosexuals","pacifists","feminists","unemployed",
                    "welfare","shareholders","aids","homeless","refugees",
                    "antifa","nobles","intersexuals","rich","drug_addicts"),
                  names(ds2))

psych::describe(ds2$antifa)

###PCA Factor and Cluster -----

## PCA
trgt_2 <- ds2[,trgt.idx]
trgt_2 <- cor(trgt_2, use = "pairwise", method = "pearson")

PCA2   <- princomp(covmat = trgt_2)
plot(PCA2, type = "lines", main = "PCA prejudice targets")
summary(PCA2)

#Factor Analysis
trgt_2 <- ds2[,trgt.idx]
fit2   <- factanal(na.omit(trgt_2), factors = 2, rotation = "varimax")
fit2
fit_load2 <- as.matrix(round(loadings(fit2)[], digits = 2))

fit_load2 <-
  fit_load2 %>%
  as.data.frame() %>%
  mutate(Factor1 = case_when(Factor1 > Factor2 ~ Factor1),
         Factor2 = case_when(is.na(Factor1) ~ Factor2))



write.table(fit_load2, sep = ";",quote = FALSE)

### Aggregate ----

#LIBERAL TARGETS
ta_lft.itm.idx <- match(row.names(data.frame(fit_load2)[!is.na(data.frame(fit_load2)$Factor2),]),names(ds2))

psych::alpha(ds2[,ta_lft.itm.idx], check.keys = T)
ds2$ta_lft  <- rowMeans(ds2[,ta_lft.itm.idx], na.rm = T)

#CONSERVATIVE TARGETS
ta_rght.itm.idx <- match(row.names(data.frame(fit_load2)[!is.na(data.frame(fit_load2)$Factor1),]),names(ds2))

psych::alpha(ds2[,ta_rght.itm.idx])
ds2$ta_rght <- rowMeans(ds2[,ta_rght.itm.idx], na.rm = T)

# ANALYSES -----

## Predictor Descriptives ----

psych::describe(ds2$rwa)
psych::describe(ds2$nga)
psych::describe(ds2$sdo)
psych::describe(ds2$con)
psych::describe(ds2$mi)
psych::describe(ds2$polid)
psych::describe(ds2$pana)
psych::describe(ds2$hueg)
psych::describe(ds2$id)

## Predictor Correlations ----

#write.table(
round(
  cor(ds2[,c("rwa","nga","sdo","con","mi","pana","hueg","id","polid")], use = "pairwise", method = "pearson")
  ,2)
#,quote = FALSE, sep = ";")

## Single Target Correlations ----

round(cor(ds2[trgt.idx],ds2$rwa, use = "pairwise", method = "pearson"),2)
round(cor(ds2[trgt.idx],ds2$nga, use = "pairwise", method = "pearson"),2)
round(cor(ds2[trgt.idx],ds2$sdo, use = "pairwise", method = "pearson"),2)
round(cor(ds2[trgt.idx],ds2$con, use = "pairwise", method = "pearson"),2)
round(cor(ds2[trgt.idx],ds2$mi, use = "pairwise", method = "pearson"),2)
round(cor(ds2[trgt.idx],ds2$polid, use = "pairwise", method = "pearson"),2)

## Aggregated Target Correlations ----

cor(ds2$rwa,ds2$ta_rght, use = "pairwise", method = "pearson")
cor(ds2$rwa,ds2$ta_lft,  use = "pairwise", method = "pearson")

cor(ds2$nga,ds2$ta_rght, use = "pairwise", method = "pearson")
cor(ds2$nga,ds2$ta_lft,  use = "pairwise", method = "pearson")

cor(ds2$sdo,ds2$ta_rght, use = "pairwise", method = "pearson")
cor(ds2$sdo,ds2$ta_lft,  use = "pairwise", method = "pearson")

cor(ds2$ta_rght,ds2$ta_lft,  use = "pairwise", method = "pearson")

## Partial Correlations -----

#indices
rwa.idx          <- which(names(ds2) == "rwa")
nga.idx          <- which(names(ds2) == "nga")
con.idx          <- which(names(ds2) == "con")
sdo.idx          <- which(names(ds2) == "sdo")
mi.idx           <- which(names(ds2) == "mi")
hueg.idx         <- which(names(ds2) == "hueg")
pana.idx         <- which(names(ds2) == "pana")
polid.idx        <- which(names(ds2) == "polid")
ta_lft.idx       <- which(names(ds2) == "ta_lft")
ta_rght.idx      <- which(names(ds2) == "ta_rght")


### rwa|con ----

partCorr <- rep(NA,length(trgt.idx))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.idx ){
  m <- ds2[,c(rwa.idx, i, con.idx)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.idx)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds2[trgt.idx])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

### rwa|pana ----

partCorr <- rep(NA,length(trgt.idx))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.idx ){
  m <- ds2[,c(rwa.idx, i, pana.idx)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.idx)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds2[trgt.idx])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)


### sdo|con ----

partCorr <- rep(NA,length(trgt.idx))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.idx ){
  m <- ds2[,c(sdo.idx, i, con.idx)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.idx)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds2[trgt.idx])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

### sdo|mi ----

partCorr <- rep(NA,length(trgt.idx))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.idx ){
  m <- ds2[,c(sdo.idx, i, mi.idx)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.idx)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds2[trgt.idx])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)

### sdo|hueg ----

partCorr <- rep(NA,length(trgt.idx))
# korrelation von Skala und allen targets einzeln berechnen und andere Skalen
# rauspartialisieren
for ( i in trgt.idx ){
  m <- ds2[,c(sdo.idx, i, hueg.idx)]
  m <- m[complete.cases(m),]
  partCorr[which(i == trgt.idx)] <- psych::partial.r(m,1:2,3)[3]
} 

#named_numbers
names(partCorr) <- colnames(ds2[trgt.idx])
round(partCorr,2) %>% as_tibble(.,rownames="target") %>% print(n = 40)


## Split -----

pred_splt_2 <- ds2 %>%
  select(case,rwa,nga,sdo,polid)%>%
  filter(complete.cases(.)) %>%
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

ta_lng <- ds2 %>%
  select(case,all_of(trgt.idx)) %>%
  tidyr::pivot_longer(cols = 2:21, names_to = "target", values_to = "rating")

ta_lng <- left_join(pred_splt_2,ta_lng, by = "case")


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


### variation in targets vs individual differences -----

med_splt %>%
  filter(scale !="polid") %>%
  ungroup() %>%
  mutate(diff = abs(hgh-low)) %>%
  summarise(mean = mean(diff),
            sd   = sd(diff),
            range = range(diff))

trgt_2 %>%
  tidyr::pivot_longer(1:20, names_to = "target", values_to = "rating") %>%
  group_by(target) %>%
  mutate(rating = rating - 1) %>%
  summarise(mean = mean(rating, na.rm = T)) %>%
  mutate(sd = sd(mean)) %>%
  summarise(mean = mean(mean)) #%>%
summarise(range = range(mean))

write.table(tbl6, row.names = F, quote = F, sep = ";")


### MLM ----

mlm2 <- ds2 %>%
  select(case,rwa,con,all_of(trgt.idx)) %>%
  tidyr::pivot_longer(cols      = 4:23,
                      names_to  = "target",
                      values_to = "rating")  %>%
  group_by(target) %>%
  mutate(cwc_rwa   = rwa - mean(rwa, na.rm = T),
         trgt_mean = mean(rating,na.rm = T)) %>%
  print(n = 80)


icc_ta2 <- lmerTest::lmer(rating ~ (1|target), data = mlm2, REML = F)
summary(icc_ta2)
performance::icc(icc_ta2)