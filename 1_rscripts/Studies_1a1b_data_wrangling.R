library(tidyverse)
library(psych)
library(purrr)
library(tibble)

# DEFINE FUNCTIONS ----

scale_descriptives <- function(data, prefixes,single_items) {
  
  map_dfr(prefixes, function(pref) {
    
    items <- data %>% 
      select(starts_with(pref))
    
    if (ncol(items) == 0) return(NULL)
    
    # Item-Deskriptiva
    item_desc <- psych::describe(items) %>% 
      as.data.frame() %>% 
      rownames_to_column("indicator") %>%
      mutate(
        scale = pref,
        raw_alpha = NA_real_
      ) %>%
      select(indicator,n,mean,sd,median,min,max,range,scale,raw_alpha)
    
    # Falls mehrere Items → Skalenwerte + Alpha
    if (ncol(items) > 1) {
      
      scale_mean <- rowMeans(items, na.rm = TRUE)
      alpha_val  <- psych::alpha(items)$total$raw_alpha
      
      scale_desc <- psych::describe(scale_mean) %>%
        as.data.frame() %>%
        rownames_to_column("indicator") %>%
        mutate(
          indicator = paste0(pref, "_scale"),
          scale = pref,
          raw_alpha = alpha_val
        ) %>%
        select(indicator,n,mean,sd,median,min,max,range,scale,raw_alpha)
      
      if (single_items == TRUE){
      bind_rows(item_desc, scale_desc)
      } else{
        scale_desc 
      }
    } else {
      
      item_desc 
      
    }
  })
}


# DATA WRANGLING ----

# STUDY 1 ----

## Get Data----
ds1_raw <- read.csv(file = './0_data/rwa_sdo_revisited_study_1a.csv', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-1","-9",NA))  %>%
  janitor::clean_names(.)


#get codebook
cdbk_1a <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 1) %>%
  mutate(variable = tolower(variable))


rename_vars.1a <- setNames(cdbk_1a %>%
                             pull(variable_label),
                           cdbk_1a %>%
                             pull(variable))

ds1_raw <- ds1_raw %>%
  rename_with(~ rename_vars.1a[.],.cols = all_of(names(rename_vars.1a)))

ds1_raw <- ds1_raw %>%
  select(case:mode,
         starts_with("rwa"),
         starts_with("sdo"),
         starts_with("prj"),
         age:ncol(.))


ds1_raw <- 
  ds1_raw %>%
  mutate(
    gender = factor(
      case_when(gender == 1 ~ "female",
                gender == 2 ~ "male",
                gender == 3 ~ "other",
                TRUE ~ NA)
    ),
    education = factor(
      education,
      levels = c(1,2,3,4,5,6,7),
      labels = c("isced_1","isced_2","isced_2","isced_3","isced_4","isced_6","isced_6")
    )
  )

## PREDICTORS ----

prdctrs <- c("rwa","sdo","polid")

#get items
rwa.itms <- ds1_raw %>% select(starts_with("rwa_")) %>% names()
sdo.itms <- ds1_raw %>% select(starts_with("sdo_")) %>% names()

###factor analysis ----
fa.parallel(na.omit(ds1_raw[,rwa.itms]),fm = "ml", fa = "fa", n.iter = 100)
fa_rwa.1a <- fa(na.omit(ds1_raw[,rwa.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")
fa_rwa.1a

fa.parallel(na.omit(ds1_raw[,sdo.itms]),fm = "ml", fa = "fa", n.iter = 100)
fa_sdo.1a <- fa(na.omit(ds1_raw[,sdo.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")
fa_sdo.1a

#descriptives and alphas ----
scale_descriptives(ds1_raw,c("rwa","sdo","polid"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

ds1_raw <- 
  ds1_raw %>%
  mutate(
    rwa = rowMeans(select(.,all_of(rwa.itms)),na.rm = T),
    sdo = rowMeans(select(.,all_of(sdo.itms)),na.rm = T)
  )


## DV: TARGETS ----

#recode
ds1_raw <- ds1_raw %>% mutate(across(starts_with("prj_"), ~ 12 - .))


#Targets
trgt.itms.1a <- 
  ds1_raw %>%
  select(starts_with("prj_")) %>%
  names()

### factor analysis -----

psych::fa.parallel(ds1_raw %>% select(starts_with("prj_")))

fa_trgts.1a <- 
  psych::fa(ds1_raw %>%
              select(starts_with("prj_")), 
            nfactors = 2, 
            rotate = "oblimin", 
            fm = "ml")

fa_trgts.1a <- 
  loadings(fa_trgts.1a)[] %>%
  data.frame() %>%
  mutate(ML2     = case_when(ML2 > ML1 ~ ML2),
         ML1     = case_when(is.na(ML2) ~ ML1),
         trgtgrp_factor = as.factor(case_when(is.na(ML2) ~ "conservative",
                                           TRUE ~ "liberal")))

sjPlot::tab_fa(ds1_raw %>%
                 select(starts_with("prj_")), 
               nmbr.fctr = 2, 
               rotation = "oblimin",
               method = "ml"
               #,file = "./2_tables/fa_study_1a.html"
               )

### descriptives ----
scale_descriptives(ds1_raw,trgt.itms.1a,single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

### AGGREGATE ----

#item names
prj_lib.grps.1a <- 
  fa_trgts.1a %>% filter(trgtgrp_factor == "liberal") %>% row.names() #LIBERAL TARGETS

prj_con.grps.1a <-
  fa_trgts.1a %>% filter(trgtgrp_factor == "conservative") %>% row.names() #CONSERVATIVE TARGETS


scale_descriptives(ds1_raw,"prj",single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

#put indicator for group factor in
ds1_raw <- 
  ds1_raw %>%
  rename_with(
    .cols = all_of(prj_lib.grps.1a),       
    .fn   = ~ str_replace_all(., "prj", "prj.lib")
  ) %>%
  rename_with(
    .cols = all_of(prj_con.grps.1a),       
    .fn   = ~ str_replace_all(., "prj", "prj.con")
  )

scale_descriptives(ds1_raw,c("prj.lib","prj.con"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

#get renamed target group items
prj_all.grps.1a <- ds1_raw %>% select(starts_with("prj.")) %>% names()
prj_lib.grps.1a <- ds1_raw %>% select(starts_with("prj.lib")) %>% names()
prj_con.grps.1a <- ds1_raw %>% select(starts_with("prj.con")) %>% names()

ds1_raw <- 
  ds1_raw %>%
  mutate(
    prj_agg.lib.grps = rowMeans(select(.,starts_with("prj.lib")),na.rm = T),
    prj_agg.con.grps = rowMeans(select(.,starts_with("prj.con")),na.rm = T)
  )


# WRITE ANALYSIS DATA SET ----

## only relevant variables

ds1_wde <- 
  ds1_raw %>%
  select(case,age,gender,education,
         all_of(c(prdctrs,prj_all.grps.1a)),
         prj_agg.lib.grps,prj_agg.con.grps)

ds1_lng <- 
  ds1_raw %>%
  select(case,age,gender,education,
         rwa,sdo,polid,
         starts_with("prj")) %>%
  pivot_longer(cols = starts_with("prj"),
               names_to = "prj_target",
               values_to = "prj_rating") %>%
  mutate(trgt_fct = case_when(str_detect(prj_target,"prj.lib") ~ "liberal",
                              str_detect(prj_target,"prj.con") ~ "conservative"),
         across(c("rwa","sdo","polid"),~as.numeric(scale(.)),.names = "{col}_scl")) %>%
  group_by(prj_target) %>%
  mutate(prj_rating_scl = as.numeric(scale(prj_rating)),
         prj_rating_gmc = prj_rating - mean(prj_rating,na.rm = T)) %>%
  ungroup()

#STUDY 2 ----

## get data----
ds2_raw <- read.csv(file = './0_data/rwa_sdo_revisited_study_1b.csv', 
                    header = TRUE, sep = ",", as.is = T, na.strings = c("-1","-9",NA))  %>%
  janitor::clean_names(.)


#get codebook
cdbk_1b <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 2) %>%
  mutate(variable = tolower(variable))


rename_vars.1b <- setNames(cdbk_1b %>%
                             pull(variable_label),
                           cdbk_1b %>%
                             pull(variable))

ds2_raw <- ds2_raw %>%
  rename_with(~ rename_vars.1b[.],.cols = all_of(names(rename_vars.1b)))

ds2_raw <- ds2_raw %>%
  select(case:mode,
         starts_with("rwa"),
         starts_with("sdo"),
         starts_with("prj"),
         gender:ncol(.))


ds2_raw <- 
  ds2_raw %>%
  mutate(
    gender = factor(
      case_when(gender == 1 ~ "female",
                gender == 2 ~ "male",
                gender == 3 ~ "other",
                TRUE ~ NA)
    ),
    education = factor(
      education,
      levels = c(1,2,3,4,5,6,7),
      labels = c("isced_1","isced_2","isced_2","isced_3","isced_4","isced_6","isced_6")
    )
  )

## PREDICTORS ----

prdctrs <- c("rwa","sdo","polid")

#get items
rwa.itms <- ds2_raw %>% select(starts_with("rwa_")) %>% names()
sdo.itms <- ds2_raw %>% select(starts_with("sdo_")) %>% names()

###factor analysis ----
fa.parallel(na.omit(ds2_raw[,rwa.itms]),fm = "ml", fa = "fa", n.iter = 100)
fa_rwa.1b <- fa(na.omit(ds2_raw[,rwa.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")
fa_rwa.1b

fa.parallel(na.omit(ds2_raw[,sdo.itms]),fm = "ml", fa = "fa", n.iter = 100)
fa_sdo.1b <- fa(na.omit(ds2_raw[,sdo.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")
fa_sdo.1b

#descriptives and alphas ----
scale_descriptives(ds2_raw,c("rwa","sdo","polid"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

ds2_raw <- 
  ds2_raw %>%
  mutate(
    rwa = rowMeans(select(.,all_of(rwa.itms)),na.rm = T),
    sdo = rowMeans(select(.,all_of(sdo.itms)),na.rm = T)
  )


## DV: TARGETS ----

#recode
ds2_raw <- ds2_raw %>% mutate(across(starts_with("prj_"), ~ 102 - .))


#Targets
trgt.itms.1b <- 
  ds2_raw %>%
  select(starts_with("prj_")) %>%
  names()

### factor analysis -----

psych::fa.parallel(ds2_raw %>% select(starts_with("prj_")))

fa_trgts.1b <- 
  psych::fa(ds2_raw %>%
              select(starts_with("prj_")), 
            nfactors = 2, 
            rotate = "oblimin", 
            fm = "ml")

fa_trgts.1b <- 
  loadings(fa_trgts.1b)[] %>%
  data.frame() %>%
  mutate(ML2     = case_when(ML2 > ML1 ~ ML2),
         ML1     = case_when(is.na(ML2) ~ ML1),
         trgtgrp_factor = as.factor(case_when(is.na(ML2) ~ "conservative",
                                              TRUE ~ "liberal")))

sjPlot::tab_fa(ds2_raw %>%
                 select(starts_with("prj_")), 
               nmbr.fctr = 2, 
               rotation = "oblimin",
               method = "ml"
               #,file = "./2_tables/fa_study_1b.html"
)

### descriptives ----
scale_descriptives(ds2_raw,trgt.itms.1b,single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

### AGGREGATE ----

#item names
prj_lib.grps.1b <- 
  fa_trgts.1b %>% filter(trgtgrp_factor == "liberal") %>% row.names() #LIBERAL TARGETS

prj_con.grps.1b <-
  fa_trgts.1b %>% filter(trgtgrp_factor == "conservative") %>% row.names() #CONSERVATIVE TARGETS


scale_descriptives(ds2_raw,"prj",single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

#put indicator for group factor in
ds2_raw <- 
  ds2_raw %>%
  rename_with(
    .cols = all_of(prj_lib.grps.1b),       
    .fn   = ~ str_replace_all(., "prj", "prj.lib")
  ) %>%
  rename_with(
    .cols = all_of(prj_con.grps.1b),       
    .fn   = ~ str_replace_all(., "prj", "prj.con")
  )

scale_descriptives(ds2_raw,c("prj.lib","prj.con"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

#get renamed target group items
prj_all.grps.1b <- ds2_raw %>% select(starts_with("prj.")) %>% names()
prj_lib.grps.1b <- ds2_raw %>% select(starts_with("prj.lib")) %>% names()
prj_con.grps.1b <- ds2_raw %>% select(starts_with("prj.con")) %>% names()

ds2_raw <- 
  ds2_raw %>%
  mutate(
    prj_agg.lib.grps = rowMeans(select(.,starts_with("prj.lib")),na.rm = T),
    prj_agg.con.grps = rowMeans(select(.,starts_with("prj.con")),na.rm = T)
  )


# WRITE ANALYSIS DATA SET ----

## only relevant variables

ds2_wde <- 
  ds2_raw %>%
  select(case,age,gender,education,
         all_of(c(prdctrs,prj_all.grps.1b)),
         prj_agg.lib.grps,prj_agg.con.grps)
