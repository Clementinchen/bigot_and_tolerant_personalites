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

## get data----
ds1 <- read.csv('https://osf.io/ywpq4/download', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-9",NA)) %>%
  janitor::clean_names(.)

ds1 <- read.csv(file = './0_data/rwa_sdo_revisited_study_1a.csv', 
                header = TRUE, sep = ",", as.is = T, na.strings = c("-1","-9",NA))  %>%
  janitor::clean_names(.)


#get codebook
cdbk_1a <- openxlsx::read.xlsx("./0_data/three_challenges_codebook.xlsx", sheet = 1) %>%
  mutate(variable = tolower(variable))


rename_vars.1a <- setNames(cdbk_1a %>%
                             pull(variable_label),
                           cdbk_1a %>%
                             pull(variable))

ds1 <- ds1 %>%
  rename_with(~ rename_vars.1a[.],.cols = all_of(names(rename_vars.1a)))

ds1 <- ds1 %>%
  select(case:mode,
         starts_with("rwa"),
         starts_with("sdo"),
         starts_with("prj"),
         age:ncol(.))


ds1 <- 
  ds1 %>%
  mutate(
    gender = factor(
      gender,
      levels = c(1,2,3,4),
      labels = c("female","male","other",NA)
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
rwa.itms <- ds1 %>% select(starts_with("rwa_")) %>% names()
sdo.itms <- ds1 %>% select(starts_with("sdo_")) %>% names()

#factor analysis
fa_rwa.1a <- factanal(na.omit(ds1[,rwa.itms]), factors = 1, rotation = "oblimin")
fa_rwa.1a

fa_sdo.1a <- factanal(na.omit(ds1[,sdo.itms]), factors = 2, rotation = "varimax")
fa_sdo.1a

#descriptives and alpha
scale_descriptives(ds1,c("rwa","sdo","polid"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

ds1 <- 
  ds1 %>%
  mutate(
    rwa = rowMeans(select(.,rwa.itms),na.rm = T),
    sdo = rowMeans(select(.,sdo.itms),na.rm = T)
  )


## DV: TARGETS ----

#recode
ds1 <- ds1 %>% mutate(across(starts_with("prj_"), ~ 12 - .))


#Targets
trgt.itms.1a <- 
  ds1 %>%
  select(starts_with("prj_")) %>%
  names()

### FACTOR ANALYSIS -----

psych::fa.parallel(ds1 %>% select(starts_with("prj_")))

fa_trgts.1a <- 
  psych::fa(ds1 %>%
              select(starts_with("prj_")), 
            nfactors = 2, 
            rotate = "oblimin", 
            fm = "ml")

fa_trgts.1a <- 
  loadings(fa_trgts.1a)[] %>%
  data.frame() %>%
  mutate(ML2     = case_when(ML2 > ML1 ~ ML2),
         ML1     = case_when(is.na(ML2) ~ ML1),
         trgt_factor = as.factor(case_when(is.na(ML2) ~ "liberal",
                                           TRUE ~ "conservative")))

sjPlot::tab_fa(ds1 %>%
                 select(starts_with("prj_")), 
               nmbr.fctr = 2, 
               rotation = "oblimin",
               method = "ml",
               file = "./2_tables/fa_study_1a.html")

### DESCRIPTIVES ----
scale_descriptives(ds1,trgt.itms.1a,single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

### AGGREGATE ----

#item names
prj_lib.trgts.1a <- 
  fa_trgts.1a %>% filter(trgt_factor == "liberal") %>% row.names() #LIBERAL TARGETS

prj_con.trgts.1a <-
  fa_trgts.1a %>% filter(trgt_factor == "conservative") %>% row.names() #CONSERVATIVE TARGETS


scale_descriptives(ds1,"prj",single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))


ds1 <- 
  ds1 %>%
  rename_with(
    .cols = all_of(prj_lib.trgts.1a),       
    .fn   = ~ str_replace_all(., "prj", "prj.lib")
  ) %>%
  rename_with(
    .cols = all_of(prj_con.trgts.1a),       
    .fn   = ~ str_replace_all(., "prj", "prj.con")
  )

prj_lib.trgts.1a <- 
  fa_trgts.1a %>% filter(trgt_factor == "liberal") %>% row.names() #LIBERAL TARGETS

prj_con.trgts.1a <-
  fa_trgts.1a %>% filter(trgt_factor == "conservative") %>% row.names() #CONSERVATIVE TARGETS

scale_descriptives(ds1,c("prj.lib","prj.con"),single_items = TRUE) %>% as_tibble() %>% print(n = nrow(.))

ds1 <- 
  ds1 %>%
  mutate(
    prj_liberal      = rowMeans(select(.,starts_with("prj.lib")),na.rm = T),
    prj_conservative = rowMeans(select(.,starts_with("prj.con")),na.rm = T)
  )

prj.trgts.1a     <- ds1 %>% select(starts_with("prj."))    %>% names() #ALL TARGETS
prj_lib.trgts.1a <- ds1 %>% select(starts_with("prj.lib")) %>% names() #LIBERAL TARGETS
prj_con.trgts.1a <- ds1 %>% select(starts_with("prj.con")) %>% names() #CONSERVATIVE TARGETS


ds1_ana <- 
  ds1 %>%
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
