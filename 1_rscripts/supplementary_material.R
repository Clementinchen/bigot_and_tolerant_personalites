# Functions ----

### helper function
get_corr <- function(dat, predictor, prejudice, split, centering){
  
  out <- tryCatch({
    
    tmp <- trgt_var(
      data = dat,
      predictor = predictor,
      prejudice = prejudice,
      pred_split = split,
      trgt_centering = centering
    )$data
    
    high <- deframe(tmp["high"])
    low  <- deframe(tmp["low"])
    
    ct <- cor.test(high, low)
    
    r  <- sprintf("%.4f", unname(ct$estimate))
    lo <- sprintf("%.4f", ct$conf.int[1])
    hi <- sprintf("%.4f", ct$conf.int[2])
    
    paste0(r, " [", lo, ", ", hi, "]")
    
  }, error = function(e){
    
    NA_character_
    
  })
  
  return(out)
}


get_iccs <- function(m) {
  
  sigma2 <- getME(m, "sigma")^2
  
  taus <- (getME(m, "theta") * getME(m, "sigma"))^2
  names(taus) <- names(getME(m, "cnms"))
  
  total_var <- sigma2 + sum(taus)
  
  ICCs <- taus / total_var
  
  c(
    sigma2 = sigma2,
    setNames(taus, paste0("tau2_", names(taus))),
    setNames(ICCs, paste0("ICC_", names(ICCs))),
    vp_residual = sigma2 / total_var
  )
}


cntry_corrs <- 
  function(data,cluster,split.var,prj.items,split.stat,by.country) {
    
    #get split stats
    
    splt_mean   <- psych::describe(data[{{ split.var }}])$mean
    splt_median <- psych::describe(data[{{ split.var }}])$median
    splt_sd     <- psych::describe(data[{{ split.var }}])$sd
    
    # prepare data
    data <- data[,c({{ split.var }},{{ cluster }},prj.items)]
    
    # long format
    data <-
      data %>%
      pivot_longer(cols = all_of(prj.items),names_to = "dv.names",values_to = "dv.rating")
    
    # split into subgroups
    data <- 
      data %>%
      mutate(
        #mean split
        mean = case_when(!!sym(split.var) >= splt_mean ~ "mean_high",
                         !!sym(split.var) <  splt_mean ~ "mean_low"),
        #median split
        median = case_when(!!sym(split.var) >= splt_median ~ "median_high",
                           !!sym(split.var) <  splt_median ~ "median_low"),
        #+-1sd split
        "1sd" = case_when(!!sym(split.var) > splt_median+splt_sd ~ "1sd_high",
                          !!sym(split.var) < splt_median-splt_sd ~ "1sd_low")
        
      )
    
    # calculate target means in cluster & subgroups
    data <- 
      bind_rows(
        
        data %>%
          group_by(!!sym(cluster),dv.names) %>%
          summarise(m_clstr = mean(dv.rating,na.rm = T),.groups = "drop"),
        
        data %>%
          group_by(!!sym(cluster),dv.names,!!sym(split.stat)) %>%
          summarise(m_clstr = mean(dv.rating,na.rm = T),.groups = "drop") %>%
          filter(!is.na(!!sym(split.stat)))
      )
    
    data <- 
      data %>%
      rename("subgroup" = {{ split.stat }}) %>%
      mutate(subgroup  = ifelse(is.na(subgroup),"none",subgroup))
    
    # transform data back to wide format to calculate within-cluster correlation
    wthn_clstr <-
      data %>%
      filter(subgroup != "none") %>%
      pivot_wider(names_from = subgroup,values_from = "m_clstr")
    
    #calculate within country correlation
    cor_within_cluster <-
      wthn_clstr %>%
      correlation::correlation() %>%
      as_tibble()
    
    cor_within_cluster <- 
      cor_within_cluster %>%
      mutate(across(c(r,CI_low,CI_high),~round(.,2)),
             "95% CI" = paste0("[",CI_low,", ",CI_high,"]")) %>%
      select(1:3,12,11,9)
    
    #calculate between country correlation
    btwn_clstr_corrs <- 
      data %>%
      pivot_wider(names_from = !!sym(cluster), values_from = "m_clstr") %>%
      group_by(subgroup) %>%
      correlation::correlation() %>%
      as_tibble() %>%
      pivot_longer(cols = c("Parameter1","Parameter2"), names_to = "parameter",values_to = "cluster")
    
    if (by.country == TRUE) {
      group_var <- c("Group", "cluster")
    } else if (by.country == FALSE) {
      group_var <- "Group"
    }
    
    btwn_clstr_corrs <- 
      btwn_clstr_corrs %>%
      group_by(!!! syms(group_var)) %>%
      summarise(
        mean_r = mean(r, na.rm = TRUE),
        z_vals = list(atanh(r)),
        .groups = "drop"
      )  %>%
      rowwise() %>%
      mutate(
        z_mean = mean(unlist(z_vals)),
        z_se = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
        z_crit = qnorm(0.975),
        z_ci_lower = z_mean - z_crit * z_se,
        z_ci_upper = z_mean + z_crit * z_se,
        r_mean = tanh(z_mean),
        r_ci_lower = tanh(z_ci_lower),
        r_ci_upper = tanh(z_ci_upper)
      ) %>% ungroup()
    
    btwn_clstr_corrs <- 
      btwn_clstr_corrs %>%
      mutate(across(c(z_mean,r_mean,r_ci_lower,r_ci_upper),~round(.,2)),
             CI95 = paste0("[",r_ci_lower,", ",r_ci_upper,"]")) %>%
      select(all_of(group_var),z_mean,r_mean,ncol(.)) %>%
      rename("Average_z" = "z_mean","Average_r"="r_mean")
    
    cat("Within-Cluster Correlation Between Subgroups.\nCluster:",toupper({{ cluster }}),
        "\nSubgroups Split Variable:", toupper({{ split.var }}),
        "\nSplit Metric:",toupper({{ split.stat }}),"\n\n")
    print(cor_within_cluster)
    
    cat("\n\nAveraged Between-Cluster Correlation Within Subgroups.\nCluster:",toupper({{ cluster }}),
        "\nSubgroups Split Variable:", toupper({{ split.var }}),
        "\nSplit Metric:",toupper({{ split.stat }}),"\n\n")
    
    print(btwn_clstr_corrs, n = nrow(btwn_clstr_corrs))
    
    
  }



# Appendix A: STUDIES 1a & 1b ----

## Materials ----

### Table A1 ----

cdbk_1a %>%
  filter(scale == "RWA") %>%
  select(german_item_original,english_item_translation,subscale) %>%
  mutate(subscale = str_replace_all(subscale,"_"," "),
         subscale = str_to_title(subscale)) %>%
  sjPlot::tab_df()

### Table A2 ----

cdbk_1a %>%
  filter(scale == "SDO") %>%
  select(german_item_original,english_item_translation, coding) %>%
  mutate(coding = case_when(is.na(coding) ~ "pro-trait",TRUE ~ "con-trait")) %>%
  sjPlot::tab_df()

### Table A3 ----

cdbk_1a %>%
  filter(scale == "Prejudice") %>%
  select(variable_label,german_item_original,english_item_translation) %>%
  mutate(Factor = case_when(variable_label %in% str_replace_all(prj_con.grps.1a,".con","") ~ "Conservative",
                            TRUE ~ "Liberal"),
         Only_Study_1a = case_when(variable_label %in% str_replace_all(prj_all.grps.1b,c(".con|.lib"),"") ~ "x",
                                   TRUE ~ " ")) %>%
  select(-variable_label) %>%
  sjPlot::tab_df()


### Table A4 ----

fa_rwa.1a <- fa(na.omit(ds1_raw[,rwa.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")
fa_rwa.1b <- fa(na.omit(ds2_raw[,rwa.itms]), nfactors = 1, rotate = "oblimin", fm = "ml")

tab_fa_rwa <- 
  full_join(
  loadings(fa_rwa.1a)[] %>% data.frame() %>% rename("Study1a" = "ML1") %>% rownames_to_column("variable_label"),
  loadings(fa_rwa.1b)[] %>% data.frame() %>% rename("Study1b" = "ML1") %>% rownames_to_column("variable_label"),
  by = "variable_label"
) 

tab_fa_rwa <- 
  cdbk_1a %>% filter(scale == "RWA") %>% select(variable_label,english_item_translation) %>%
  full_join(.,tab_fa_rwa, by = "variable_label") %>%
  select(english_item_translation,Study1a,Study1b) 

tab_fa_rwa %>%
  add_row(english_item_translation ="Alpha", 
          Study1a = as.numeric(psych::alpha(ds1_raw[rwa.itms],warnings = F)$total[1]),
          Study1b = as.numeric(psych::alpha(ds2_raw[rwa.itms],warnings = F)$total[1])) %>%
  mutate(across(where(is.numeric),~round(.,2))) %>%
  sjPlot::tab_df()

### Table A5 ----

tab_fa_sdo <- 
  full_join(
    loadings(fa_sdo.1a)[] %>% data.frame() %>% rename("Study1a" = "ML1") %>% rownames_to_column("variable_label"),
    loadings(fa_sdo.1b)[] %>% data.frame() %>% rename("Study1b" = "ML1") %>% rownames_to_column("variable_label"),
    by = "variable_label"
  ) 

tab_fa_sdo <- 
  cdbk_1a %>% filter(scale == "SDO") %>% select(variable_label,english_item_translation) %>%
  full_join(.,tab_fa_sdo, by = "variable_label") %>%
  select(english_item_translation,Study1a,Study1b) 

tab_fa_sdo %>%
  add_row(english_item_translation ="Alpha", 
          Study1a = as.numeric(psych::alpha(ds1_raw[sdo.itms],warnings = F)$total[1]),
          Study1b = as.numeric(psych::alpha(ds2_raw[sdo.itms],warnings = F)$total[1])) %>%
  mutate(across(where(is.numeric),~round(.,2))) %>%
  sjPlot::tab_df()

### Table A6 ----

tab_fa_trgts <- 
  full_join(
    fa_trgts.1a %>% data.frame() %>% select(-trgtgrp_factor) %>% rename("Study1a_fct1" = "ML1","Study1a_fct2" = "ML2") %>% rownames_to_column("variable_label"),
    fa_trgts.1b %>% data.frame() %>% select(-trgtgrp_factor) %>% rename("Study1b_fct1" = "ML1","Study1b_fct2" = "ML2") %>% rownames_to_column("variable_label"),
    by = "variable_label"
  ) 

tab_fa_trgts <- 
  cdbk_1a %>% filter(scale == "Prejudice") %>% select(variable_label,english_item_translation) %>%
  full_join(.,tab_fa_trgts, by = "variable_label") %>%
  select(english_item_translation,Study1a_fct1,Study1a_fct2,Study1b_fct1,Study1b_fct2) 

tab_fa_trgts %>%
  add_row(english_item_translation ="Alpha", 
          Study1a_fct1 = as.numeric(psych::alpha(ds1_raw[prj_con.grps.1a],warnings = F)$total[1]),
          Study1a_fct2 = as.numeric(psych::alpha(ds1_raw[prj_lib.grps.1a],warnings = F)$total[1]),
          Study1b_fct1 = as.numeric(psych::alpha(ds2_raw[prj_con.grps.1b],warnings = F)$total[1]),
          Study1b_fct2 = as.numeric(psych::alpha(ds2_raw[prj_lib.grps.1b],warnings = F)$total[1])) %>%
  mutate(across(where(is.numeric),~round(.,2))) %>%
  sjPlot::tab_df()

### Table A7 ----

ds1_wde %>%
  select(all_of(prdctrs),starts_with("prj")) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  filter(complete.cases(.)) %>%
  group_by(variable) %>%
  summarise(
    N = n(),
    Mean = mean(value, na.rm = T),
    SD = sd(value, na.rm = T),
    Median = median(value, na.rm = T),
    Min = min(value,na.rm = T),
    Max = max(value, na.rm = T),
    range = abs(Max-Min)) %>%
  mutate(
    'Target_Group' = case_when(str_detect(variable,"prj.con") ~ "Conservative",
                               str_detect(variable,"prj.lib") ~ "Liberal",
                               TRUE ~ NA),
    variable = str_remove_all(variable,c("prj.con_|prj.lib_|prj_agg.")),
    variable = str_replace_all(variable,"_"," "),
    variable = str_to_title(variable)
  ) %>%
  arrange(!is.na(Target_Group),Target_Group) %>%
  sjPlot::tab_df()

### Table A8 ----

ds2_wde %>%
  select(all_of(prdctrs),starts_with("prj")) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  filter(complete.cases(.)) %>%
  group_by(variable) %>%
  summarise(
    N = n(),
    Mean = mean(value, na.rm = T),
    SD = sd(value, na.rm = T),
    Median = median(value, na.rm = T),
    Min = min(value,na.rm = T),
    Max = max(value, na.rm = T),
    range = abs(Max-Min)) %>%
  mutate(
    'Target_Group' = case_when(str_detect(variable,"prj.con") ~ "Conservative",
                               str_detect(variable,"prj.lib") ~ "Liberal",
                               TRUE ~ NA),
    variable = str_remove_all(variable,c("prj.con_|prj.lib_|prj_agg.")),
    variable = str_replace_all(variable,"_"," "),
    variable = str_to_title(variable)
  ) %>%
  arrange(!is.na(Target_Group),Target_Group) %>%
  sjPlot::tab_df()

### Table A9 ----

tab_bivr_rwa <-
  ds1_wde %>%
  select(rwa,all_of(prj_all.grps.1a)) %>%
  pivot_longer(cols = all_of(prj_all.grps.1a), names_to = "target",values_to = "rating") %>%
  filter(complete.cases(.)) %>%
  mutate(Study = "Study1a") %>%
  bind_rows(.,
            ds2_wde %>%
              select(rwa,all_of(prj_all.grps.1b)) %>%
              pivot_longer(cols = all_of(prj_all.grps.1b), names_to = "target",values_to = "rating") %>%
              filter(complete.cases(.)) %>%
              mutate(Study = "Study1b")
            ) %>%
  group_by(target,Study) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)
  ) %>%
  mutate(
    across(where(is.numeric),~round(.,2)),
    M_SD = paste0(mean," (",sd,")")) %>%
  pivot_wider(names_from = "Study", values_from = c(n,mean,sd,M_SD),names_glue = "{Study}_{.value}") %>%
  ungroup()

tab_bivr_rwa <- 
  tab_bivr_rwa %>%
    full_join(.,
              ds1_wde %>%
                select(rwa,all_of(prj_all.grps.1a)) %>%
                pivot_longer(cols = all_of(prj_all.grps.1a), names_to = "target",values_to = "rating") %>%
                group_by(target) %>%
                correlation::correlation() %>%
                as_tibble() %>%
                mutate(across(c(r,CI_low,CI_high),~round(.,2)),
                       CI95 = paste0("[",CI_low,", ",CI_high,"]")) %>%
                rename("target" = "Group") %>%
                select(target,r,CI95,p) %>%
                rename_at(vars(r:p), ~ paste0("Study1a_",.))
                
    )

tab_bivr_rwa <- 
  tab_bivr_rwa %>%
  full_join(.,
            ds2_wde %>%
              select(rwa,all_of(prj_all.grps.1b)) %>%
              pivot_longer(cols = all_of(prj_all.grps.1b), names_to = "target",values_to = "rating") %>%
              group_by(target) %>%
              correlation::correlation() %>%
              as_tibble() %>%
              mutate(across(c(r,CI_low,CI_high),~round(.,2)),
                     CI95 = paste0("[",CI_low,", ",CI_high,"]")) %>%
              rename("target" = "Group") %>%
              select(target,r,CI95,p) %>%
              rename_at(vars(r:p), ~ paste0("Study1b_",.))
            
  )

tab_bivr_rwa %>%
  mutate(target = str_remove_all(target,c("prj.con_|prj.lib_")),
         target = str_replace_all(target,"_"," "),
         target = str_to_title(target)) %>%
  select(target,
         Study1a_n,Study1a_M_SD,Study1a_r,Study1a_CI95,Study1a_p,
         Study1b_n,Study1b_M_SD,Study1b_r,Study1b_CI95,Study1b_p) %>%
  sjPlot::tab_df()


### Table A10 ----

tab_bivr_sdo <-
  ds1_wde %>%
  select(sdo,all_of(prj_all.grps.1a)) %>%
  pivot_longer(cols = all_of(prj_all.grps.1a), names_to = "target",values_to = "rating") %>%
  filter(complete.cases(.)) %>%
  mutate(Study = "Study1a") %>%
  bind_rows(.,
            ds2_wde %>%
              select(sdo,all_of(prj_all.grps.1b)) %>%
              pivot_longer(cols = all_of(prj_all.grps.1b), names_to = "target",values_to = "rating") %>%
              filter(complete.cases(.)) %>%
              mutate(Study = "Study1b")
  ) %>%
  group_by(target,Study) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)
  ) %>%
  mutate(
    across(where(is.numeric),~round(.,2)),
    M_SD = paste0(mean," (",sd,")")) %>%
  pivot_wider(names_from = "Study", values_from = c(n,mean,sd,M_SD),names_glue = "{Study}_{.value}") %>%
  ungroup() 

tab_bivr_sdo <- 
  tab_bivr_sdo %>%
  full_join(.,
            ds1_wde %>%
              select(sdo,all_of(prj_all.grps.1a)) %>%
              pivot_longer(cols = all_of(prj_all.grps.1a), names_to = "target",values_to = "rating") %>%
              group_by(target) %>%
              correlation::correlation() %>%
              as_tibble() %>%
              mutate(across(c(r,CI_low,CI_high),~round(.,2)),
                     CI95 = paste0("[",CI_low,", ",CI_high,"]")) %>%
              rename("target" = "Group") %>%
              select(target,r,CI95,p) %>%
              rename_at(vars(r:p), ~ paste0("Study1a_",.))
            
  )

tab_bivr_sdo <- 
  tab_bivr_sdo %>%
  full_join(.,
            ds2_wde %>%
              select(sdo,all_of(prj_all.grps.1b)) %>%
              pivot_longer(cols = all_of(prj_all.grps.1b), names_to = "target",values_to = "rating") %>%
              group_by(target) %>%
              correlation::correlation() %>%
              as_tibble() %>%
              mutate(across(c(r,CI_low,CI_high),~round(.,2)),
                     CI95 = paste0("[",CI_low,", ",CI_high,"]")) %>%
              rename("target" = "Group") %>%
              select(target,r,CI95,p) %>%
              rename_at(vars(r:p), ~ paste0("Study1b_",.))
            
  )

tab_bivr_sdo %>%
  mutate(target = str_remove_all(target,c("prj.con_|prj.lib_")),
         target = str_replace_all(target,"_"," "),
         target = str_to_title(target)) %>%
  select(target,
         Study1a_n,Study1a_M_SD,Study1a_r,Study1a_CI95,Study1a_p,
         Study1b_n,Study1b_M_SD,Study1b_r,Study1b_CI95,Study1b_p) %>%
  sjPlot::tab_df()

### Table A11 ----

#Srudy 1a
tab_splt_med1a_raw <- 
  trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")$data %>%
  mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
  rename_at(vars(high:absolute_diff),~ paste0("RWA_Split_",.)) %>%
  full_join(.,
            trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")$data %>%
              mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
              rename_at(vars(high:absolute_diff),~ paste0("SDO_Split_",.))) %>%
  arrange(-RWA_Split_high)
  
tab_splt_med1a_raw <- 
  tab_splt_med1a_raw %>%
  add_row(
    tab_splt_med1a_raw %>%
      summarise(across(where(is.numeric),mean)) %>%
      mutate(prejudice_target = "Mean")
  )

tab_splt_med1a_raw <- 
  tab_splt_med1a_raw %>%
  add_row(
    tab_splt_med1a_raw %>%
      summarise(across(where(is.numeric),sd)) %>%
      mutate(prejudice_target = "SD")
  )

tab_splt_med1a_raw %>%
  sjPlot::tab_df()


#Study 1b
tab_splt_med1b_raw <- 
  trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")$data %>%
  mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
  rename_at(vars(high:absolute_diff),~ paste0("RWA_Split_",.)) %>%
  full_join(.,
            trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")$data %>%
              mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
              rename_at(vars(high:absolute_diff),~ paste0("SDO_Split_",.))) %>%
  arrange(-RWA_Split_high)

tab_splt_med1b_raw <- 
  tab_splt_med1b_raw %>%
  add_row(
    tab_splt_med1b_raw %>%
      summarise(across(where(is.numeric),mean)) %>%
      mutate(prejudice_target = "Mean")
  )

tab_splt_med1b_raw <- 
  tab_splt_med1b_raw %>%
  add_row(
    tab_splt_med1b_raw %>%
      summarise(across(where(is.numeric),sd)) %>%
      mutate(prejudice_target = "SD")
  )

tab_splt_med1b_raw %>%
  sjPlot::tab_df()

trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")


### Table A12 ----

#Study 1a
tab_splt_med1a_cwc <- 
  trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")$data %>%
  mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
  rename_at(vars(high:absolute_diff),~ paste0("RWA_Split_",.)) %>%
  full_join(.,
            trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")$data %>%
              mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
              rename_at(vars(high:absolute_diff),~ paste0("SDO_Split_",.))) %>%
  arrange(RWA_Split_high)

tab_splt_med1a_cwc <- 
  tab_splt_med1a_cwc %>%
  add_row(
    tab_splt_med1a_cwc %>%
      summarise(across(where(is.numeric),mean)) %>%
      mutate(prejudice_target = "Mean")
  )

tab_splt_med1a_cwc <- 
  tab_splt_med1a_cwc %>%
  add_row(
    tab_splt_med1a_cwc %>%
      summarise(across(where(is.numeric),sd)) %>%
      mutate(prejudice_target = "SD")
  )

tab_splt_med1a_cwc %>%
  sjPlot::tab_df()


#Study 1b
tab_splt_med1b_cwc <- 
  trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")$data %>%
  mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
  rename_at(vars(high:absolute_diff),~ paste0("RWA_Split_",.)) %>%
  full_join(.,
            trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")$data %>%
              mutate(prejudice_target = str_remove_all(prejudice_target,c("Prj |Con |Lib "))) %>%
              rename_at(vars(high:absolute_diff),~ paste0("SDO_Split_",.))) %>%
  arrange(RWA_Split_high)

tab_splt_med1b_cwc <- 
  tab_splt_med1b_cwc %>%
  add_row(
    tab_splt_med1b_cwc %>%
      summarise(across(where(is.numeric),mean)) %>%
      mutate(prejudice_target = "Mean")
  )

tab_splt_med1b_cwc <- 
  tab_splt_med1b_cwc %>%
  add_row(
    tab_splt_med1b_cwc %>%
      summarise(across(where(is.numeric),sd)) %>%
      mutate(prejudice_target = "SD")
  )

tab_splt_med1b_cwc %>%
  sjPlot::tab_df()

trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")

### Table A13 ----
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

corrs_vars   <- c("rwa","sdo","polid")
corrs_splits <- c("median","mean","1sd","2sd")
corrs_cent   <- c("none","cwc","gmc","z")
corrs_stdys  <- c("ds1_wde","ds2_wde")


### loop
results <- list()
counter <- 1

for (study in corrs_stdys) {
  
  dat <- get(study)
  
  prejudice <- if (study == "ds1_wde") {
    prj_all.grps.1a
  } else {
    prj_all.grps.1b
  }
  
  for (var in corrs_vars) {
    
    for (split in corrs_splits) {
      
      row_res <- data.frame(
        study = study,
        predictor = var,
        split_metric = split
      )
      
      # centering methods become columns
      for (cent in corrs_cent) {
        
        row_res[[cent]] <- get_corr(
          dat = dat,
          predictor = var,
          prejudice = prejudice,
          split = split,
          centering = cent
        )
      }
      
      results[[counter]] <- row_res
      counter <- counter + 1
    }
  }
}

### final dataframe
results_df <- bind_rows(results)

results_df

results_df %>%
  sjPlot::tab_df()

### Table A15 ----

var_decomp_1a <- 
  ds1_wde %>%
  select(case,rwa,sdo,starts_with("prj.")) %>%
  tidyr::pivot_longer(cols = starts_with("prj"),
                      names_to = "target",
                      values_to = "rating") %>%
  filter(complete.cases(.))

mod_rwa_1a_1 = lmer(rating ~ 1 + (1|case)                       , data = var_decomp_1a)
mod_rwa_1a_2 = lmer(rating ~ 1 + (1|case) + (1|rwa)             , data = var_decomp_1a)
mod_rwa_1a_3 = lmer(rating ~ 1 + (1|case) + (1|rwa) + (1|target), data = var_decomp_1a)

sjPlot::tab_model(
  mod_rwa_1a_1,
  mod_rwa_1a_2,
  mod_rwa_1a_3,
  show.se = T,
  show.std = T,
show.est = FALSE
)


tab_var_decomp_rwa <- 
  data.frame(
  var = names(get_iccs(mod_rwa_1a_3))
)

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1a_1)),"var")
  )

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1a_2)),"var")
  )

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1a_3)),"var")
  )

tab_var_decomp_rwa %>%
  sjPlot::tab_df()

### Table A16 ----

mod_sdo_1a_1 = lmer(rating ~ 1 + (1|case)                       , data = var_decomp_1a)
mod_sdo_1a_2 = lmer(rating ~ 1 + (1|case) + (1|sdo)             , data = var_decomp_1a)
mod_sdo_1a_3 = lmer(rating ~ 1 + (1|case) + (1|sdo) + (1|target), data = var_decomp_1a)

sjPlot::tab_model(
  mod_sdo_1a_1,
  mod_sdo_1a_2,
  mod_sdo_1a_3,
  show.se = T,
  show.std = T,
  show.est = FALSE
)

tab_var_decomp_sdo <- 
  data.frame(
    var = names(get_iccs(mod_sdo_1a_3))
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1a_1)),"var")
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1a_2)),"var")
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1a_3)),"var")
  )

tab_var_decomp_sdo %>%
  sjPlot::tab_df()

### Table A17 ----

var_decomp_1b <- 
  ds2_wde %>%
  select(case,rwa,sdo,starts_with("prj.")) %>%
  tidyr::pivot_longer(cols = starts_with("prj"),
                      names_to = "target",
                      values_to = "rating") %>%
  filter(complete.cases(.))

mod_rwa_1b_1 = lmer(rating ~ 1 + (1|case)                       , data = var_decomp_1b)
mod_rwa_1b_2 = lmer(rating ~ 1 + (1|case) + (1|rwa)             , data = var_decomp_1b)
mod_rwa_1b_3 = lmer(rating ~ 1 + (1|case) + (1|rwa) + (1|target), data = var_decomp_1b)

sjPlot::tab_model(
  mod_rwa_1b_1,
  mod_rwa_1b_2,
  mod_rwa_1b_3,
  show.se = T,
  show.std = T,
  show.est = FALSE
)


tab_var_decomp_rwa <- 
  data.frame(
    var = names(get_iccs(mod_rwa_1b_3))
  )

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1b_1)),"var")
  )

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1b_2)),"var")
  )

tab_var_decomp_rwa <-
  full_join(
    tab_var_decomp_rwa,
    rownames_to_column(as.data.frame(get_iccs(mod_rwa_1b_3)),"var")
  )

tab_var_decomp_rwa %>%
  sjPlot::tab_df()

### Table A18 ----

mod_sdo_1b_1 = lmer(rating ~ 1 + (1|case)                       , data = var_decomp_1b)
mod_sdo_1b_2 = lmer(rating ~ 1 + (1|case) + (1|sdo)             , data = var_decomp_1b)
mod_sdo_1b_3 = lmer(rating ~ 1 + (1|case) + (1|sdo) + (1|target), data = var_decomp_1b)

sjPlot::tab_model(
  mod_sdo_1b_1,
  mod_sdo_1b_2,
  mod_sdo_1b_3,
  show.se = T,
  show.std = T,
  show.est = FALSE
)

tab_var_decomp_sdo <- 
  data.frame(
    var = names(get_iccs(mod_sdo_1b_3))
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1b_1)),"var")
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1b_2)),"var")
  )

tab_var_decomp_sdo <-
  full_join(
    tab_var_decomp_sdo,
    rownames_to_column(as.data.frame(get_iccs(mod_sdo_1b_3)),"var")
  )

tab_var_decomp_sdo %>%
  sjPlot::tab_df()

# Appendix B: Study 2 ----

### Table B1 ----

cdbk_2 %>%
  filter(scale == "Traditionalism") %>%
  select(full_item,coding) %>%
  sjPlot::tab_df(footnote = paste("Introduction:",
                                  cdbk_2 %>%
                   filter(scale == "Traditionalism") %>%
                   select(introduction) %>% distinct() %>% pull(introduction)),
                 show.footnote = T)

### Table B2 ----

cdbk_2 %>%
  filter(str_detect(scale,"Prejudice")) %>%
  select(scale,full_item,coding) %>%
  mutate(scale = str_replace_all(scale,"_"," "),
         scale = str_to_title(scale)) %>%
  sjPlot::tab_df(footnote = paste("Response scales:",
                                  paste(
                                    cdbk_2 %>%
                                    filter(str_detect(scale,"Prejudice")) %>%
                                    select(response_scale) %>% distinct() %>% pull(response_scale), collapse = ","
                                    )),
                 show.footnote = T)


### Table B3 ----
ess4_ana %>%
  janitor::tabyl(gndr) %>%
  janitor::adorn_totals("row") %>%
  select(-valid_percent) %>%
  rename("Gender" = "gndr") %>%
  sjPlot::tab_df()
  
### Table B4 ----
ess4_ana %>%
  janitor::tabyl(education_eisced) %>%
  janitor::adorn_totals("row") %>%
  select(-valid_percent) %>%
  rename("Education" = "education_eisced") %>%
  sjPlot::tab_df()

### Table B5 ----

ess4_ana %>%
  select(age,ends_with("_raw")) %>%
  pivot_longer(cols = 1:ncol(.), names_to = "scale", values_to = "score") %>%
  filter(complete.cases(.)) %>%
  group_by(scale) %>%
  summarise(
    n = n(),
    Mean = mean(score),
    Median = median(score),
    SD = sd(score),
    Min = min(score),
    Max = max(score)
  ) %>%
  mutate(Range = Max - Min) %>%
  sjPlot::tab_df()

### Table B6 ----

full_join(
  ess4_ana %>%
    select(cntry_lbl) %>%
    group_by(cntry_lbl) %>%
    summarise(n = n()),
  ess4_ana %>%
    select(cntry_lbl,age,ends_with("_raw")) %>%
    pivot_longer(cols = age:ncol(.), names_to = "scale", values_to = "score") %>%
    filter(complete.cases(.)) %>%
    group_by(cntry_lbl,scale) %>%
    summarise(
      Mean = formatC(mean(score),format = "f",digits = 2),
      SD   = formatC(sd(score),format = "f",digits = 2)
    ) %>%
    mutate(m_sd = paste0(Mean," (",SD,")"),.keep = "unused") %>%
    pivot_wider(names_from = "scale",values_from = "m_sd")
) %>%  
  select(1:3,10,7,6,9,8,4,5) %>% sjPlot::tab_df()

### Table B7 ----

ess4_ana %>%
  select(lrscale,ends_with("_raw")) %>%
  correlation::correlation() %>%
  as_tibble() %>%
  mutate(
    Parameter1 = str_remove_all(Parameter1,c("_raw|prj_")),
    Parameter2 = str_remove_all(Parameter2,c("_raw|prj_")),
    Parameter1 = str_to_title(Parameter1),
    Parameter2 = str_to_title(Parameter2),
    CI95 = paste0("[",
                  formatC(CI_low,format = "f",digits = 2),", ",
                  formatC(CI_high,format = "f",digits = 2),
                  "]"),
    p = format.pval(p,eps = 0.0001, scientific = FALSE)
  ) %>%
  select(Parameter1,Parameter2,n_Obs,r,CI95,p) %>%
  sjPlot::tab_df()

### Table B8 ----

cfi_dat <- 
  ess4_raw %>%
  select(
    #Grouping Variable: Country
    cntry_lbl,
    #Traditionalism
    trad_1,trad_2,trad_3,trad_4,trad_5,
    #Prejudice: Anti-Gay
    prj_gay_1,
    #Prejudice: Immigrants
    prj_immi_1,prj_immi_2,prj_immi_3,
    #Prejudice: Unemployed
    prj_unempl_1,
    #Prejudice: Women
    prj_wmn_1,prj_wmn_2,
    #Prejudice: Age
    prj_age20,prj_age70
  ) %>%
  mutate(
    across(where(is.numeric), ~as.numeric(scale(.)),.names = "{.col}_scl")
  )

# Traditionalism

trd_inv <- '
trd_cfa =~ trad_1_scl + trad_2_scl + trad_3_scl + trad_4_scl + trad_5_scl
'

trd.config <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM')
trd.metric <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal ="loadings")
trd.scalar <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal = c("loadings", "intercepts"))


tab_mi_trad <- 
  cbind(
    data.frame(configural = fitMeasures(trd.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(metric     = fitMeasures(trd.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(scalar     = fitMeasures(trd.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr")))
  )


tab_mi_trad <- 
  tab_mi_trad %>%
  rownames_to_column("fit_measure") %>%
  mutate(
    configural_metric = configural - metric,
    metric_scalar = metric - scalar,
    configural_metric = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ configural_metric,TRUE ~ NA),
    metric_scalar = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ metric_scalar,TRUE ~ NA)
  )

tab_mi_trad <- 
  tab_mi_trad %>%
  mutate(across(c(2:ncol(.)),~as.numeric(.)),
         across(c(2:ncol(.)),~case_when(fit_measure == "pvalue" ~ format.pval(.,eps = 0.0001, scientific = FALSE), 
                                        TRUE ~ formatC(.,format = "f",digits = 2))),
         fit_measure = str_to_upper(fit_measure)
  )

tab_mi_trad %>%
  sjPlot::tab_df()

anova(trd.config,trd.metric)
anova(trd.metric,trd.scalar)


### Table B9 ----
# prejudice targets

# Define model
trgt_inv <- '

prj_gay        =~ prj_gay_1_scl
prj_immigrants =~ prj_immi_1_scl + prj_immi_2_scl + prj_immi_3_scl
prj_unemployed =~ prj_unempl_1_scl
prj_women      =~ prj_wmn_1_scl + prj_wmn_2_scl
prj_age20      =~ prj_age20_scl
prj_age70      =~ prj_age70_scl

'

trgt.config <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR")
trgt.metric <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR",
                   group.equal = "loadings")
trgt.scalar <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR",
                   group.equal = c("loadings","Intercepts"))

tab_mi_trgt <- 
  cbind(
    data.frame(configural = fitMeasures(trgt.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(metric     = fitMeasures(trgt.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(scalar     = fitMeasures(trgt.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr")))
  )


tab_mi_trgt <- 
  tab_mi_trgt %>%
  rownames_to_column("fit_measure") %>%
  mutate(
    configural_metric = configural - metric,
    metric_scalar = metric - scalar,
    configural_metric = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ configural_metric,TRUE ~ NA),
    metric_scalar = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ metric_scalar,TRUE ~ NA)
  )

tab_mi_trgt <- 
  tab_mi_trgt %>%
  mutate(across(c(2:ncol(.)),~as.numeric(.)),
         across(c(2:ncol(.)),~case_when(fit_measure == "pvalue" ~ format.pval(.,eps = 0.0001, scientific = FALSE), 
                                        TRUE ~ formatC(.,format = "f",digits = 2))),
         fit_measure = str_to_upper(fit_measure)
  )

tab_mi_trgt %>%
  sjPlot::tab_df()

anova(trgt.config,trgt.metric)
anova(trgt.metric,trgt.scalar)

### Table B10 ----

tab_cntry_corrs <- 
  ess4_ana %>%
  select(cntry_lbl,trad_raw,(starts_with("prj_")&ends_with("_scl"))) %>%
  group_by(cntry_lbl) %>%
  mutate(trad_subgrp = case_when(trad_raw >= median(trad_raw,na.rm = T) ~"high",
                                 trad_raw <  median(trad_raw,na.rm = T) ~ "low"),
         cntry_lbl = fct_drop(cntry_lbl)) %>%
  pivot_longer(cols = starts_with("prj"), names_to = "target", values_to = "rating") %>%
  group_by(cntry_lbl,target,trad_subgrp) %>%
  summarise(mean_subgrp_rating = mean(rating,na.rm = T),.groups = "drop") %>%
  filter(!is.na(trad_subgrp)) %>%
  pivot_wider(names_from = trad_subgrp,values_from = mean_subgrp_rating) %>%
  group_by(cntry_lbl) %>%
  correlation::correlation() %>%
  as_tibble() %>%
  mutate(
    across(where(is.numeric),~formatC(.,format = "f",digits = 2)),
    within_cntry = paste0(r," [",CI_low,", ",CI_high,"]"),
    cntry_lbl = Group
  ) %>%
  select(cntry_lbl,within_cntry)

tab_cntry_corrs <- 
  full_join(
    tab_cntry_corrs,
    cntry_corrs(data = ess4_ana,
                cluster = "cntry_lbl",
                split.var = "trad_raw",
                prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
                split.stat = "median",
                by.country = TRUE) %>%
      mutate(
        btween_cntry_rci = paste(Average_r,CI95),
        cntry_lbl = cluster
      ) %>% select(Group,cntry_lbl,btween_cntry_rci) %>%
      pivot_wider(names_from = Group,values_from = btween_cntry_rci) %>%
      select(cntry_lbl,none,median_low,median_high)
  )
tab_cntry_corrs %>%
  sjPlot::tab_df()

cntry_corrs(data = ess4_ana,
            cluster = "cntry_lbl",
            split.var = "trad_raw",
            prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
            split.stat = "median",
            by.country = FALSE) 

# Appendix C: Study 3 ----

### Table C1 ----

cdbk_3 %>%
  filter(scale == "Traditionalism") %>%
  select(full_item,coding) %>%
  sjPlot::tab_df(footnote = paste("Introduction:",
                                  cdbk_3 %>%
                                    filter(scale == "Traditionalism") %>%
                                    select(introduction) %>% distinct() %>% pull(introduction)),
                 show.footnote = T)

### Table C2 ----

cdbk_3 %>%
  filter(str_detect(scale,"Prejudice")) %>%
  select(scale,full_item,coding) %>%
  mutate(scale = str_replace_all(scale,"_"," "),
         scale = str_to_title(scale)) %>%
  sjPlot::tab_df(footnote = paste("Response scales:",
                                  paste(
                                    cdbk_3 %>%
                                      filter(str_detect(scale,"Prejudice")) %>%
                                      select(response_scale) %>% distinct() %>% pull(response_scale), collapse = ","
                                  )),
                 show.footnote = T)

### Table C3 ----

ess9_ana %>%
  select(trad,lrscale,anti_mig,anti_gay) %>%
  pivot_longer(cols = 1:ncol(.),names_to = "scale",values_to = "score") %>%
  filter(complete.cases(.)) %>%
  group_by(scale) %>%
  summarise(
    n = n(),
    mean = mean(score),
    sd = sd(score),
    median = median(score),
    min = min(score),
    max = max(score)
  ) %>%
  mutate(
    Range = max - min,
    across(c("mean","sd","median"),~formatC(.,format = "f",digits = 2))
  ) %>%
  slice(4,3,2,1) %>%
  sjPlot::tab_df()

psych::alpha(ess9_raw[trad.itms])
psych::alpha(ess9_raw[anti_mig.itms])
psych::alpha(ess9_raw[anti_gay.itms])

### Table C4 ----

ess9_ana %>%
  select(trad,lrscale,anti_mig,anti_gay,
         rgn_lvl_trad,rgn_lvl_anti_mig,rgn_lvl_anti_gay) %>%
  correlation::correlation() %>%
  as_tibble() %>%
  mutate(
    Parameter1 = str_remove_all(Parameter1,c("_raw|prj_")),
    Parameter2 = str_remove_all(Parameter2,c("_raw|prj_")),
    Parameter1 = str_to_title(Parameter1),
    Parameter2 = str_to_title(Parameter2),
    CI95 = paste0("[",
                  formatC(CI_low,format = "f",digits = 2),", ",
                  formatC(CI_high,format = "f",digits = 2),
                  "]"),
    p = format.pval(p,eps = 0.0001, scientific = FALSE)
  ) %>%
  select(Parameter1,Parameter2,n_Obs,r,CI95,p) %>%
  sjPlot::tab_df()


### Table C5 ----

cfi_dat3 <- 
  ess9_raw %>%
  select(
    #Grouping Variable: Country
    cntry_lbl,
    #Traditionalism
    trad_1,trad_2,trad_3,trad_4,trad_5,
    #Prejudice: Anti-Gay
    prj_gay_1,prj_gay_2,prj_gay_3,
    #Prejudice: Immigrants
    prj_immi_1,prj_immi_2,prj_immi_3
  ) %>%
  mutate(
    across(where(is.numeric), ~as.numeric(scale(.)),.names = "{.col}_scl")
  )

# Traditionalism

trd_inv <- '
trd_cfa =~ trad_1_scl + trad_2_scl + trad_3_scl + trad_4_scl + trad_5_scl
'

trd.config <- cfa(trd_inv, data = cfi_dat3, group = "cntry_lbl", estimator = 'MLM')
trd.metric <- cfa(trd_inv, data = cfi_dat3, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal ="loadings")
trd.scalar <- cfa(trd_inv, data = cfi_dat3, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal = c("loadings", "intercepts"))


tab_mi_trad <- 
  cbind(
    data.frame(configural = fitMeasures(trd.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(metric     = fitMeasures(trd.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(scalar     = fitMeasures(trd.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr")))
  )


tab_mi_trad <- 
  tab_mi_trad %>%
  rownames_to_column("fit_measure") %>%
  mutate(
    configural_metric = configural - metric,
    metric_scalar = metric - scalar,
    configural_metric = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ configural_metric,TRUE ~ NA),
    metric_scalar = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ metric_scalar,TRUE ~ NA)
  )

tab_mi_trad <- 
  tab_mi_trad %>%
  mutate(across(c(2:ncol(.)),~as.numeric(.)),
         across(c(2:ncol(.)),~case_when(fit_measure == "pvalue" ~ format.pval(.,eps = 0.0001, scientific = FALSE), 
                                        TRUE ~ formatC(.,format = "f",digits = 2))),
         fit_measure = str_to_upper(fit_measure)
  )

tab_mi_trad %>%
  sjPlot::tab_df()

anova(trd.config,trd.metric)
anova(trd.metric,trd.scalar)


### Table C6 ----
# prejudice targets

# Define model
trgt_inv <- '

prj_gay        =~ prj_gay_1_scl + prj_gay_2_scl + prj_gay_3_scl
prj_immigrants =~ prj_immi_1_scl + prj_immi_2_scl + prj_immi_3_scl
'

trgt.config <- cfa(trgt_inv, data = cfi_dat3, group = "cntry_lbl", estimator = "MLR")
trgt.metric <- cfa(trgt_inv, data = cfi_dat3, group = "cntry_lbl", estimator = "MLR",
                   group.equal = "loadings")
trgt.scalar <- cfa(trgt_inv, data = cfi_dat3, group = "cntry_lbl", estimator = "MLR",
                   group.equal = c("loadings","Intercepts"))

tab_mi_trgt <- 
  cbind(
    data.frame(configural = fitMeasures(trgt.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(metric     = fitMeasures(trgt.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))),
    data.frame(scalar     = fitMeasures(trgt.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr")))
  )


tab_mi_trgt <- 
  tab_mi_trgt %>%
  rownames_to_column("fit_measure") %>%
  mutate(
    configural_metric = configural - metric,
    metric_scalar = metric - scalar,
    configural_metric = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ configural_metric,TRUE ~ NA),
    metric_scalar = case_when(fit_measure == "cfi"|fit_measure == "rmsea"|fit_measure == "srmr" ~ metric_scalar,TRUE ~ NA)
  )

tab_mi_trgt <- 
  tab_mi_trgt %>%
  mutate(across(c(2:ncol(.)),~as.numeric(.)),
         across(c(2:ncol(.)),~case_when(fit_measure == "pvalue" ~ format.pval(.,eps = 0.0001, scientific = FALSE), 
                                        TRUE ~ formatC(.,format = "f",digits = 2))),
         fit_measure = str_to_upper(fit_measure)
  )

tab_mi_trgt %>%
  sjPlot::tab_df()

anova(trgt.config,trgt.metric)
anova(trgt.metric,trgt.scalar)

### Table C7 ----

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

#Anti-Gay Prejudice
  
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
                  ,dv.labels = "Model: Anti-Gay Attitudes"
                  #,file = "./2_tables/25_03_06_anti_gay.html"
)

performance::check_collinearity(mlm_anti_gay_5_no_cntrls)
performance::check_collinearity(mlm_anti_gay_5_cntrls)

performance::check_convergence(mlm_anti_gay_5_no_cntrls)
performance::check_convergence(mlm_anti_gay_5_cntrls)

### Table C8 ----

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
                  show.re.var = F
                  #,file = "./2_tables/model_building_anti_mig.html"
                  )


### Table C9 ----

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
                  show.re.var = F
                  #,file = "./2_tables/model_building_anti_gay.html"
                  )