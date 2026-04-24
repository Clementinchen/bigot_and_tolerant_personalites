# LIBRARIES ----

library(tidyverse)
library(lme4)
library(easystats)

# FUNCTIONS ----

tab_desc_rs <- function(data,vars) {
  msd <- 
    data %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = 1:ncol(.),names_to = "scale",values_to = "score") %>%
    group_by(scale) %>%
    summarise(M = mean(score,na.rm = T),
              SD = sd(score,na.rm = T),
              msd = paste0(round(M,2)," (",round(SD,2),")"),
              range = paste(range(score,na.rm = T)[1],"-",range(score,na.rm = T)[2])) %>%
    ungroup %>%
    select(-c(M,SD)) %>%
    data.frame()
  
  rs <- 
    data %>%
    select(all_of(vars)) %>%
    corrr::correlate(use = "pairwise.complete.obs",method = "pearson")%>%
    rename("scale" = "term") %>%
    data.frame() %>%
    mutate(across(where(is.numeric),~round(.,2)))
  
  rs[upper.tri(rs, diag = FALSE)] <- NA
  order <-  rs %>% pull(scale)
  
  tab_res <- full_join(msd,rs, by = "scale") %>% 
    arrange(factor(scale, levels = order)) %>%
    select(-ncol(.))
  print(tab_res)
  
}

clstr_means <- function(data,cluster,vars) {
  data %>%
    select(cluster,all_of(vars)) %>%
    pivot_longer(cols = all_of(vars),names_to = "var",values_to = "score") %>%
    group_by(!!sym(cluster),var) %>%
    summarise(mean = mean(score,na.rm = T),.groups = "drop") %>%
    pivot_wider(names_from = "var",values_from = "mean") %>%
    print(n = nrow(.))
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
             "95% CI" = paste0("[",r_ci_lower,", ",r_ci_upper,"]")) %>%
      select(all_of(group_var),z_mean,r_mean,ncol(.)) %>%
      rename("Average z" = "z_mean","Average r"="r_mean")
    
    cat("Within-Cluster Correlation Between Subgroups.\nCluster:",toupper({{ cluster }}),
        "\nSubgroups Split Variable:", toupper({{ split.var }}),
        "\nSplit Metric:",toupper({{ split.stat }}),"\n\n")
    print(cor_within_cluster)
    
    cat("\n\nAveraged Between-Cluster Correlation Within Subgroups.\nCluster:",toupper({{ cluster }}),
        "\nSubgroups Split Variable:", toupper({{ split.var }}),
        "\nSplit Metric:",toupper({{ split.stat }}),"\n\n")
    
    print(btwn_clstr_corrs, n = nrow(btwn_clstr_corrs))
    
    
  }

get_iccs <- function(model) {
  variances <- as.data.frame(VarCorr(model))$vcov
  names(variances) <- as.data.frame(VarCorr(model))$grp
  
  total_var <- sum(variances)
  
  variances <- data.frame(variances)
  variances$iccs <- variances$variances / total_var
  
  return(variances)
}



bootstrap_icc <- function(model,
                          iterations,
                          type) {
  
  # ------------------------------------------------------------
  # helper: smart rounding
  # ------------------------------------------------------------
  smart_round <- function(x, digits = 2, max_digits = 6) {
    for (d in digits:max_digits) {
      r <- round(x, d)
      if (r != 0) {
        return(formatC(r, format = "f", digits = d))
      }
    }
    formatC(0, format = "f", digits = max_digits)
  }
  
  # ------------------------------------------------------------
  # summary function
  # ------------------------------------------------------------
  mysumm <- function(m) {
    
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
  
  # ------------------------------------------------------------
  # bootstrap
  # ------------------------------------------------------------
  boot <- bootMer(
    model,
    FUN = mysumm,
    nsim = iterations,
    type = type,
    use.u = FALSE
  )
  
  # ------------------------------------------------------------
  # results table
  # ------------------------------------------------------------
  results <- data.frame(
    observed = boot$t0,
    CI_norm  = NA_character_,
    CI_basic = NA_character_,
    CI_perc  = NA_character_
  )
  
  # ------------------------------------------------------------
  # confidence intervals
  # ------------------------------------------------------------
  for (i in seq_len(nrow(results))) {
    
    ci <- boot::boot.ci(
      boot,
      index = i,
      conf = .95,
      type = c("norm", "basic", "perc")
    )
    
    results$CI_norm[i]  <- paste0(
      "[",
      smart_round(ci$normal[2]), ", ",
      smart_round(ci$normal[3]), "]"
    )
    
    results$CI_basic[i] <- paste0(
      "[",
      smart_round(ci$basic[4]), ", ",
      smart_round(ci$basic[5]), "]"
    )
    
    results$CI_perc[i]  <- paste0(
      "[",
      smart_round(ci$percent[4]), ", ",
      smart_round(ci$percent[5]), "]"
    )
  }
  
  results
}

# PREPARE LONG DATA FOR MLM ----

ess4_long <- 
  ess4_ana %>% 
  pivot_longer(cols = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
               names_to = "prj_target",
               values_to = "prj_rating_scl")

# DESCRIPTIVES -----

## Scales & Correlations ----

tab_desc_rs(data = ess4_ana,
            vars = c("trad_raw","prj_immigrants_raw","prj_gay_raw",
                     "prj_women_raw","prj_unemployed_raw","prj_age20_raw","prj_age70_raw"))

## Means by Country ----

clstr_means(data = ess4_ana,cluster = "cntry_lbl",
            vars = c("trad_raw","prj_immigrants_raw","prj_gay_raw","prj_women_raw",
                     "prj_unemployed_raw","prj_age20_raw","prj_age70_raw"))

## Correlations ----

cntry_corrs(data = ess4_ana,
            cluster = "cntry_lbl",
            split.var = "trad_raw",
            prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
            split.stat = "median",
            by.country = TRUE)

cntry_corrs(data = ess4_ana,
            cluster = "cntry_lbl",
            split.var = "trad_raw",
            prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
            split.stat = "median",
            by.country = FALSE)

cntry_corrs(data = ess4_ana,
            cluster = "cntry_lbl",
            split.var = "trad_raw",
            prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
            split.stat = "1sd",
            by.country = TRUE)

cntry_corrs(data = ess4_ana,
            cluster = "cntry_lbl",
            split.var = "trad_raw",
            prj.items = c("prj_immigrants_scl","prj_gay_scl","prj_unemployed_scl","prj_women_scl","prj_age20_scl","prj_age70_scl"),
            split.stat = "1sd",
            by.country = FALSE)


# MULTILEVEL VARIANCE DECOMPOSITION ----
mlm_ess4_0_reg  <- lmer(prj_rating_scl ~ (1|region),     data = ess4_long)
mlm_ess4_0_trg  <- lmer(prj_rating_scl ~ (1|prj_target), data = ess4_long)
mlm_ess4_0_cse  <- lmer(prj_rating_scl ~ (1|idno),       data = ess4_long)
mlm_ess4_0_trd  <- lmer(prj_rating_scl ~ (1|trad_raw),   data = ess4_long)

iccs_null <- 
  data.frame(
  rbind(
  get_iccs(mlm_ess4_0_reg)$iccs,
  get_iccs(mlm_ess4_0_trg)$iccs,
  get_iccs(mlm_ess4_0_cse)$iccs,
  get_iccs(mlm_ess4_0_trd)$iccs
  ))

colnames(iccs_null) <- c("ICC","Residual")
row.names(iccs_null) <- c("Null Model Region","Null Model Target","Null Model Participant","Null Model Traditionalism")

#ICCs Null Models for each Random Factor Variable
iccs_null

#testing models by stepwise including random factor variables and testing two nesting structures
#target nested in region
#participant nested in region
mlm_ess4_m1  <- lmer(prj_rating_scl ~ (1|region/prj_target),                           data = ess4_long)
mlm_ess4_m2  <- lmer(prj_rating_scl ~ (1|region/prj_target) + (1|idno),                data = ess4_long)
mlm_ess4_m3  <- lmer(prj_rating_scl ~ (1|region/prj_target) + (1|idno) + (1|trad_raw), data = ess4_long)
mlm_ess4_m4  <- lmer(prj_rating_scl ~ (1|region/idno),                                 data = ess4_long)
mlm_ess4_m5  <- lmer(prj_rating_scl ~ (1|region/idno) + (1|prj_target),                data = ess4_long)
mlm_ess4_m6  <- lmer(prj_rating_scl ~ (1|region/idno) + (1|prj_target) + (1|trad_raw), data = ess4_long)

#iccs
rownames_to_column(get_iccs(mlm_ess4_m1)[2],"variable") %>% rename("Model 1" = "iccs") %>%
  full_join(.,rownames_to_column(get_iccs(mlm_ess4_m2)[2],"variable") %>% rename("Model 2" = "iccs")) %>%
  full_join(.,rownames_to_column(get_iccs(mlm_ess4_m3)[2],"variable") %>% rename("Model 3" = "iccs")) %>%
  full_join(.,rownames_to_column(get_iccs(mlm_ess4_m4)[2],"variable") %>% rename("Model 4" = "iccs")) %>%
  full_join(.,rownames_to_column(get_iccs(mlm_ess4_m5)[2],"variable") %>% rename("Model 5" = "iccs")) %>%
  full_join(.,rownames_to_column(get_iccs(mlm_ess4_m6)[2],"variable") %>% rename("Model 6" = "iccs")) %>%
  
  slice(1:2,4:nrow(.),3)


#Bootstrap ICCs for the model Target nested in Region, including random factors participant and traditionalism
bootstrap_icc(mlm_ess4_m3,
              iterations = 1000,
              type = "parametric")

# FIGURE 3 ----

fig3 <- 
  ess4_long %>%
  group_by(cntry_lbl) %>%
  mutate(
    trad_cntry_median = 
      case_when(
        trad_raw >= median(trad_raw,na.rm = T) ~ "high",
        trad_raw <  median(trad_raw,na.rm = T) ~ "low",
      )
  ) %>%
  group_by(trad_cntry_median,cntry_lbl,prj_target) %>%
  summarise(mean_rating = mean(prj_rating_scl,na.rm = T)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  mutate(
    trad_cntry_median = str_to_title(trad_cntry_median),
    trad_cntry_median = forcats::fct_rev(trad_cntry_median),
    prj_target = str_remove_all(prj_target,"prj_"),
    prj_target = forcats::fct_recode(prj_target,
                                     "age 20" = "age20_scl",
                                     "age 70" = "age70_scl",
                                     "gay people" = "gay_scl"),
    prj_target = str_remove(prj_target,"_scl"),
    prj_target = str_to_title(prj_target),
    cntry_lbl = str_wrap(cntry_lbl,10)
  ) %>%
  ggplot(aes(trad_cntry_median, mean_rating, group = prj_target)) +
  geom_point(aes(color = prj_target), size = 1.5) +
  geom_line(aes(color = prj_target), linewidth = 0.4) +
  facet_wrap(~cntry_lbl) +
  scale_x_discrete(expand = c(0.02, 0.02)) +
  paletteer::scale_color_paletteer_d("MoMAColors::Picasso") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text=element_text(size=12),
    legend.title = element_text(size = 12),
    legend.justification = "right",
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(
      hjust = c(0, 1)
    ),
    axis.title.x = element_text(size=13, face="bold", colour = "black"),    
    axis.title.y = element_text(size=13, face="bold", colour = "black")
  ) + guides(fill = guide_legend(nrow = 1)) +
  labs(color = "Target") +
  ylab("Mean Prejudice Rating (z-score)") +
  xlab("Traditionalism Subsample")


fig3
