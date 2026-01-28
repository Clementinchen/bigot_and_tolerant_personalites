ess4_ana %>%
  group_by(target) %>%
  correlation::correlation()

ess4 %>%
  select(
    cntry_lbl,
    cntry_cde,
    region,
    traditionalism,
    prj_gay,
    prj_immigrants,
    prj_unemployed,
    prj_women,
    prj_age20,
    prj_age70
  ) %>%
  correlation::correlation()

ess4 %>%
  select(
    cntry_lbl,
    cntry_cde,
    region,
    traditionalism,
    prj_gay,
    prj_immigrants,
    prj_unemployed,
    prj_women,
    prj_age20,
    prj_age70
  ) %>%
  corrr::correlate()


ess4_ana <- 
  ess4_ana %>%
  group_by(target) %>%
  mutate(
    rating_scl = as.numeric(scale(rating))
  ) %>%
  group_by(cntry_lbl) %>%
  mutate(
    traditionalism_cntry_median = 
      case_when(
        traditionalism >= median(traditionalism,na.rm = T) ~ "high",
        traditionalism <  median(traditionalism,na.rm = T) ~ "low",
      )
  ) %>%
  ungroup()

ess4_cor <- 
  ess4_ana %>%
  group_by(
    cntry_lbl,
    target,
    traditionalism_cntry_median
    ) %>%
  filter(!is.na(traditionalism_cntry_median)) 

ess4_ana %>%
  group_by(target) %>%
  mutate(prj_rating_scl = as.numeric(scale(rating))) %>%
  ungroup() %>%
  summarise(r = cor(rating,prj_rating_scl,use = "pairwise"))


ess_ana <- 
  ess4 %>%
  select(
    idno,
    cntry_lbl,
    cntry_cde,
    region,
    lrscale,
    age,
    trd,
    prj_gay,
    prj_immigrants,
    prj_unemployed,
    prj_women,
    prj_age20,
    prj_age70
  ) %>%
  mutate(
    idno = paste0(cntry_cde,"_",idno),
    across(starts_with("prj_"),~as.numeric(scale(.)),.names = "{.col}_scl")
    )

ess4_long <- 
  ess_ana %>%
  select(idno,
         cntry_lbl,
         cntry_cde,
         region,
         lrscale,
         age,
         trd,
         prj_gay_scl,
         prj_immigrants_scl,
         prj_unemployed_scl,
         prj_women_scl,
         prj_age20_scl,
         prj_age70_scl
         ) %>%
  pivot_longer(
    cols = ends_with("_scl"),
    names_to = "prj_target",
    values_to = "prj_rating_scl"
  )

ess4_long <- 
  left_join(
  ess4_long %>%
    mutate(prj_target = str_remove_all(prj_target,"_scl")),
  ess_ana %>%
  select(idno,
         prj_gay,
         prj_immigrants,
         prj_unemployed,
         prj_women,
         prj_age20,
         prj_age70
  ) %>%
  pivot_longer(
    cols = starts_with("prj_"),
    names_to = "prj_target",
    values_to = "prj_rating"
  ),
  by = c("idno","prj_target")
)

ess4_long <- 
  ess4_long %>%
  #group_by(prj_target) %>%
  mutate(prj_rating_scl2 = as.numeric(scale(prj_rating)))




vcs <- as.data.frame(VarCorr(mlm_ess4_m7))$vcov
names(vcs) <- as.data.frame(VarCorr(mlm_ess4_m7))$grp

total_var <- sum(vcs)

vcs <- data.frame(vcs)
vcs$iccs <- vcs$vcs / total_var




get_iccs <- function(model) {
  variances <- as.data.frame(VarCorr(model))$vcov
  names(variances) <- as.data.frame(VarCorr(model))$grp
  
  total_var <- sum(variances)
  
  variances <- data.frame(variances)
  variances$iccs <- variances$variances / total_var
  
  return(variances)
}



ess4_long <-
  ess4_long %>%
  mutate(idno = as.factor(idno))

ess4_long %>%
  group_by(region) %>%
  summarise(n = n()) %>%
  filter(n < 50)




ess4_mlm <- 
  ess4_long %>%
  mutate(region = 
           fct_relabel(
             region,
             janitor::make_clean_names
           ),
         region = case_when(region == "x" ~ NA,
                            TRUE ~ region))
table(is.na(ess4_mlm$region))

levels(ess4_long$region)

ess4_mlm <- 
  ess4_mlm %>%
  filter(!is.na(region))


library(lme4)
mlm_ess4_m7 <- lmer(prj_rating_scl ~ (1|region/prj_target) + (1|idno) + (1|trd), data = ess4_long)
summary(mlm_ess4_m7)

get_iccs(mlm_ess4_m7)

bootstrap_icc(mlm_ess4_m7,
              iterations = 500,
              type = "parametric")








ess4_long %>%
  group_by(cntry_lbl) %>%
  mutate(
    trd_cntry_median = 
      case_when(
        trd >= median(trd,na.rm = T) ~ "high",
        trd <  median(trd,na.rm = T) ~ "low",
      )
  ) %>%
  group_by(trd_cntry_median,cntry_lbl,prj_target) %>%
  summarise(mean_rating = mean(prj_rating_scl,na.rm = T)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  pivot_wider(names_from = "trd_cntry_median",
              values_from = "mean_rating") %>%
  correlation::correlation(p_adjust = "bonferroni")

cntxt_cor <- function(data,
                      split_statistic,
                      split_var,
                      cluster_1,
                      cluster_2,
                      rating_var,
                      p_adjust) {
  dat <- 
    data %>%
    select(split_var,
           cluster_1,
           cluster_2,
           rating) %>%
    group_by(cluster_1) %>%
    mutate(split = case_when(split_var >= split_statistic(trd,na.rm = T) ~ "low",
                             split_var <  split_statistic(trd,na.rm = T) ~ "high",
    )) %>%
    ungroup()
  dat %>%
    group_by(split,cluster_1,cluster_2) %>%
    summarise(mean_rating = mean(rating_var,na.rm = T)) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    pivot_wider(names_from = "trd_cntry_median",
                values_from = "mean_rating") %>%
    correlation::correlation(p_adjust = [p_adjust])
}


ess4_long %>%
  group_by(cntry_lbl) %>%
  mutate(
    trd_cntry_median = 
      case_when(
        trd >= median(trd,na.rm = T) ~ "high",
        trd <  median(trd,na.rm = T) ~ "low",
      )
  ) %>%
  group_by(trd_cntry_median,cntry_lbl,prj_target) %>%
  summarise(mean_rating = mean(prj_rating_rsc,na.rm = T)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(trd_cntry_median == "low") %>%
  pivot_wider(names_from = "cntry_lbl",
              values_from = "mean_rating") %>%
  correlation::correlation() %>%
  as_tibble()



# long-Format: one row per country
btwn_cntry_whole <- 
  btwn_cntry_whole %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(whole_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")



library(dplyr)
library(tidyr)
library(correlation)

cntxt_cor <- function(data,
                      split_var,
                      split_statistic = median,
                      cluster_1,
                      cluster_2,
                      rating_var,
                      p_adjust = "bonferroni",
                      labels = c("low", "high")) {
  
  data %>%
    group_by({{ cluster_1 }}) %>%
    mutate(
      split = case_when(
        {{ split_var }} >= split_statistic({{ split_var }}, na.rm = TRUE) ~ labels[2],
        {{ split_var }} <  split_statistic({{ split_var }}, na.rm = TRUE) ~ labels[1]
      )
    ) %>%
    ungroup() %>%
    group_by(split, {{ cluster_1 }}, {{ cluster_2 }}) %>%
    summarise(
      mean_rating = mean({{ rating_var }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(complete.cases(.)) %>%
    pivot_wider(
      names_from = split,
      values_from = mean_rating
    ) %>%
    correlation::correlation(p_adjust = p_adjust)
}

cntxt_cor(
  data = ess4_long,
  split_var = trd,
  cluster_1 = cntry_lbl,
  cluster_2 = prj_target,
  rating_var = prj_rating_scl,
  split_statistic = median,
  p_adjust = "bonferroni"
)





















fisher_summary <- function(r, conf = 0.95) {
  
  z      <- atanh(r)
  z_mean <- mean(z, na.rm = TRUE)
  z_se   <- sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z)))
  z_crit <- qnorm((1 + conf) / 2)
  
  tibble(
    r_mean     = tanh(z_mean),
    r_ci_lower = tanh(z_mean - z_crit * z_se),
    r_ci_upper = tanh(z_mean + z_crit * z_se)
  )
}

between_country_cor <- function(data,
                                country_var,
                                split_var,
                                split_level = NULL,
                                target_vars) {
  
  data %>%
    group_by({{ cluster_1 }}) %>%
    mutate(
      split = ifelse(
        {{ split_var }} >= split_statistic({{ split_var }}, na.rm = TRUE),
        "high", "low"
      )
    ) %>%
    ungroup() %>%
    group_by(split, {{ cluster_1 }}, {{ cluster_2 }}) %>%
    summarise(mean_rating = mean({{ rating_var }}, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(
      names_from  = {{ country_var }},
      values_from = Rating
    ) %>%
    correlation::correlation() %>%
    filter(Parameter1 != Parameter2) %>%
    distinct(Parameter1, Parameter2, .keep_all = TRUE) %>%
    select(Parameter1, Parameter2, r) %>%
    pivot_longer(
      cols = c(Parameter1, Parameter2),
      names_to = "role",
      values_to = "cntry_lbl"
    ) %>%
    group_by(cntry_lbl) %>%
    summarise(fisher_summary(r), .groups = "drop")
}

cntxt_cor <- function(data,
                      mode = c("within", "between"),
                      split_var,
                      split_statistic = median,
                      split_level = NULL,
                      cluster_1,
                      cluster_2,
                      rating_var,
                      target_vars = NULL,
                      p_adjust = "bonferroni") {
  
  mode <- match.arg(mode)
  
  if (mode == "within") {
    
    data %>%
      group_by({{ cluster_1 }}) %>%
      mutate(
        split = ifelse(
          {{ split_var }} >= split_statistic({{ split_var }}, na.rm = TRUE),
          "high", "low"
        )
      ) %>%
      ungroup() %>%
      group_by(split, {{ cluster_1 }}, {{ cluster_2 }}) %>%
      summarise(mean_rating = mean({{ rating_var }}, na.rm = TRUE),
                .groups = "drop") %>%
      pivot_wider(
        names_from  = split,
        values_from = mean_rating
      ) %>%
      correlation::correlation(p_adjust = p_adjust)
    
  } else {
    
    stopifnot(!is.null(target_vars))
    
    between_country_cor(
      data        = data,
      country_var = {{ cluster_1 }},
      split_var   = {{ split_var }},
      split_level = split_level,
      target_vars = target_vars
    )
  }
}
cntxt_cor(
  ess4_long,
  mode = "between",
  cluster_1 = cntry_lbl,
  split_var = trd,
  target_vars = "prj_rating_scl"
)


btwn_cntry_low <- 
  ess4_long %>%
  group_by(cntry_lbl) %>%
  group_by(cntry_lbl,prj_target) %>%
  summarise(mean_rating = mean(prj_rating_scl,na.rm = T)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  #filter(trd_cntry_median  != "low") %>%
  tidyr::pivot_wider(names_from = cntry_lbl, values_from = mean_rating) %>%
  correlation::correlation() %>%
  data.frame() %>%
  filter(Parameter1 != Parameter2) %>%
  distinct(Parameter1, Parameter2, .keep_all = TRUE)

# long-Format: one row per country
btwn_cntry_low <- 
  btwn_cntry_low %>%
  select(Parameter1, Parameter2, r) %>%
  pivot_longer(cols = c(Parameter1, Parameter2),
               names_to = "role",
               values_to = "cntry_lbl") %>%
  #group_by(cntry_lbl) %>%
  summarise(
    mean_r      = mean(r, na.rm = TRUE),
    z_vals      = list(atanh(r)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    z_mean      = mean(unlist(z_vals)),
    z_se        = sd(unlist(z_vals)) / sqrt(length(unlist(z_vals))),
    z_crit      = qnorm(0.975),
    z_ci_lower  = z_mean - z_crit * z_se,
    z_ci_upper  = z_mean + z_crit * z_se,
    r_mean      = tanh(z_mean),
    r_ci_lower  = tanh(z_ci_lower),
    r_ci_upper  = tanh(z_ci_upper)
  ) %>%
  select(cntry_lbl, r_mean, r_ci_lower, r_ci_upper) %>%
  ungroup() %>%
  mutate(low_cntry = paste0(
    format(round(r_mean,2),nsmall = 2),
    " [",
    format(round(r_ci_lower,2),nsmall = 2),
    ", ",
    format(round(r_ci_upper,2),nsmall = 2),
    "]"
  ),.keep = "unused")



library(ggplot2)
std2_means <- 
  ess4_long %>%
  group_by(cntry_lbl) %>%
  mutate(
    trd_cntry_median = 
      case_when(
        trd >= median(trd,na.rm = T) ~ "high",
        trd <  median(trd,na.rm = T) ~ "low",
      )
  ) %>%
  group_by(trd_cntry_median,cntry_lbl,prj_target) %>%
  summarise(mean_rating = mean(prj_rating_scl,na.rm = T)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  mutate(
    trd_cntry_median = str_to_title(trd_cntry_median),
    trd_cntry_median = forcats::fct_rev(trd_cntry_median),
    prj_target = str_remove_all(prj_target,"prj_"),
    prj_target = forcats::fct_recode(prj_target,
                                     "age 20" = "age20",
                                     "age 70" = "age70",
                                     "gay people" = "gay"),
    prj_target = str_to_title(prj_target),
    cntry_lbl = str_wrap(cntry_lbl,10)
    ) %>%
  ggplot(aes(trd_cntry_median, mean_rating, group = prj_target)) +
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

  
ggsave(file = "./4_plots/std2_means.jpeg",width = 10, height = 10)












library(lme4)

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

ds1_long <-
  ds1 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1a)) %>%
  pivot_longer(cols = all_of(trgt.itms.1a),
               names_to = "target",
               values_to = "rating")



model1a_rwa = lmer(rating ~ 1 + (1|case) + (1|target) + (1|rwa), data = ds1_long)
model1a_sdo = lmer(rating ~ 1 + (1|case) + (1|target) + (1|sdo), data = ds1_long)

summary(model1a_rwa)
summary(model1a_sdo)



ds2_long <-
  ds2 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1b)) %>%
  pivot_longer(cols = all_of(trgt.itms.1b),
               names_to = "target",
               values_to = "rating")



model1b_rwa = lmer(rating ~ 1 + (1|case) + (1|target) + (1|rwa), data = ds2_long)
model1b_sdo = lmer(rating ~ 1 + (1|case) + (1|target) + (1|sdo), data = ds2_long)

summary(model1b_rwa)
summary(model1b_sdo)

bootstrap_icc(model = model1b_rwa,
              iterations = 1000,
              type = "parametric")

bootstrap_icc(model = model1b_sdo,
              iterations = 1000,
              type = "parametric")