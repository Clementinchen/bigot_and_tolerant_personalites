# DEFINE FUNCTIONS ----

biv_r <- 
  function(data,vars1,vars2,method,missing, ci,p_adjust) {
    res <- data %>% correlation::correlation(select = vars1,
                                      select2 = vars2,
                                      method = method,
                                      missing = missing,
                                      ci = ci,
                                      p_adjust = p_adjust)
    res <- as_tibble(res) %>%
      mutate(Var1 = Parameter1,
             Var2 = Parameter2,
             N = n_Obs,
             r = r,
             CI = paste0("[",
                         format(round(CI_low,2),nsmall = 2),
                         ", ",
                         format(round(CI_high,2),nsmall = 2),
                         "]"),
             p_scientific = p,
             p_star = case_when(
               p < .001 ~ "p < .001",
               p < .01  ~ "p < .01",
               p < .05 ~ "p < .05",
               TRUE ~ "n.s."),
             .keep = "none"
             )
    res %>% select(Var1,Var2,N,r,CI,p_scientific,p_star) %>% rename_with(.,~paste0("CI"," (",gsub("^0+([\\.])","",ci),"%",")"),"CI") %>% print(n = nrow(.))
  }

## Target Variance
library(rlang)
trgt_var <- 
  function(data, predictor, pred_split, trgt_centering) {
    
    data <- data %>%
      mutate(
        prj_rating_gmc = prj_rating - mean(prj_rating, na.rm = TRUE)
      )
    
    # ---- hier sauberer Kontrollfluss ----
    if (pred_split == "median") {
      data <- data %>%
        mutate(
          pred_hilo = case_when(
            {{ predictor }} > median({{ predictor }}, na.rm = TRUE) ~  "high", TRUE ~ "low"
          )
        )
    }
    
    if (pred_split == "mean") {
      data <- data %>%
        mutate(
          pred_hilo = case_when(
            {{ predictor }} > mean({{ predictor }}, na.rm = TRUE) ~ "high", TRUE ~ "low"
          )
        )
    }
    data <- data %>% filter(!is.na(pred_hilo))
    res <- data %>%
      group_by(prj_target) %>%
      mutate(
        prj_rating_scl = as.numeric(scale(prj_rating)),
        prj_rating_cwc = prj_rating - mean(prj_rating, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = c(prj_rating, prj_rating_scl, prj_rating_gmc, prj_rating_cwc),
        names_to = "trgt_cent",
        values_to = "prj_rating"
      ) %>%
      mutate(
        trgt_cent = if_else(
          trgt_cent == "prj_rating",
          "prj_rating_none",
          trgt_cent
        )
      ) %>%
      group_by(pred_hilo, prj_target, trgt_cent) %>%
      summarise(mean = mean(prj_rating, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = "pred_hilo", values_from = "mean") %>%
      mutate(diff = abs(high - low))
    
    res <- res %>%
      mutate(
        trgt_cent = str_remove_all(trgt_cent,"prj_rating_"),
        trgt_cent = recode(trgt_cent,
                           "scl" = "z-score")
      )
    
    if (!trgt_centering %in% c("none","cwc","gmc","z-score")) {
      stop("trgt_centering must be one of: none, cwc, gmc, z-score")
    }
    
    res <- res %>%
      filter(trgt_cent == trgt_centering) %>%
      select(-trgt_cent)
    
    cor_hilo <- res %>%
      correlation::correlation(
        select = c("high","low"),
        method = "pearson",
        missing = "keep_pairwise"
      )
    
    cat("\nMean prejudice ratings by target group within the high and low",toupper(deparse(substitute(predictor))),"subsample. Target Centering:",toupper(trgt_centering),"\n\n")
    print(res, n = nrow(res))
    
    cat("\nCorrelation in prejudice ratings between high and low",toupper(deparse(substitute(predictor))),"subsamples\n")
    print(cor_hilo)
    
  }


# ANALYSES ----

## INCONSISTENT PREDICTION ----

### BIVARIATE CORRELATIONS ----

#predictors
biv_r(data = ds1_wde, vars1 = prdctrs,vars2 = NULL ,method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "none")

#rwa - targets
biv_r(data = ds1_wde, vars1 = "rwa", vars2 = prj.trgts.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#sdo - targets
biv_r(data = ds1_wde, vars1 = "sdo", vars2 = prj.trgts.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

### Correlations Target Factors
#rwa - targets
biv_r(data = ds1_wde, vars1 = c("rwa","sdo"), vars2 = c("prj_liberal","prj_conservative"), method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")


### PARTIAL CORRELATIONS -----
partial <- expand_grid(
  var1    = c("rwa", "sdo"),
  var2    = c("prj_liberal", "prj_conservative"),
  partial = c(FALSE, TRUE)
) %>%
  pmap_dfr(function(var1, var2, partial) {
    
    select_vars <- if (partial) c(var1, "polid") else var1
    
    correlation::correlation(
      ds1_wde,
      select  = select_vars,
      select2 = var2,
      partial = partial,
      p_adjust = "none"
    ) %>%
      as_tibble() %>%
      { if (partial) slice(., 1) else . } %>%
      mutate(
        correlation_type = if_else(
          partial,
          "partial_polid",
          "bivariate"
        )
      )
  })

partial[,c(1,2,12,11,3:6,9)]

## IDEOLOGICAL CONFLICT ----

### Left-right interaction ----

#rwa
lm_rwa.1a_no_cntrls <- lm(prj_rating ~ rwa_scl*trgt_fct, data = ds1_lng) #no controls
lm_rwa.1a_cntrls    <- lm(prj_rating ~ rwa_scl*trgt_fct + age + gender + education + polid, data = ds1_lng) #controlling for age, gender, education, political self placement

summary(lm_rwa.1a_no_cntrls)
summary(lm_rwa.1a_cntrls)

interactions::sim_slopes(lm_rwa.1a_no_cntrls, pred = rwa_scl, modx = trgt_fct)
interactions::sim_slopes(lm_rwa.1a_cntrls, pred = rwa_scl, modx = trgt_fct)

interactions::interact_plot(lm_rwa.1a_no_cntrls, pred = rwa_scl, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_rwa.1a_cntrls, pred = rwa_scl, modx = trgt_fct,interval = T)  

#sdo
lm_sdo.1a_no_cntrls <- lm(prj_rating ~ sdo_scl*trgt_fct, data = ds1_lng) #no controls
lm_sdo.1a_cntrls    <- lm(prj_rating ~ sdo_scl*trgt_fct + age + gender + education + polid, data = ds1_lng) #controlling for age, gender, education, political self placement

summary(lm_sdo.1a_no_cntrls)
summary(lm_sdo.1a_cntrls)

interactions::sim_slopes(lm_sdo.1a_no_cntrls, pred = sdo_scl, modx = trgt_fct)
interactions::sim_slopes(lm_sdo.1a_cntrls, pred = sdo_scl, modx = trgt_fct)

interactions::interact_plot(lm_sdo.1a_no_cntrls, pred = sdo_scl, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_sdo.1a_cntrls, pred = sdo_scl, modx = trgt_fct,interval = T)  

## Plot

ds1_lng %>%
  pivot_longer(cols = c(rwa_scl,sdo_scl), names_to = "predictor", values_to = "score_scl") %>%
  ggplot(aes(y = prj_rating, x = score_scl,color = trgt_fct)) +
  geom_smooth(se = T, method = lm) +
  facet_wrap(~predictor) +
  theme_minimal()


## HIGHLIGHTING TARGET VARIANCE ----
trgt_var(data = ds1_lng,predictor = rwa,pred_split = "median",trgt_centering = "none")
trgt_var(data = ds1_lng,predictor = rwa,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds1_lng,predictor = rwa,pred_split = "median",trgt_centering = "z-score")

trgt_var(data = ds1_lng,predictor = sdo,pred_split = "median",trgt_centering = "none")
trgt_var(data = ds1_lng,predictor = sdo,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds1_lng,predictor = sdo,pred_split = "median",trgt_centering = "z-score")


## SHARED PREJUDICE ----


### SAMPLE SPLIT ----

split.1a <- 
  ds1_wde %>%
  select(case,rwa,sdo,all_of(prj.trgts.1a)) %>%
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
  
  
list(data = split.1a,
     means = split.1a %>%
       summarise_if(is.numeric,mean, na.rm = TRUE),
     sd = split.1a %>%
       summarise_if(is.numeric,sd, na.rm = TRUE),
     corr = split.1a %>% select(!ends_with("_diff")) %>% correlation::correlation()
     )


### VARIANCE DECOMPOSITION ----

mlm_1a <- 
  ds1_wde %>%
  select(case,rwa,sdo,starts_with("prj")) %>%
  tidyr::pivot_longer(cols = starts_with("prj"),
                      names_to = "target",
                      values_to = "rating")

mlm_rwa_1a <- lme4::lmer(rating ~ rwa + (rwa|target), data = mlm_1a)
summary(mlm_rwa_1a)

mlm_sdo_1a <- lme4::lmer(rating ~ sdo + (sdo|target), data = mlm_1a)
summary(mlm_sdo_1a)

sjPlot::tab_model(mlm_rwa_1a, show.std = T,show.est = F)
sjPlot::tab_model(mlm_sdo_1a, show.std = T,show.est = F)

sjPlot::tab_model(mlm_rwa_1a,mlm_sdo_1a,show.std = T,show.est = F,
                  file = "variance_decomp_mlm_study1a.html")

r2mlm::r2mlm(mlm_rwa_1a)$R2s %>%
  round(.,4)
r2mlm::r2mlm(mlm_sdo_1a)$R2s %>%
  round(.,4)

# DESCRIPTIVES ----

ds1_wde %>%
  select(all_of(prdctrs),starts_with("prj"),prj_liberal,prj_conservative) %>%
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

ds1_wde %>%
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
                   ds1_wde %>%
                     mutate(gender = case_when(gender == 1 ~ "female",
                                               gender == 2 ~ "male",
                                               TRUE~ "other")) %>%
                     janitor::tabyl(gender) %>%
                     rename("variable"="gender") %>%
                     data.frame()
  ) %>% 
  plyr::rbind.fill(
    ds1_wde %>%
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

