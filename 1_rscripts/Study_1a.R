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


# ANALYSES ----

## INCONSISTENT PREDICTION ----

### BIVARIATE CORRELATIONS ----

#predictors
biv_r(data = ds1, vars1 = prdctrs,vars2 = NULL ,method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "none")

#rwa - targets
biv_r(data = ds1, vars1 = "rwa", vars2 = prj.trgts.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#sdo - targets
biv_r(data = ds1, vars1 = "sdo", vars2 = prj.trgts.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

### Correlations Target Factors
#rwa - targets
biv_r(data = ds1, vars1 = c("rwa","sdo"), vars2 = c("prj_liberal","prj_conservative"), method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")


### PARTIAL CORRELATIONS -----
partial <- expand_grid(
  var1    = c("rwa", "sdo"),
  var2    = c("prj_liberal", "prj_conservative"),
  partial = c(FALSE, TRUE)
) %>%
  pmap_dfr(function(var1, var2, partial) {
    
    select_vars <- if (partial) c(var1, "polid") else var1
    
    correlation::correlation(
      ds1,
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
lm_rwa.1a_no_cntrls <- lm(prj_rating ~ rwa_scl*trgt_fct, data = ds1_ana) #no controls
lm_rwa.1a_cntrls    <- lm(prj_rating ~ rwa_scl*trgt_fct + age + gender + education + polid, data = ds1_ana) #controlling for age, gender, education, political self placement

summary(lm_rwa.1a_no_cntrls)
summary(lm_rwa.1a_cntrls)

interactions::sim_slopes(lm_rwa.1a_no_cntrls, pred = rwa_scl, modx = trgt_fct)
interactions::sim_slopes(lm_rwa.1a_cntrls, pred = rwa_scl, modx = trgt_fct)

interactions::interact_plot(lm_rwa.1a_no_cntrls, pred = rwa_scl, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_rwa.1a_cntrls, pred = rwa_scl, modx = trgt_fct,interval = T)  

#sdo
lm_sdo.1a_no_cntrls <- lm(prj_rating ~ sdo_scl*trgt_fct, data = ds1_ana) #no controls
lm_sdo.1a_cntrls    <- lm(prj_rating ~ sdo_scl*trgt_fct + age + gender + education + polid, data = ds1_ana) #controlling for age, gender, education, political self placement

summary(lm_sdo.1a_no_cntrls)
summary(lm_sdo.1a_cntrls)

interactions::sim_slopes(lm_sdo.1a_no_cntrls, pred = sdo_scl, modx = trgt_fct)
interactions::sim_slopes(lm_sdo.1a_cntrls, pred = sdo_scl, modx = trgt_fct)

interactions::interact_plot(lm_sdo.1a_no_cntrls, pred = sdo_scl, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_sdo.1a_cntrls, pred = sdo_scl, modx = trgt_fct,interval = T)  

## Plot

ds1_ana %>%
  pivot_longer(cols = c(rwa_scl,sdo_scl), names_to = "predictor", values_to = "score_scl") %>%
  ggplot(aes(y = prj_rating, x = score_scl,color = trgt_fct)) +
  geom_smooth(se = T, method = lm) +
  facet_wrap(~predictor) +
  theme_minimal()


## HIGHLIGHTING TARGET VARIANCE ----
ds1_ana %>%
  pivot_longer(cols = c(rwa,sdo), names_to = "scale",values_to = "score") %>%
  pivot_longer(cols = c(prj_rating,prj_rating_scl,prj_rating_gmc), names_to = "trgt_centering",values_to = "prj_rating") %>%
  mutate(trgt_centering = factor(case_when(trgt_centering == "prj_rating" ~ "prj_rating_raw", TRUE ~ trgt_centering))) %>%
  group_by(scale) %>%
  mutate(scale_hilo = case_when(score > median(score,na.rm = T) ~ "high", TRUE ~ "low")) %>%
  group_by(scale,scale_hilo,prj_target,trgt_centering) %>%
  summarise(mean = mean(prj_rating,na.rm = T)) %>%
  pivot_wider(names_from = "scale_hilo", values_from = "mean") %>%
  mutate(diff = abs(high-low))

prej_cent.1a <- 
prej_cent.1a %>%
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
                      names_to = "prej_cent.1aer",
                      values_to = "prj_rating") %>%
  tidyr::pivot_wider(names_from = c("hilo","prej_cent.1aer"),
                     values_from = prj_rating) %>%
  group_by(scale) %>%
  mutate(diff_raw = abs(high_raw-low_raw),
         diff_gmc = abs(high_gmc-low_gmc),
         diff_scl = abs(high_scl-low_scl)) %>%
  select(target,scale,
         low_raw,high_raw,diff_raw,
         low_gmc,high_gmc,diff_gmc,
         low_scl,high_scl,diff_scl)

list(data = prej_cent.1a,
means = prej_cent.1a %>%
  summarise_if(is.numeric,mean, na.rm = TRUE),
sd = prej_cent.1a %>%
  summarise_if(is.numeric,sd, na.rm = TRUE),
corr = prej_cent.1a %>% summarise(r_raw = cor(low_raw,high_raw),
                              r_gmc = cor(low_gmc,high_gmc),
                              r_scl = cor(low_scl,high_scl))
)


prej_cent.1a %>% 
  select(!ends_with("_diff")) %>% 
  correlation::correlation() %>%
  data.frame() %>%
  filter(
    (Group == "rwa" & Parameter1 == "low_raw" & Parameter2 == "high_raw") |
      (Group == "rwa" & Parameter1 == "low_gmc" & Parameter2 == "high_gmc") |
      (Group == "rwa" & Parameter1 == "low_scl" & Parameter2 == "high_scl") |
      (Group == "sdo" & Parameter1 == "low_raw" & Parameter2 == "high_raw") |
      (Group == "sdo" & Parameter1 == "low_gmc" & Parameter2 == "high_gmc") |
      (Group == "sdo" & Parameter1 == "low_scl" & Parameter2 == "high_scl")
  ) %>%
  mutate(p = p.adjust(p, method = "bonferroni"))


## SHARED PREJUDICE ----


### SAMPLE SPLIT ----

split.1a <- 
  ds1 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1a)) %>%
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
     corr = split.1a %>% select(!ends_with("_diff")) %>% corrr::correlate())


split.1a %>% 
  select(!ends_with("_diff")) %>% 
  correlation::correlation() %>%
  data.frame() %>%
  filter(
    (Parameter1 == "rwa_high" & Parameter2 == "rwa_low") |
      (Parameter1 == "sdo_high" & Parameter2 == "sdo_low")
  ) %>%
  mutate(p = p.adjust(p, method = "bonferroni"))

### VARIANCE DECOMPOSITION ----

mlm_1a <- 
  ds1 %>%
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

ds1 %>%
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

ds1 %>%
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
                   ds1 %>%
                     mutate(gender = case_when(gender == 1 ~ "female",
                                               gender == 2 ~ "male",
                                               TRUE~ "other")) %>%
                     janitor::tabyl(gender) %>%
                     rename("variable"="gender") %>%
                     data.frame()
  ) %>% 
  plyr::rbind.fill(
    ds1 %>%
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

