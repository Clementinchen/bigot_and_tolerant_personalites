# LIBRARIES ----
library(tidyverse)
library(rlang)
library(ggplot2)
library(ggtext)
library(ggpubr)
library(lme4)


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
trgt_var <- function(data,
                     predictor,
                     prejudice,
                     pred_split = c("mean", "median", "1sd", "2sd"),
                     trgt_centering = c("raw", "gmc", "cwc", "z", "none")) {
  
  # -----------------------------
  # 1. Validate inputs
  # -----------------------------
  pred_split <- match.arg(pred_split)
  trgt_centering <- match.arg(trgt_centering)
  
  if (trgt_centering == "none") {
    trgt_centering <- "raw"
  }
  
  if (!all(c(predictor, prejudice) %in% names(data))) {
    stop("Predictor or prejudice variables not found in data.")
  }
  
  # -----------------------------
  # 2. Select & reshape data
  # -----------------------------
  data <- data %>%
    dplyr::select(dplyr::all_of(c(predictor, prejudice))) %>%
    dplyr::rename(predictor = .data[[predictor]]) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(prejudice),
      names_to = "prejudice_target",
      values_to = "prj_rating_raw"
    )
  
  # -----------------------------
  # 3. Compute centering variables
  # -----------------------------
  grand_mean <- mean(data$prj_rating_raw, na.rm = TRUE)
  
  data <- data %>%
    dplyr::group_by(prejudice_target) %>%
    dplyr::mutate(
      prj_clstr_mean = mean(prj_rating_raw, na.rm = TRUE),
      prj_rating_z   = as.numeric(scale(prj_rating_raw))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      prj_grand_mean = grand_mean,
      prj_rating_gmc = prj_rating_raw - prj_grand_mean,
      prj_rating_cwc = prj_rating_raw - prj_clstr_mean
    )
  
  # -----------------------------
  # 4. Predictor statistics
  # -----------------------------
  pred_mean   <- mean(data$predictor, na.rm = TRUE)
  pred_median <- median(data$predictor, na.rm = TRUE)
  pred_sd     <- sd(data$predictor, na.rm = TRUE)
  
  data <- data %>%
    dplyr::mutate(
      predictor_split_mean = dplyr::case_when(
        is.na(predictor) ~ NA_character_,
        predictor > pred_mean ~ "high",
        TRUE ~ "low"
      ),
      predictor_split_median = dplyr::case_when(
        is.na(predictor) ~ NA_character_,
        predictor > pred_median ~ "high",
        TRUE ~ "low"
      ),
      predictor_split_1sd = dplyr::case_when(
        is.na(predictor) ~ NA_character_,
        predictor > (pred_median + pred_sd) ~ "high",
        predictor < (pred_median - pred_sd) ~ "low",
        TRUE ~ NA_character_
      ),
      predictor_split_2sd = dplyr::case_when(
        is.na(predictor) ~ NA_character_,
        predictor > (pred_median + 2 * pred_sd) ~ "high",
        predictor < (pred_median - 2 * pred_sd) ~ "low",
        TRUE ~ NA_character_
      )
    )
  
  # -----------------------------
  # 5. Dynamic variable selection
  # -----------------------------
  group_var <- paste0("predictor_split_", pred_split)
  prj_var   <- paste0("prj_rating_", trgt_centering)
  
  # -----------------------------
  # 6. Aggregate & reshape
  # -----------------------------
  result <- data %>%
    dplyr::group_by(prejudice_target, .data[[group_var]]) %>%
    dplyr::summarise(
      prejudice_mean = mean(.data[[prj_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(.data[[group_var]])) %>% 
    tidyr::pivot_wider(
      names_from = .data[[group_var]],
      values_from = prejudice_mean
    ) %>%
    dplyr::mutate(
      absolute_diff = abs(high - low),
      prejudice_target = prejudice_target |>
        stringr::str_replace_all("[[:punct:]]", " ") |>
        stringr::str_to_title()
    )
  
  # -----------------------------
  # 7. Correlation
  # -----------------------------
  cor_hilo <- correlation::correlation(
    result,
    select = c("high", "low"),
    method = "pearson",
    missing = "keep_pairwise"
  )
  
  # -----------------------------
  # 8. Output
  # -----------------------------
  cat(
    "\nMean prejudice ratings by target group within the high and low",
    toupper(predictor),
    "subsample.\nTarget Centering:",
    toupper(trgt_centering),
    "\n\n"
  )
  
  print(result, n = nrow(result))
  
  cat(
    "\nCorrelation in prejudice ratings between high and low",
    toupper(predictor),
    "subsamples\n"
  )
  
  print(cor_hilo)
  
  # return invisibly for programmatic use
  invisible(list(
    data = result,
    correlation = cor_hilo
  ))
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

plot_int_prd.trgt <- function(id_var, data, pred_vars, prj_targets, title = NULL) {
  
  if (!all(c(pred_vars, prj_targets) %in% names(data))) {
    stop("Predictor or prejudice variables not found in data.")
  }
  
  data <- 
    data %>%
    dplyr::select(dplyr::all_of(c(id_var, pred_vars, prj_targets))) %>%
    dplyr::mutate(across(-{{ id_var }}, ~as.numeric(scale(.)))) %>%
    tidyr::pivot_longer(cols = all_of(pred_vars), names_to = "scale", values_to = "score") %>%
    tidyr::pivot_longer(cols = all_of(prj_targets), names_to = "trgt_fct", values_to = "prejudice") %>%
    dplyr::mutate(
      trgt_fct = as.factor(dplyr::case_when(
        stringr::str_detect(trgt_fct, "con.grps") ~ "Conservative",
        stringr::str_detect(trgt_fct, "lib.grps") ~ "Liberal"
      )),
      scale = stringr::str_to_upper(scale)
    )
  
  if (is.null(title)) title <- "Default Title"
  
  cors <- 
    data %>%
    group_by(scale,trgt_fct) %>%
    summarise(r = cor(score,prejudice,use = "pairwise",method = "pearson")) %>%
    mutate(name = paste(scale, trgt_fct, sep = "_"),
           r = round(r,2),
           r = str_replace_all(r,"0.",".")) %>%
    ungroup() %>%
    select(name,r) %>%
    deframe()
  
  print(cors)
  
  ggplot2::ggplot(data, ggplot2::aes(y = prejudice, x = score, color = trgt_fct, linetype = scale)) +
    ggplot2::geom_smooth(se = FALSE, method = "lm", linewidth = 0.9) +
    ggplot2::ggtitle(title) +
    labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")) + 
    scale_color_manual(values = c(
      "Liberal" = "#336699",  
      "Conservative" = "#990000")) +
    labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    annotate(geom = "richtext", x = 2, y = -0.15, 
             label = paste0("<i>r<sub>RWA</sub></i> = ",cors["RWA_Conservative"]),
             hjust = 0,
             size = 4, 
             fill = NA, 
             label.color = NA,
             colour = "#990000") +
    annotate(geom = "richtext", x = 2, y = -.35, 
             label = paste0("<i>r<sub>SDO</sub></i> = ",cors["SDO_Conservative"]),
             hjust = 0,
             size = 4, 
             fill = NA, 
             label.color = NA,
             colour = "#990000") +
    annotate(geom = "richtext", x = 2, y = .9, 
             label = paste0("<i>r<sub>RWA</sub></i> = ",cors["RWA_Liberal"]),
             hjust = 0,
             size = 4, 
             fill = NA, 
             label.color = NA,
             colour = "#336699") +
    annotate(geom = "richtext", x =2, y = .7, 
             label = paste0("<i>r<sub>SDO</sub></i> = ",cors["SDO_Liberal"]),
             hjust = 0,
             size = 4, 
             fill = NA, 
             label.color = NA,
             colour = "#336699")

}


plot_trgt_means <- function(data, predictors, prejudice,
                            pred_split = "median",
                            trgt_centering = "raw") {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(correlation)
  
  # -----------------------------
  # 1. Daten erzeugen
  # -----------------------------
  plot_data <- map_dfr(predictors, function(pred) {
    
    trgt_var(
      data = data,
      predictor = pred,
      prejudice = prejudice,
      pred_split = pred_split,
      trgt_centering = trgt_centering
    )$data %>%
      
      mutate(scale = toupper(pred))
    
  })
  
  
  # -----------------------------
  # 2. Korrelationen berechnen
  # -----------------------------
  cor_df <- plot_data %>%
    group_by(scale) %>%
    summarise(
      r = correlation::correlation(
        data.frame(high, low),
        method = "pearson"
      )$r[1],
      .groups = "drop"
    )
  
  # Label bauen (für Plot)
  cor_label <- paste0(
    paste0(
      "r<sub>", cor_df$scale, "</sub> = ",
      sprintf("%.2f", cor_df$r)
    ),
    collapse = ", "
  )
  
  # -----------------------------
  # 3. Plot-Format
  # -----------------------------
  plot_data_long <- plot_data %>%
    select(-absolute_diff) %>%
    pivot_longer(cols = c("high", "low"),
                 names_to = "hilo",
                 values_to = "mean") %>%
    mutate(
      hilo = str_to_title(hilo),
      hilo = forcats::fct_rev(hilo),
      trgt_fct = case_when(
        str_detect(prejudice_target, "Lib") ~ "Liberal",
        str_detect(prejudice_target, "Con") ~ "Conservative",
        TRUE ~ "Other"
      )
    )
  
  y_max <- max(plot_data_long$mean, na.rm = TRUE)
  
  # -----------------------------
  # 4. Plot
  # -----------------------------
  p <- ggplot(plot_data_long,
              aes(x = hilo, y = mean,
                  group = interaction(prejudice_target, scale, trgt_fct),
                  color = trgt_fct,
                  linetype = scale)) +
    geom_point(size = 1.5) +
    geom_line(size = .9) +
    scale_color_manual(values = c(
      "Liberal" = "#336699",
      "Conservative" = "#990000",
      "Other" = "grey50"
    )) +
    scale_x_discrete(expand = c(0.03, 0.03)) +
    theme_minimal() +
    
    geom_segment(aes(x = "Low", xend = "High",
                     y = y_max + (y_max/20), yend = y_max + (y_max/20)),
                 inherit.aes = FALSE,
                 color = "black", size = 1) +
    
    annotate(
      geom = "richtext",
      x = 1.5,
      y = y_max + (y_max/10),
      label = paste0("<i>", cor_label, "</i>"),
      size = 5,
      fill = NA,
      label.color = NA
    ) +
    
    labs(
      y = "Prejudice",
      x = "Predictor",
      color = "Target",
      linetype = "Scale"
    ) +
    theme(legend.position = "bottom")
  
  print(p)
  
  # -----------------------------
  # 5. Rückgabe
  # -----------------------------
  invisible(list(
    plot = p,
    data = plot_data_long,
    correlations = cor_df
  ))
}



# MAIN TEXT ----

## descriptives ----

ds1_wde %>%
  select(all_of(prdctrs),starts_with("prj")) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  #group_by(variable) %>%
  reframe(Mean = mean(value, na.rm = T),
          SD = sd(value, na.rm = T),
          Median = median(value, na.rm = T),
          Min = min(value,na.rm = T),
          Max = max(value, na.rm = T),
          range = abs(Max-Min),
          .by = variable) %>%
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

## demographics ----

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
                     janitor::tabyl(gender) %>%
                     select(-valid_percent) %>%
                     mutate(variable = "gender") %>%
                     rename("response" = "gender") %>%
                     data.frame()
  ) %>% 
  plyr::rbind.fill(
    ds1_wde %>%
      janitor::tabyl(education) %>%
      select(-valid_percent) %>%
      mutate(variable = "education") %>%
      rename("response" = "education") %>%
      data.frame()
  )%>%
  sjPlot::tab_df()

## 1. bivariate correlations (Replication Chambers et al., 2013) ----

### predictors ----
biv_r(data = ds1_wde, vars1 = prdctrs,vars2 = NULL ,method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "none")

### predictor - targets ----

#rwa - targets
biv_r(data = ds1_wde, vars1 = "rwa", vars2 = prj_all.grps.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#sdo - targets
biv_r(data = ds1_wde, vars1 = "sdo", vars2 = prj_all.grps.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#polid - targets
biv_r(data = ds1_wde, vars1 = "polid", vars2 = prj_all.grps.1a, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")


#Correlations Target Factors
#predictors - targets
biv_r(data = ds1_wde, vars1 = c("rwa","sdo","polid"), vars2 = c("prj_agg.lib.grps","prj_agg.con.grps"), method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

### left-right interaction ----

#prepare long data set with relevant variables
lm_dat.1a <-
  ds1_wde %>%
  select(case,rwa,sdo,prj_agg.con.grps,prj_agg.lib.grps) %>%
  mutate(across(-case,~as.numeric(scale(.)))) %>%
  pivot_longer(cols = c(prj_agg.con.grps,prj_agg.lib.grps),
               names_to = "trgt_fct",
               values_to = "prj_mean")

#put controls in
lm_dat.1a <-
  lm_dat.1a %>%
  full_join(.,
            ds1_wde %>%
              select(case,age,gender,education,polid),
            by = "case")


#run models
#rwa
lm_rwa.1a_no_cntrls <- lm(prj_mean ~ rwa*trgt_fct, data = lm_dat.1a) #no controls
lm_rwa.1a_cntrls    <- lm(prj_mean ~ rwa*trgt_fct + age + gender + education + polid, data = lm_dat.1a) #controlling for age, gender, education, political self placement

summary(lm_rwa.1a_no_cntrls)
summary(lm_rwa.1a_cntrls)

interactions::sim_slopes(lm_rwa.1a_no_cntrls, pred = rwa, modx = trgt_fct)
interactions::sim_slopes(lm_rwa.1a_cntrls,    pred = rwa, modx = trgt_fct)

interactions::interact_plot(lm_rwa.1a_no_cntrls, pred = rwa, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_rwa.1a_cntrls, pred = rwa, modx = trgt_fct,interval = T)  

#sdo
lm_sdo.1a_no_cntrls <- lm(prj_mean ~ sdo*trgt_fct, data = lm_dat.1a) #no controls
lm_sdo.1a_cntrls    <- lm(prj_mean ~ sdo*trgt_fct + age + gender + education + polid, data = lm_dat.1a) #controlling for age, gender, education, political self placement

summary(lm_sdo.1a_no_cntrls)
summary(lm_sdo.1a_cntrls)

interactions::sim_slopes(lm_sdo.1a_no_cntrls, pred = sdo, modx = trgt_fct)
interactions::sim_slopes(lm_sdo.1a_cntrls, pred = sdo, modx = trgt_fct)

interactions::interact_plot(lm_sdo.1a_no_cntrls, pred = sdo, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_sdo.1a_cntrls, pred = sdo, modx = trgt_fct,interval = T)  

## 2. Comparing within- and between-target variance ----

#function
#data = data in wide format
#predictor = select individual difference variable
#prejudice = select one or more prejudice variable
#select stat to split the sample along the predicotr pred_split = c("mean", "median", "1sd", "2sd"),
#how should targets be centered, considering between target variance trgt_centering = c("raw", "gmc", "cwc", "z", "none"

#RWA
trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds1_wde,predictor = "rwa",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "z")

#SDO
trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds1_wde,predictor = "sdo",prejudice = prj_all.grps.1a,pred_split = "median",trgt_centering = "z")


## 3. Multilevel variance decomposition ----

var_decomp_1a <- 
  ds1_wde %>%
  select(case,rwa,sdo,starts_with("prj.")) %>%
  tidyr::pivot_longer(cols = starts_with("prj"),
                      names_to = "target",
                      values_to = "rating") %>%
  filter(complete.cases(.))

mod_rwa_1a = lmer(rating ~ 1 + (1|case) + (1|target) + (1|rwa), data = var_decomp_1a)
mod_sdo_1a = lmer(rating ~ 1 + (1|case) + (1|target) + (1|sdo), data = var_decomp_1a)

summary(mod_rwa_1a)
summary(mod_sdo_1a)

bootstrap_icc(mod_rwa_1a,iterations = 1000,type = "parametric")
bootstrap_icc(mod_sdo_1a,iterations = 1000,type = "parametric")

sjPlot::tab_model(mod_rwa_1a, show.std = T,show.est = F)
sjPlot::tab_model(mod_sdo_1a, show.std = T,show.est = F)

## Study 2 ----

### descriptives ----

ds2_wde %>%
  select(all_of(prdctrs),starts_with("prj")) %>%
  tidyr::pivot_longer(cols = 1:ncol(.),names_to = "variable") %>%
  #group_by(variable) %>%
  reframe(Mean = mean(value, na.rm = T),
          SD = sd(value, na.rm = T),
          Median = median(value, na.rm = T),
          Min = min(value,na.rm = T),
          Max = max(value, na.rm = T),
          range = abs(Max-Min),
          .by = variable) %>%
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

### demographics ----

ds2_wde %>%
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
                   ds2_wde %>%
                     janitor::tabyl(gender) %>%
                     select(-valid_percent) %>%
                     mutate(variable = "gender") %>%
                     rename("response" = "gender") %>%
                     data.frame()
  ) %>% 
  plyr::rbind.fill(
    ds2_wde %>%
      janitor::tabyl(education) %>%
      select(-valid_percent) %>%
      mutate(variable = "education") %>%
      rename("response" = "education") %>%
      data.frame()
  )%>%
  sjPlot::tab_df()

## 1. bivariate correlations (Replication Chambers et al., 2013) ----

### predictors ----
biv_r(data = ds2_wde, vars1 = prdctrs,vars2 = NULL ,method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "none")

### predictor - targets ----

#rwa - targets
biv_r(data = ds2_wde, vars1 = "rwa", vars2 = prj_all.grps.1b, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#sdo - targets
biv_r(data = ds2_wde, vars1 = "sdo", vars2 = prj_all.grps.1b, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

#polid - targets
biv_r(data = ds2_wde, vars1 = "polid", vars2 = prj_all.grps.1b, method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")


#Correlations Target Factors
#predictors - targets
biv_r(data = ds2_wde, vars1 = c("rwa","sdo","polid"), vars2 = c("prj_agg.lib.grps","prj_agg.con.grps"), method  = "pearson",missing = "keep_pairwise",ci = .95, p_adjust = "holm")

### left-right interaction ----

#prepare long data set with relevant variables
lm_dat.1b <-
  ds2_wde %>%
  select(case,rwa,sdo,prj_agg.con.grps,prj_agg.lib.grps) %>%
  mutate(across(-case,~as.numeric(scale(.)))) %>%
  pivot_longer(cols = c(prj_agg.con.grps,prj_agg.lib.grps),
               names_to = "trgt_fct",
               values_to = "prj_mean")

#put controls in
lm_dat.1b <-
  lm_dat.1b %>%
  full_join(.,
            ds2_wde %>%
              select(case,age,gender,education,polid),
            by = "case")


#run models
#rwa
lm_rwa.1b_no_cntrls <- lm(prj_mean ~ rwa*trgt_fct, data = lm_dat.1b) #no controls
lm_rwa.1b_cntrls    <- lm(prj_mean ~ rwa*trgt_fct + age + gender + education + polid, data = lm_dat.1b) #controlling for age, gender, education, political self placement

summary(lm_rwa.1b_no_cntrls)
summary(lm_rwa.1b_cntrls)

interactions::sim_slopes(lm_rwa.1b_no_cntrls, pred = rwa, modx = trgt_fct)
interactions::sim_slopes(lm_rwa.1b_cntrls,    pred = rwa, modx = trgt_fct)

interactions::interact_plot(lm_rwa.1b_no_cntrls, pred = rwa, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_rwa.1b_cntrls, pred = rwa, modx = trgt_fct,interval = T)  

#sdo
lm_sdo.1b_no_cntrls <- lm(prj_mean ~ sdo*trgt_fct, data = lm_dat.1b) #no controls
lm_sdo.1b_cntrls    <- lm(prj_mean ~ sdo*trgt_fct + age + gender + education + polid, data = lm_dat.1b) #controlling for age, gender, education, political self placement

summary(lm_sdo.1b_no_cntrls)
summary(lm_sdo.1b_cntrls)

interactions::sim_slopes(lm_sdo.1b_no_cntrls, pred = sdo, modx = trgt_fct)
interactions::sim_slopes(lm_sdo.1b_cntrls, pred = sdo, modx = trgt_fct)

interactions::interact_plot(lm_sdo.1b_no_cntrls, pred = sdo, modx = trgt_fct,interval = T)  
interactions::interact_plot(lm_sdo.1b_cntrls, pred = sdo, modx = trgt_fct,interval = T)  

## 2. Comparing within- and between-target variance ----

#function
#data = data in wide format
#predictor = select individual difference variable
#prejudice = select one or more prejudice variable
#select stat to split the sample along the predicotr pred_split = c("mean", "median", "1sd", "2sd"),
#how should targets be centered, considering between target variance trgt_centering = c("raw", "gmc", "cwc", "z", "none"

#RWA
trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds2_wde,predictor = "rwa",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "z")

#SDO
trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "raw")
trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "cwc")
trgt_var(data = ds2_wde,predictor = "sdo",prejudice = prj_all.grps.1b,pred_split = "median",trgt_centering = "z")


## 3. Multilevel variance decomposition ----

var_decomp_1b <- 
  ds2_wde %>%
  select(case,rwa,sdo,starts_with("prj.")) %>%
  tidyr::pivot_longer(cols = starts_with("prj"),
                      names_to = "target",
                      values_to = "rating") %>%
  filter(complete.cases(.))

mod_rwa_1b = lmer(rating ~ 1 + (1|case) + (1|target) + (1|rwa), data = var_decomp_1b)
mod_sdo_1b = lmer(rating ~ 1 + (1|case) + (1|target) + (1|sdo), data = var_decomp_1b)

summary(mod_rwa_1b)
summary(mod_sdo_1b)

bootstrap_icc(mod_rwa_1b,iterations = 1000,type = "parametric")
bootstrap_icc(mod_sdo_1b,iterations = 1000,type = "parametric")

sjPlot::tab_model(mod_rwa_1b, show.std = T,show.est = F)
sjPlot::tab_model(mod_sdo_1b, show.std = T,show.est = F)

# FIGURE 2 ----

plot_int_prd.trgt(data = ds1_wde,
                  id_var = "case",
                  pred_vars   = c("rwa","sdo"),
                  prj_targets = c("prj_agg.con.grps","prj_agg.lib.grps"),
                  title = "Study 1")

plot_int_prd.trgt(data = ds2_wde,
                  id_var = "case",
                  pred_vars   = c("rwa","sdo"),
                  prj_targets = c("prj_agg.con.grps","prj_agg.lib.grps"),
                  title = "Study 2")

plot_trgt_means(
  data = ds1_wde,
  predictors = c("rwa", "sdo"),
  prejudice = prj_all.grps.1a,
  pred_split = "median"
)

plot_trgt_means(
  data = ds2_wde,
  predictors = c("rwa", "sdo"),
  prejudice = prj_all.grps.1b,
  pred_split = "median"
)


#merge single plots to one
fig2 <-
  ggarrange(
  #correlations 1a
  plot_int_prd.trgt(data = ds1_wde,
                    id_var = "case",
                    pred_vars   = c("rwa","sdo"),
                    prj_targets = c("prj_agg.con.grps","prj_agg.lib.grps"),
                    title = "Study 1"
                    ) + rremove("ylab"),
  
  #correlations 1b
  plot_int_prd.trgt(data = ds2_wde,
                    id_var = "case",
                    pred_vars   = c("rwa","sdo"),
                    prj_targets = c("prj_agg.con.grps","prj_agg.lib.grps"),
                    title = "Study 2"
                    )+ rremove("ylab"),
  
  #prejudice means 1a
  plot_trgt_means(
    data = ds1_wde,
    predictors = c("rwa", "sdo"),
    prejudice = prj_all.grps.1a,
    pred_split = "median"
  )$p + rremove("xlab")+rremove("ylab"),
  
  #prejudice means 1b
  plot_trgt_means(
    data = ds2_wde,
    predictors = c("rwa", "sdo"),
    prejudice = prj_all.grps.1b,
    pred_split = "median"
  )$p + rremove("xlab")+rremove("ylab"),
  
  common.legend = T, 
  ncol = 2,
  nrow = 2,
  align = "v",
  labels = c(
    "A","A"
    ,"B","B"
  ),
  legend = "top"
)
  
fig2 <- 
  annotate_figure(fig2, left = text_grob("Prejudice", rot = 90),
                           bottom = text_grob("Predictor"))
fig2 <- 
  annotate_figure(fig2, bottom = text_grob(
  paste("A: Correlations of RWA and SDO (the \"mirror-image\" pattern) with averaged scores (i.e., between-target variance removed) of \n      liberal and conservative prejudice target groups",
        "B: Correlation between high/low RWA/SDO subsample for prejudice ratings (between target variance is considered)",
        sep = "\n"),
  face = "italic",hjust = 0, x = unit(5.5,"pt")))


fig2
ggsave(file = "./4_plots/figure2.jpeg",width = 10, height = 10)

# SUPPLEMENTARY MATERIALS ----

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