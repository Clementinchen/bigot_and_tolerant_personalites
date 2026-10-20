
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





library(tidyverse)


raw_data = read.csv(
  file = './0_data/personality_group_prejudice_study_1_utf8.csv', 
  header = TRUE, sep = ";", na.strings = c("","-9","-1"),encoding = "UTF-8",
)

cdbk_3 <- openxlsx::read.xlsx("./0_data/collective_nature_of_pejudice_codebook.xlsx", sheet = 3)

ds <-
  raw_data %>%
  filter(CO02 == 1 & MAXPAGE > 34)



rename_vars.3 <- setNames(cdbk_3 %>%
                             pull(new_label),
                          cdbk_3 %>%
                             pull(old_label))

ds <- ds %>%
  rename_with(~ rename_vars.3[.],.cols = all_of(names(rename_vars.3)))

ds <- ds %>% 
  janitor::clean_names()

ds <- 
  ds %>% 
  select(
    case,
    starts_with("rwa_"),#rwa
    
    starts_with("sdo_"),#sdo
    
    starts_with("prj_"),#targets
    
    age,
    gender,
    polid
    
    
  )

psych::alpha(ds %>% select(starts_with("rwa_") & ends_with("w1")))
psych::alpha(ds %>% select(starts_with("rwa_") & ends_with("w2")))
psych::alpha(ds %>% select(starts_with("rwa_") & ends_with("w3")))

psych::alpha(ds %>% select(starts_with("sdo_") & ends_with("w1")))
psych::alpha(ds %>% select(starts_with("sdo_") & ends_with("w2")))
psych::alpha(ds %>% select(starts_with("sdo_") & ends_with("w3")))



seq_along(1:9)

ds <-
  ds %>% 
  mutate(
    rwa_w1 = rowMeans(ds[paste0("rwa_0",seq_along(1:9),"_w1")],na.rm = T),
    rwa_w2 = rowMeans(ds[paste0("rwa_0",seq_along(1:9),"_w2")],na.rm = T),
    rwa_w3 = rowMeans(ds[paste0("rwa_0",seq_along(1:9),"_w3")],na.rm = T),
    
    sdo_w1 = rowMeans(ds[paste0("sdo_0",seq_along(1:4),"_w1")],na.rm = T),
    sdo_w2 = rowMeans(ds[paste0("sdo_0",seq_along(1:4),"_w2")],na.rm = T),
    sdo_w3 = rowMeans(ds[paste0("sdo_0",seq_along(1:4),"_w3")],na.rm = T)
  )

ds %>% 
  select(
    case,
    starts_with("rwa_w"),
    starts_with("sdo_w"),
    starts_with("prj_")
  )

ds <-
  ds %>% 
  mutate(
    across(starts_with("prj_"),~102-.)
  )

psych::describe(rowMeans(ds[c("rwa_w1","rwa_w2","rwa_w3")]))
psych::describe(rowMeans(ds[c("sdo_w1","sdo_w2","sdo_w3")]))


ds_long <-
  ds %>% 
  select(
    case,
    starts_with("rwa_w")
  ) %>% 
  mutate(
    across(starts_with("rwa_"),~as.numeric(scale(.)))
  ) %>%
  pivot_longer(
    cols = starts_with("rwa"),names_to = "wave",values_to = "rwa"
  ) %>% 
  mutate(wave = str_remove_all(wave,"rwa_"))

ds_long <-
  full_join(
    ds_long,
    ds %>% 
      select(
        case,
        starts_with("sdo_w")
      ) %>% 
      mutate(
        across(starts_with("sdo_"),~as.numeric(scale(.)))
      ) %>%
      pivot_longer(
        cols = starts_with("sdo"),names_to = "wave",values_to = "sdo"
      ) %>% 
      mutate(wave = str_remove_all(wave,"sdo_")),
    by = c("case","wave")
    )

ds_long <-
  full_join(
    ds_long,
    ds %>% 
      select(
        case,
        starts_with("polid")
      ) %>% 
      mutate(
        polid = as.numeric(scale(polid))
      ) %>% 
      pivot_longer(
        cols = starts_with("polid"),names_to = "wave",values_to = "polid"
      ) %>% 
      select(-wave),
    by = "case"
  )

ds_long <-
  full_join(
    ds_long,
    ds %>% 
      select(
        case,
        starts_with("prj_")
      ) %>% 
      pivot_longer(
        cols = starts_with("prj_"),names_to = "target",values_to = "prejudice"
      ) %>% 
      mutate(wave = str_split_i(target, "_", -1),
             target = str_remove_all(target,"_w.*")),
    by = c("case","wave")
  )

library(lme4)

m1 <- lmer(prejudice ~ (1|case) + (1|target) + (1|target:case),       data = ds_long)
m2 <- lmer(prejudice ~ (1|case) + (1|target) + (1|target:case) + rwa, data = ds_long)
m3 <- lmer(prejudice ~ (1|case) + (1|target) + (1|target:case) + sdo, data = ds_long)
m4 <- lmer(prejudice ~ (1|case) + (1|target) + (1|target:case) + polid, data = ds_long)

summary(m1)
summary(m2)
summary(m3)
summary(m4)


bootstrap_icc(m1,iterations = 100, type = "parametric")
bootstrap_icc(m2,iterations = 100, type = "parametric")
bootstrap_icc(m3,iterations = 100, type = "parametric")
bootstrap_icc(m4,iterations = 100, type = "parametric")

performance::r2(m1)
performance::r2(m2)
performance::r2(m3)
performance::r2(m4)



str_remove(prj_all.grps.1a,c("prj.con_|prj.lib_"))
str_remove(prj_all.grps.1b,c("prj.con_|prj.lib_"))
prj_all.grps.3 <-
  ds %>%
  select(starts_with("prj_")) %>%
  names() %>%
  str_remove_all(.,c("prj_|_w3|_w2|_w1")) %>%
  unique()

table(prj_all.grps.3 %in% str_remove(prj_all.grps.1a,c("prj.con_|prj.lib_")))
table(prj_all.grps.3 %in% str_remove(prj_all.grps.1b,c("prj.con_|prj.lib_")))

prj_all.grps.3[!(prj_all.grps.3 %in% str_remove(prj_all.grps.1a,c("prj.con_|prj.lib_")))]
