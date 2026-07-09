remove(list = ls())
library(tidyverse)

d <- haven::read_sav('./0_data/WVS_Cross-National_Wave_7_spss_v6_0.sav')
cb <- openxlsx::read.xlsx('./0_data/codebook_variables_relevant_excel.xlsx', sheet = 2)

as_tibble(d)
as_tibble(cb)

d <- d |> 
  janitor::clean_names()

cb <- cb |> 
  mutate(
    variable = janitor::make_clean_names(variable)
  )

d <- 
  d |>
  select(
    cb |>
  filter(var_select == "select") |> 
  pull(variable)
  )


names(d)

rename_vec <- 
  setNames(
    cb |> filter(var_select == "select") |> pull(variable),
    cb |> filter(var_select == "select") |> pull(var_label)
)

d <- 
  d |> 
  rename(all_of(rename_vec))

names(d)


## Finding Factors ----

## work
wrk.itms <- 
  d |> 
  select(starts_with("wrk_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(wrk.itms)))

fa_wrk <- 
  psych::fa(d %>% select(all_of(wrk.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")
fa_wrk

psych::alpha(d[wrk.itms])

## gender roles
gndr_role.itms <-
  d |> 
  select(starts_with("gndr_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(gndr_role.itms)))


fa_gndrrle <- 
  psych::fa(d %>% select(all_of(gndr_role.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")
fa_gndrrle

psych::alpha(d[gndr_role.itms])

## immigration ----

immgrtn.itms <-
  d |> 
  select(starts_with("immgrtn_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(immgrtn.itms)))


fa_immgrtn <- 
  psych::fa(d %>% select(all_of(immgrtn.itms)), 
            nfactors = 3, 
            rotate = "oblimin", 
            fm = "ml")
fa_immgrtn

immgrtn.itms <- 
  loadings(fa_immgrtn)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.4 ~ NA, TRUE ~ .))
  ) |> 
  pivot_longer(cols = c(2:4), names_to = "factor", values_to = "loading") |> 
  filter(!is.na(loading)) |> 
  group_split(factor) |> 
  purrr::map(~ pull(., item))

names(immgrtn.itms)[1] <- "immgrtn_crime"
names(immgrtn.itms)[2] <- "immgrtn_cnflt"
names(immgrtn.itms)[3] <- "immgrtn_enrch"

psych::alpha(d[immgrtn.itms$immgrtn_cnflt])
psych::alpha(d[immgrtn.itms$immgrtn_crime])
psych::alpha(d[immgrtn.itms$immgrtn_enrch])

## Trust ----

trst.itms <-
  d |> 
  select(starts_with("trst_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(trst.itms)))

fa_trst <- 
  psych::fa(d %>% select(all_of(trst.itms)), 
            nfactors = 1, 
            rotate = "oblimin", 
            fm = "ml")
fa_trst

trst.itms <- 
  loadings(fa_trst)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.3 ~ NA, TRUE ~ .))
  ) |> 
  filter(!is.na(ML1)) |> 
  pull(item)


psych::alpha(d[trst.itms])

## science ----
d$science_vs_faith    <- 11 - d$science_vs_faith
d$science_right_wrong <- 11 - d$science_right_wrong
d$science_imprtant    <- 11 - d$science_imprtant

scnce.itms <-
  d |> 
  select(starts_with("science_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(scnce.itms)))

fa_scnce <- 
  psych::fa(d %>% select(all_of(scnce.itms)), 
            nfactors = 3, 
            rotate = "oblimin", 
            fm = "ml")
fa_scnce


scnce.itms <- 
  loadings(fa_scnce)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.4 ~ NA, TRUE ~ .))
  ) |> 
  pivot_longer(cols = c(2:4), names_to = "factor", values_to = "loading") |> 
  filter(!is.na(loading)) |> 
  group_split(factor) |> 
  purrr::map(~ pull(., item))

names(scnce.itms)[1] <- "scnce_con"
names(scnce.itms)[2] <- "scnce_pro"


## government ----
gvrnmt.itms <-
  d |> 
  select(starts_with("gvrnmt_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(gvrnmt.itms)))

fa_gvrnmt <- 
  psych::fa(d %>% select(all_of(gvrnmt.itms)), 
            nfactors = 3, 
            rotate = "oblimin", 
            fm = "ml")
fa_gvrnmt

gvrnmt.itms <- 
  loadings(fa_gvrnmt)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.3 ~ NA, TRUE ~ .))
  ) |> 
  pivot_longer(cols = c(2:4), names_to = "factor", values_to = "loading") |> 
  filter(!is.na(loading)) |> 
  group_split(factor) |> 
  purrr::map(~ pull(., item))

names(gvrnmt.itms)[1] <- "gvrnmt_poor"
names(gvrnmt.itms)[2] <- "gvrnmt_good"
names(gvrnmt.itms)[3] <- "gvrnmt_auth"

psych::alpha(d[gvrnmt.itms[[1]]])[1]
psych::alpha(d[gvrnmt.itms[[2]]])[1]
psych::alpha(d[gvrnmt.itms[[3]]])[1]

## politicians ----

pltcns.itms <-
  d |> 
  select(starts_with("pltcns_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(pltcns.itms)))

fa_pltcns <- 
  psych::fa(d %>% select(all_of(pltcns.itms)), 
            nfactors = 2, 
            rotate = "oblimin", 
            fm = "ml")
fa_pltcns

pltcns.itms <- 
  loadings(fa_pltcns)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.3 ~ NA, TRUE ~ .))
  ) |> 
  pivot_longer(cols = c(2:3), names_to = "factor", values_to = "loading") |> 
  filter(!is.na(loading)) |> 
  group_split(factor) |> 
  purrr::map(~ pull(., item))

names(pltcns.itms)[1] <- "pltcns_bad"
names(pltcns.itms)[2] <- "pltcns_trst"

psych::alpha(d[pltcns.itms[[1]]])[1]
psych::alpha(d[pltcns.itms[[2]]])[1]

## democracy ----

dmcry.itms <-
  d |> 
  select(starts_with("democracy_")) |> 
  names()

psych::fa.parallel(d %>% select(all_of(dmcry.itms)))

fa_dmcry <- 
  psych::fa(d %>% select(all_of(dmcry.itms)), 
            nfactors = 3, 
            rotate = "oblimin", 
            fm = "ml")
fa_dmcry

dmcry.itms <- 
  loadings(fa_dmcry)[] |> 
  data.frame() |> 
  rownames_to_column("item") |> 
  as_tibble() |> 
  mutate(
    across(where(is.numeric),~case_when(.<.3 ~ NA, TRUE ~ .))
  ) |> 
  pivot_longer(cols = c(2:4), names_to = "factor", values_to = "loading") |> 
  filter(!is.na(loading)) |> 
  group_split(factor) |> 
  purrr::map(~ pull(., item))

names(dmcry.itms)[1] <- "dmcry_aut"
names(dmcry.itms)[2] <- "dmcry_lib"

psych::alpha(d[dmcry.itms[[1]]])[1]
psych::alpha(d[dmcry.itms[[2]]])[1]

# Clean up things ----

att.itms <- 
  cb |> 
  filter(attitude_item == "attitude") |> 
  pull(var_label) 

att_range <- matrix(nrow = length(att.itms), ncol = 3)
for (i in att.itms) {
  att_range[which(att.itms %in% i),1] <- i
  att_range[which(att.itms %in% i),2] <- min(d[i],na.rm = T)
  att_range[which(att.itms %in% i),3] <- max(d[i],na.rm = T)

}
att_range <- att_range |> 
  as_tibble() %>%
  rename("item" = "V1","min" = "V2", "max" = "V3") |> 
  mutate(
    across(c("min","max"), as.numeric),
    range = max-min
  ) |> filter(range > 1)


lin_stretch <- function(x, new_min, new_max) {
  
  old_min <- min(x, na.rm = TRUE)
  old_max <- max(x, na.rm = TRUE)
  
  if (old_max == old_min) {
    stop("Lineares Strecken nicht möglich: alle Werte sind identisch.")
  }
  
  stretched <- new_min +
    ((x - old_min) * (new_max - new_min)) /
    (old_max - old_min)
  
  return(stretched)
}

d <- 
  d |>
  mutate(
    across(
      all_of(att_range$item),
      ~ lin_stretch(.x, new_min = 0, new_max = 1)
    )
  )

ls(pattern = ".itms")

psych::alpha(d[dmcry.itms[[1]]])$total
psych::alpha(d[dmcry.itms[[2]]])$total

psych::alpha(d[gvrnmt.itms[[1]]])$total
psych::alpha(d[gvrnmt.itms[[2]]])$total
psych::alpha(d[gvrnmt.itms[[3]]])$total

psych::alpha(d[gndr_role.itms])$total

psych::alpha(d[immgrtn.itms[[1]]])$total
psych::alpha(d[immgrtn.itms[[2]]])$total
psych::alpha(d[immgrtn.itms[[3]]])$total

psych::alpha(d[pltcns.itms[[1]]])$total
psych::alpha(d[pltcns.itms[[2]]])$total

psych::alpha(d[scnce.itms[[1]]])$total
psych::alpha(d[scnce.itms[[2]]])$total

psych::alpha(d[trst.itms])$total

psych::alpha(d[wrk.itms])$total



d <- d |> 
  mutate(
    dmcry_lib = rowMeans(pick(all_of(dmcry.itms$dmcry_lib)),, na.rm = T),
    dmcry_aut = rowMeans(pick(all_of(dmcry.itms$dmcry_aut)),, na.rm = T),

    gvrnmt_poor = rowMeans(pick(all_of(gvrnmt.itms$gvrnmt_poor)),, na.rm = T),
    gvrnmt_auth = rowMeans(pick(all_of(gvrnmt.itms$gvrnmt_auth)),, na.rm = T),
    gvrnmt_good = rowMeans(pick(all_of(gvrnmt.itms$gvrnmt_good)),, na.rm = T),

    gndr_role  = rowMeans(pick(all_of(gndr_role.itms)),, na.rm = T),

    immgrtn_enrch = rowMeans(pick(all_of(immgrtn.itms$immgrtn_enrch)),, na.rm = T),
    immgrtn_crime = rowMeans(pick(all_of(immgrtn.itms$immgrtn_crime)),, na.rm = T),

    pltcns_bad = rowMeans(pick(all_of(pltcns.itms$pltcns_bad)),, na.rm = T),
    
    scnce_pro = rowMeans(pick(all_of(scnce.itms$scnce_pro)),, na.rm = T),

    trst = rowMeans(pick(all_of(trst.itms)),, na.rm = T),

    wrk = rowMeans(pick(all_of(wrk.itms)),, na.rm = T)

  )

att.scales <-  c(
  "dmcry_lib","dmcry_aut","gvrnmt_poor","gvrnmt_auth","gvrnmt_good","gndr_role",
  "immgrtn_enrch","immgrtn_crime","pltcns_bad","scnce_pro","trst","wrk"
)

att.itms %in% 
  c(dmcry.itms$dmcry_lib,dmcry.itms$dmcry_aut,
  gvrnmt.itms$gvrnmt_poor,gvrnmt.itms$gvrnmt_auth,gvrnmt.itms$gvrnmt_good,
  gndr_role.itms,
  immgrtn.itms$immgrtn_enrch,immgrtn.itms$immgrtn_crime,
  pltcns.itms$pltcns_bad,
  scnce.itms$scnce_pro,
  trst.itms,
  wrk.itms
)

att.itms.use <- setdiff(
  att_range$item,
    c(dmcry.itms$dmcry_lib,dmcry.itms$dmcry_aut,
  gvrnmt.itms$gvrnmt_poor,gvrnmt.itms$gvrnmt_auth,gvrnmt.itms$gvrnmt_good,
  gndr_role.itms,
  immgrtn.itms$immgrtn_enrch,immgrtn.itms$immgrtn_crime,
  pltcns.itms$pltcns_bad,
  scnce.itms$scnce_pro,
  trst.itms,
  wrk.itms
)
)


d <- d |> 
  mutate(
    lr_split = case_when(
      left_right_scale >= 4 ~ "left",
    left_right_scale <= 6 ~ "right")
  ) 

d$iso_3166_1_numeric_country_code <- haven::as_factor(d$iso_3166_1_numeric_country_code,"label")

d$interview_id
d |> 
  select(all_of(att.itms.use),all_of(att.scales)) |> 
  pivot_longer(
    cols = 1:last_col(),
    names_to = "att_item"
  ) |> 
  group_by(att_item) |> 
  summarise(
    mean = mean(value,na.rm = T)
  ) |> arrange(desc(mean)) |> print(n = Inf)



d |> 
  select(
    interview_id,
    lr_split,iso_3166_1_numeric_country_code,
    all_of(att.scales),all_of(att.itms.use)
  ) |> 
  pivot_longer(
    cols = c(all_of(att.scales),all_of(att.itms.use)),
    names_to = "att_item",values_to = "att_rating") |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  group_by(lr_split,iso_3166_1_numeric_country_code,att_item) |> 
  summarise(
    mean_att_rating = mean(att_rating, na.rm = T),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = lr_split,values_from = mean_att_rating) |>
  group_by(iso_3166_1_numeric_country_code) |> 
  summarise(
    cor = cor(left,right, use = "pairwise.complete.obs",method = "pearson")
  ) |> print(n = Inf)

d |> 
  select(
    interview_id,
    lr_split,iso_3166_1_numeric_country_code,
    all_of(att.scales),all_of(att.itms.use)
  ) |> 
  pivot_longer(
    cols = c(all_of(att.scales),all_of(att.itms.use)),
    names_to = "att_item",values_to = "att_rating") |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  group_by(lr_split,iso_3166_1_numeric_country_code,att_item) |> 
  summarise(
    mean_att_rating = mean(att_rating, na.rm = T),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = lr_split,
    values_from = mean_att_rating
  ) |>
  group_by(iso_3166_1_numeric_country_code) |>
  group_modify(~ correlation::correlation(.x)) |> 
  print(n = Inf)

d |> 
  select(
    interview_id,
    lr_split,iso_3166_1_numeric_country_code,
    all_of(att.scales),all_of(att.itms.use)
  ) |> 
  pivot_longer(
    cols = c(all_of(att.scales),all_of(att.itms.use)),
    names_to = "att_item",values_to = "att_rating") |> 
  filter(if_all(everything(), ~ !is.na(.))) |> 
  group_by(lr_split,iso_3166_1_numeric_country_code,att_item) |> 
  summarise(
    mean_att_rating = mean(att_rating, na.rm = T),
    .groups = "drop"
  ) |> 
  filter(lr_split == "left") |> 
  pivot_wider(
    names_from = "iso_3166_1_numeric_country_code",
    values_from = "mean_att_rating"
) |> 
  correlation::correlation() |> 
  as_tibble() |> 
  group_by(Parameter1) |> 
  summarise(meanr = mean(r,na.rm = T)) |> 
  print(n = Inf)


library(lme4)

d_long <- d |> 
  select(
    interview_id,
    left_right_scale,iso_3166_1_numeric_country_code,
    all_of(att.scales),all_of(att.itms.use)
  ) |> 
  pivot_longer(
    cols = c(all_of(att.scales),all_of(att.itms.use)),
    names_to = "att_item",values_to = "att_rating") |> 
  filter(if_all(everything(), ~ !is.na(.)))

m1 <- lmer(att_rating ~ 
  (1|interview_id) + 
  (1|left_right_scale) +
  (1|att_item:iso_3166_1_numeric_country_code),
data = d_long
)

summary(m1)

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

mysumm(m1)





(35+15)/4
35-12.5
15-12.5
