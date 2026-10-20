library(spdep)
library(spatialreg)

ggplot() + geom_sf(data = prj_reg_sf, aes(fill = prj_immigrants)) + theme_minimal()

moran.test(prj_reg_sf$prj_immigrants, listw = weight_matrix)
moran.test(prj_reg_sf$prj_gay, listw = weight_matrix)
moran.test(prj_reg_sf$prj_women, listw = weight_matrix)
moran.test(prj_reg_sf$prj_unemployed, listw = weight_matrix)
moran.test(prj_reg_sf$prj_age20, listw = weight_matrix)
moran.test(prj_reg_sf$prj_age70, listw = weight_matrix)

nb <- poly2nb(prj_reg_sf)
weight_matrix <- nb2listw(nb, style = "W",zero.policy = TRUE)
summary(weight_matrix)

length(nb)

sum(card(nb) == 0)

form <- formula(prj_rating_lin ~ trad_raw)


write.csv(head(ess4_long,10),file = "./chat.csv")


spatial_dat <- 
  ess4_long %>%
  filter(region %in% prj_reg_sf$region)

library(hglm)
hglm_test <- 
  hglm(
  fixed = prj_rating_lin ~ trad_raw,
  random = ~ 1 | region,
  data = spatial_dat,
  family = gaussian(),
  rand.family = CAR(D = W)
)








region_shape <- 
  prj_reg_sf %>%
  select(cntry_cde,region,geometry)

prj_targets <- c("prj_immigrants","prj_gay","prj_unemployed","prj_women","prj_age20","prj_age70")

ess4_long <- 
  ess4_ana %>% 
  select(idno,region,cntry_cde,trad_raw,lrscale,(starts_with("prj_") & ends_with("_lin"))) %>%
  group_by(cntry_cde) %>%
  mutate(
    trad_scl = as.numeric(scale(trad_raw)),
    lrscale_scl = as.numeric(scale(lrscale))
  ) %>%
  ungroup() %>%
  pivot_longer(cols = paste0(prj_targets,"_lin"),
               names_to = "prj_target",
               values_to = "prj_rating_lin")


ess4_long <- 
  ess4_long %>%
  left_join(region_shape, by = c("cntry_cde","region")) %>%
  filter(complete.cases(.))

nb <- poly2nb(ess4_long %>% st_as_sf())
weight_matrix <- nb2listw(nb, style = "W",zero.policy = TRUE)
summary(weight_matrix)


ess4_long %>%
  filter(is.na(geometry))

spatial_dat_de <- 
  ess4_long %>%
  filter(cntry_cde == "DE") %>%
  filter(prj_target == "prj_immigrants_lin") %>%
  filter(complete.cases(.))

spatial_dat_de %>%
  left_join(prj_reg_sf %>% select(cntry_cde,region,geometry))

nb <- poly2nb(de_test)
weight_matrix <- nb2listw(nb, style = "W",zero.policy = TRUE)
summary(weight_matrix)

hglm_car_m4 <- 
  hglm2(
  prj_rating_lin ~ lrscale_scl + (1 |region) + (1 | idno),
  data = spatial_dat_de,
  rand.family = list(CAR(D = weight_matrix), gaussian()),
  sparse = TRUE
)


spatial_lag <- lagsarlm(prj_rating_lin ~ lrscale_scl, data = spatial_dat_de, listw = weight_matrix)
