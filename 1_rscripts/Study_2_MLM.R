long <- 
  ess4 %>%
  select(idno,rwa,cntry_lbl,region,all_of(target.itms_scl)) %>%
  tidyr::pivot_longer(cols = anti_gay_scl:ncol(.),
                      names_to = "target",
                      values_to = "rating")

mlm0 <- lme4::lmer(rating ~ rwa + (1|idno), data = long, REML = FALSE)
summary(mlm0)

r2mlm::r2mlm(mlm0)

mlm1 <- lme4::lmer(rating ~ rwa + (target|region), data = long, REML = FALSE)
summary(mlm1)  

m1 <- lme4::lmer(rating ~ (1|target),       data = long, REML = F)
m2 <- lme4::lmer(rating ~ (1|cntry:target), data = long, REML = F)

summary(m1)  
