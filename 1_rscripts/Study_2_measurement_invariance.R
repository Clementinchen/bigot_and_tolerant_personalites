# MEASUREMENT INVARIANCE ----

cfi_dat <- 
  ess4 %>%
  select(
    #Grouping Variable: Country
    cntry_lbl,
    #Traditionalism
    trad_1,trad_2,trad_3,trad_4,trad_5,
    #Prejudice: Anti-Gay
    prj_gay,
    #Prejudice: Immigrants
    prj_immi_1,prj_immi_2,prj_immi_3,
    #Prejudice: Unemployed
    prj_unemployed,
    #Prejudice: Women
    prj_wmn_1,prj_wmn_2,
    #Prejudice: Age
    prj_age20,prj_age70
  ) %>%
  mutate(
    across(where(is.numeric), ~as.numeric(scale(.)),.names = "{.col}_scl")
  )

#* Traditionalism ----

trd_inv <- '
trd_cfa =~ trad_1_scl + trad_2_scl + trad_3_scl + trad_4_scl + trad_5_scl
'

trd.config <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM')
trd.metric <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal ="loadings")
trd.scalar <- cfa(trd_inv, data = cfi_dat, group = "cntry_lbl", estimator = 'MLM', 
                  group.equal = c("loadings", "intercepts"))


fitMeasures(trd.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))
fitMeasures(trd.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))
fitMeasures(trd.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))

fitmeasures(trd.config,"cfi") - fitmeasures(trd.metric,"cfi")
fitmeasures(trd.config,"srmr") - fitmeasures(trd.metric,"srmr")
fitmeasures(trd.config,"rmsea") - fitmeasures(trd.metric,"rmsea")


#* prejudice targets ----

# Define model
trgt_inv <- '

prj_gay        =~ prj_gay_scl
prj_immigrants =~ prj_immi_1_scl + prj_immi_2_scl + prj_immi_3_scl
prj_unemployed =~ prj_unemployed_scl
prj_women      =~ prj_wmn_1_scl + prj_wmn_2_scl
prj_age20      =~ prj_age20_scl
prj_age70      =~ prj_age70_scl

'

trgt.config <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR")
trgt.metric <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR",
                   group.equal = "loadings")
trgt.scalar <- cfa(trgt_inv, data = cfi_dat, group = "cntry_lbl", estimator = "MLR",
                   group.equal = c("loadings","Intercepts"))

#extract cfi
cfi_configural <- fitMeasures(trgt.config, "cfi")
cfi_metric     <- fitMeasures(trgt.metric, "cfi")
cfi_scalar     <- fitMeasures(trgt.scalar, "cfi")

#cfi change
cfi_configural - cfi_metric
cfi_configural - cfi_scalar

#fit measures
fitMeasures(trgt.config, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))
fitMeasures(trgt.metric, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))
fitMeasures(trgt.scalar, c("chisq", "df", "pvalue", "cfi","tli", "rmsea", "srmr"))


