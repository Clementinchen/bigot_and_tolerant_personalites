library(dplyr)
library(ggplot2)
library(ggtext)
library(ggpubr)

# PLOT 1A & 1B ----

## Study 1a ----

### 1AA ----

plot_1aa <- 
  ds1 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1a)) %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms.1a),
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(rwa_scl  = as.numeric(scale(rwa)),
         sdo_scl  = as.numeric(scale(sdo)),
         trgt_fct = case_when(target %in% ta_con.itms.1a ~ "Conservative",
                              target %in% ta_lib.itms.1a ~ "Liberal")
         ,rating = as.numeric(scale(rating))
         )  %>%
  tidyr::pivot_longer(cols = c(rwa,rwa_scl,sdo,sdo_scl), 
                      names_to = "scale",
                      values_to = "score") %>%
  filter(scale == "rwa_scl"|scale == "sdo_scl") %>%
  mutate(scale = forcats::fct_recode(scale, "RWA" = "rwa_scl","SDO"="sdo_scl")) %>%
  ggplot(aes(y = rating, x = score, color = trgt_fct, linetype = scale)) +
  geom_smooth(se = FALSE, method = lm, size = 0.9) + 
  ggtitle("Study 1a") +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  annotate(geom = "richtext", x = 0, y = -0.4, 
           label = "<i>r<sub>RWA</sub></i> = .52",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#990000") +
  annotate(geom = "richtext", x = 0, y = -0.5, 
           label = "<i>r<sub>SDO</sub></i> = .59",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#990000") +
  annotate(geom = "richtext", x = 0, y = .5, 
           label = "<i>r<sub>RWA</sub></i> = -.22",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#336699") +
  annotate(geom = "richtext", x = 0, y = 0.4, 
           label = "<i>r<sub>SDO</sub></i> = -.21",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#336699")
  


 ### 1AB ----

plot_dat.1a.gmc <- 
  ds1 %>%
  mutate(across(all_of(trgt.itms.1a), 
                ~ . - mean(.,na.rm = T), 
                .names = "{.col}_gmc")) %>%
  select(case, rwa, sdo, ends_with("_gmc")) %>%
  tidyr::pivot_longer(cols = ends_with("_gmc"), names_to = "target", values_to = "rating") %>%
  tidyr::pivot_longer(cols = 2:3, names_to = "scale", values_to = "value") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(value > median(value, na.rm = TRUE) ~ "High",
                          TRUE ~ "Low")) %>%
  ungroup() %>%
  group_by(target, scale, hilo) %>%
  summarise(m = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(target) %>%
  mutate(trgt_mean = mean(m)) %>%
  ungroup() %>%
  mutate(label = stringr::str_remove_all(target, "prj_"),
         label = stringr::str_to_title(stringr::str_replace_all(label, "_", " ")),
         scale = stringr::str_to_upper(scale),
         highlight = case_when(
           trgt_mean == max(trgt_mean) ~ "Max",
           trgt_mean == min(trgt_mean) ~ "Min",
           trgt_mean == quantile(trgt_mean, p = 0.25, type = 1) ~ "0.25",
           trgt_mean == quantile(trgt_mean, p = 0.75, type = 4) ~ "0.75",
           TRUE ~ "Indiff"),
         highlight = forcats::fct_relevel(highlight,"Max","0.75","0.25","Min"),
         hilo = forcats::fct_relevel(hilo,"Low","High"),
         target = stringr::str_remove_all(target,"_gmc"),
         trgt_fct = case_when(target %in% ta_lib.itms.1a ~ "Liberal",
                              target %in% ta_con.itms.1a ~ "Conservative"))


plot_1ab <- 
  plot_dat.1a.gmc %>%
  ggplot(aes(x = hilo, y = m, 
             group = interaction(target, scale,trgt_fct), 
             color = trgt_fct,
             linetype = scale)) +
  geom_point(size = 1.5) +
  geom_line(size = .9) +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  theme_minimal() +
  scale_x_discrete(expand = c(0.03, 0.03)) +
  geom_segment(aes(x = "Low", xend = "High", 
                   y = max(m) + 0.3, yend = max(m) + 0.3), 
               color = "black", size = 1) +  
  annotate(geom = "richtext", x = 1.5, y = max(plot_dat.1a.gmc$m)+.5, 
           label = "<i>r<sub>RWA</sub></i> = -.99, <i>r<sub>SDO</sub></i> = -.99", 
           size = 5, 
           fill = NA, 
           label.color = NA) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme(legend.position = "bottom")


### 1AC ----

plot_dat.1a <- 
  ds1 %>%
  select(case, rwa, sdo, starts_with("prj_")) %>%
  tidyr::pivot_longer(cols = starts_with("prj_"), names_to = "target", values_to = "rating") %>%
  tidyr::pivot_longer(cols = 2:3, names_to = "scale", values_to = "value") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(value > median(value, na.rm = TRUE) ~ "High",
                          TRUE ~ "Low")) %>%
  ungroup() %>%
  group_by(target, scale, hilo) %>%
  summarise(m = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(target) %>%
  mutate(trgt_mean = mean(m)) %>%
  ungroup() %>%
  mutate(label = stringr::str_remove_all(target, "prj_"),
         label = stringr::str_to_title(stringr::str_replace_all(label, "_", " ")),
         scale = stringr::str_to_upper(scale),
         highlight = case_when(
           trgt_mean == max(trgt_mean) ~ "Max",
           trgt_mean == min(trgt_mean) ~ "Min",
           trgt_mean == quantile(trgt_mean, p = 0.25, type = 1) ~ "0.25",
           trgt_mean == quantile(trgt_mean, p = 0.75, type = 4) ~ "0.75",
           TRUE ~ "Indiff"),
         highlight = forcats::fct_relevel(highlight,"Max","0.75","0.25","Min"),
         hilo = forcats::fct_relevel(hilo,"Low","High"),
         trgt_fct = case_when(target %in% ta_lib.itms.1a ~ "Liberal",
                              target %in% ta_con.itms.1a ~ "Conservative"))



plot_1ac <- 
  plot_dat.1a %>%
  ggplot(aes(x = hilo, y = m, 
             group = interaction(target, scale,trgt_fct), 
             color = trgt_fct,
             linetype = scale)) +
  geom_point(size = 1.5) +
  geom_line(size = .9) +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  scale_x_discrete(expand = c(0.03, 0.03)) +
  theme_minimal() +
  geom_segment(aes(x = "Low", xend = "High", 
                   y = max(m) + 0.5, yend = max(m) + 0.5), 
               color = "black", size = 1) +  
  annotate(geom = "richtext", x = 1.5, y = max(plot_dat.1a$m)+1, 
           label = "<i>r<sub>RWA</sub></i> = .80, <i>r<sub>SDO</sub></i> = .80", 
           size = 5, 
           fill = NA, 
           label.color = NA) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme(legend.position = "bottom")





ggpubr::ggarrange(plot_1aa,plot_1ab,plot_1ac,common.legend = T, 
                  ncol = 1, 
                  align = "v",
                  labels = c("A","B","C"))

ggsave(file = "./4_plots/test.jpeg",width = 5,height = 12)


## Study 1b ----

### 1BA ----

plot_1ba <- 
  ds2 %>%
  select(case,rwa,sdo,all_of(trgt.itms.1b)) %>%
  tidyr::pivot_longer(cols = all_of(trgt.itms.1b),
                      names_to  = "target",
                      values_to = "rating") %>%
  mutate(rwa_scl  = as.numeric(scale(rwa)),
         sdo_scl  = as.numeric(scale(sdo)),
         trgt_fct = case_when(target %in% ta_con.itms.1b ~ "Conservative",
                              target %in% ta_lib.itms.1b ~ "Liberal")
         ,rating = as.numeric(scale(rating))
  )  %>%
  tidyr::pivot_longer(cols = c(rwa,rwa_scl,sdo,sdo_scl), 
                      names_to = "scale",
                      values_to = "score") %>%
  filter(scale == "rwa_scl"|scale == "sdo_scl") %>%
  mutate(scale = forcats::fct_recode(scale, "RWA" = "rwa_scl","SDO"="sdo_scl")) %>%
  ggplot(aes(y = rating, x = score, color = trgt_fct, linetype = scale)) +
  geom_smooth(se = FALSE, method = lm, size = 0.9) + 
  ggtitle("Study 1b") +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  annotate(geom = "richtext", x = 0, y = -0.4, 
           label = "<i>r<sub>RWA</sub></i> = .59",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#990000") +
  annotate(geom = "richtext", x = 0, y = -0.5, 
           label = "<i>r<sub>SDO</sub></i> = .56",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#990000") +
  annotate(geom = "richtext", x = 0, y = .5, 
           label = "<i>r<sub>RWA</sub></i> = -.31",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#336699") +
  annotate(geom = "richtext", x = 0, y = 0.4, 
           label = "<i>r<sub>SDO</sub></i> = -.25",
           hjust = 0,
           size = 4, 
           fill = NA, 
           label.color = NA,
           colour = "#336699")


### 1BB ----

plot_dat.1b.gmc <- 
  ds2 %>%
  mutate(across(all_of(trgt.itms.1b), 
                ~ . - mean(.,na.rm = T), 
                .names = "{.col}_gmc")) %>%
  select(case, rwa, sdo, ends_with("_gmc")) %>%
  tidyr::pivot_longer(cols = ends_with("_gmc"), names_to = "target", values_to = "rating") %>%
  tidyr::pivot_longer(cols = 2:3, names_to = "scale", values_to = "value") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(value > median(value, na.rm = TRUE) ~ "High",
                          TRUE ~ "Low")) %>%
  ungroup() %>%
  group_by(target, scale, hilo) %>%
  summarise(m = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(target) %>%
  mutate(trgt_mean = mean(m)) %>%
  ungroup() %>%
  mutate(label = stringr::str_remove_all(target, "prj_"),
         label = stringr::str_to_title(stringr::str_replace_all(label, "_", " ")),
         scale = stringr::str_to_upper(scale),
         highlight = case_when(
           trgt_mean == max(trgt_mean) ~ "Max",
           trgt_mean == min(trgt_mean) ~ "Min",
           trgt_mean == quantile(trgt_mean, p = 0.25, type = 1) ~ "0.25",
           trgt_mean == quantile(trgt_mean, p = 0.75, type = 4) ~ "0.75",
           TRUE ~ "Indiff"),
         highlight = forcats::fct_relevel(highlight,"Max","0.75","0.25","Min"),
         hilo = forcats::fct_relevel(hilo,"Low","High"),
         target = stringr::str_remove_all(target,"_gmc"),
         trgt_fct = case_when(target %in% ta_lib.itms.1b ~ "Liberal",
                              target %in% ta_con.itms.1b ~ "Conservative"))


plot_1bb <- 
  plot_dat.1b.gmc %>%
  ggplot(aes(x = hilo, y = m, 
             group = interaction(target, scale,trgt_fct), 
             color = trgt_fct,
             linetype = scale)) +
  geom_point(size = 1.5) +
  geom_line(size = .9) +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  theme_minimal() +
  scale_x_discrete(expand = c(0.03, 0.03)) +
  geom_segment(aes(x = "Low", xend = "High", 
                   y = max(m) + 1, yend = max(m) + 1), 
               color = "black", size = 1) +  
  annotate(geom = "richtext", x = 1.5, y = max(plot_dat.1b.gmc$m)+2.6, 
           label = "<i>r<sub>RWA</sub></i> = -.99, <i>r<sub>SDO</sub></i> = -.99", 
           size = 5, 
           fill = NA, 
           label.color = NA) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme(legend.position = "bottom")


### 1BC ----

plot_dat.1b <- 
  ds2 %>%
  select(case, rwa, sdo, starts_with("prj_")) %>%
  tidyr::pivot_longer(cols = starts_with("prj_"), names_to = "target", values_to = "rating") %>%
  tidyr::pivot_longer(cols = 2:3, names_to = "scale", values_to = "value") %>%
  group_by(scale) %>%
  mutate(hilo = case_when(value > median(value, na.rm = TRUE) ~ "High",
                          TRUE ~ "Low")) %>%
  ungroup() %>%
  group_by(target, scale, hilo) %>%
  summarise(m = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(target) %>%
  mutate(trgt_mean = mean(m)) %>%
  ungroup() %>%
  mutate(label = stringr::str_remove_all(target, "prj_"),
         label = stringr::str_to_title(stringr::str_replace_all(label, "_", " ")),
         scale = stringr::str_to_upper(scale),
         highlight = case_when(
           trgt_mean == max(trgt_mean) ~ "Max",
           trgt_mean == min(trgt_mean) ~ "Min",
           trgt_mean == quantile(trgt_mean, p = 0.25, type = 1) ~ "0.25",
           trgt_mean == quantile(trgt_mean, p = 0.75, type = 4) ~ "0.75",
           TRUE ~ "Indiff"),
         highlight = forcats::fct_relevel(highlight,"Max","0.75","0.25","Min"),
         hilo = forcats::fct_relevel(hilo,"Low","High"),
         trgt_fct = case_when(target %in% ta_lib.itms.1b ~ "Liberal",
                              target %in% ta_con.itms.1b ~ "Conservative"))



plot_1bc <- 
  plot_dat.1b %>%
  ggplot(aes(x = hilo, y = m, 
             group = interaction(target, scale,trgt_fct), 
             color = trgt_fct,
             linetype = scale)) +
  geom_point(size = 1.5) +
  geom_line(size = .9) +
  scale_color_manual(values = c(
    "Liberal" = "#336699",  
    "Conservative" = "#990000")) +
  scale_x_discrete(expand = c(0.03, 0.03)) +
  theme_minimal() +
  geom_segment(aes(x = "Low", xend = "High", 
                   y = max(m) + 3, yend = max(m) + 3), 
               color = "black", size = 1) +  
  annotate(geom = "richtext", x = 1.5, y = max(plot_dat.1b$m)+6.5, 
           label = "<i>r<sub>RWA</sub></i> = .58, <i>r<sub>SDO</sub></i> = .64", 
           size = 5, 
           fill = NA, 
           label.color = NA) +
  labs(y = "Prejudice", x = "Predictor", color = "Target",linetype = "Scale") +
  theme(legend.position = "bottom")


ggpubr::ggarrange(plot_1ba,plot_1bb,plot_1bc,common.legend = T, 
                  ncol = 1, 
                  align = "v",
                  labels = c("A","B","C"))

ggsave(file = "./4_plots/test2.jpeg",width = 5,height = 12)


## Common Plot ----

fig_1ab <- ggarrange(plot_1aa+rremove("xlab")+rremove("ylab"),plot_1ba+rremove("xlab")+rremove("ylab"),
                     #plot_1ab+rremove("xlab")+rremove("ylab"),plot_1bb+rremove("xlab")+rremove("ylab"),
                     plot_1ac+rremove("xlab")+rremove("ylab"),plot_1bc+rremove("xlab")+rremove("ylab"),
                     common.legend = T, 
                     ncol = 2,
                     nrow = 2,
                     align = "v",
                     labels = c(
                       "A","A"
                       ,"B","B"
                       #,"C","C"
                       ),
                     legend = "top"
)

fig_1ab <- annotate_figure(fig_1ab, left = text_grob("Prejudice", rot = 90),
                bottom = text_grob("Predictor"))

annotate_figure(fig_1ab, bottom = text_grob(
paste("A: Correlations of RWA and SDO (the \"mirror-image\" pattern) with averaged scores (i.e., between-target variance removed) of \n      liberal and conservative prejudice target groups",
      #"B: Correlation between high/low RWA/SDO subsample for centered prejudice targets (between target variance removed)",
      "B: Correlation between high/low RWA/SDO subsample for prejudice ratings (between target variance is considered)",
      sep = "\n"),
                                            face = "italic",hjust = 0, x = unit(5.5,"pt")))

ggsave(file = "./4_plots/studies_1a_1b.jpeg",width = 10, height = 10)


# PLOT STUDY 2 ----

ess4 %>%
  select(idno,cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl),
                      names_to = "target",
                      values_to = "rating") %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median,target) %>%
  summarise(m = mean(rating,na.rm = T)) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  #tidyr::pivot_wider(names_from = rwa_hilo_cntry_median, values_from = m) %>%
  filter(cntry_lbl == "Belgium"|cntry_lbl == "Netherlands"|cntry_lbl == "Estonia"|cntry_lbl == "Latvia") %>%
  mutate(cntry_lbl = forcats::fct_drop(cntry_lbl),
         cntry_lbl = forcats::fct_relevel(cntry_lbl,"Latvia","Belgium","Estonia","Netherlands"),
         target = forcats::fct_recode(target,
                                      "Gays and Lesbians" = "anti_gay_scl",
                                      "Immigrants" = "anti_immigrant_scl",
                                      "People in their 20s" = "ppl_20s_scl",
                                      "People over 70" = "ppl_70s_scl",
                                      "Women" = "sexist_scl",
                                      "Unemployed" = "unemployed_scl"),
         rwa_hilo_cntry_median = as.factor(stringr::str_to_title(rwa_hilo_cntry_median)),
         rwa_hilo_cntry_median = forcats::fct_relevel(rwa_hilo_cntry_median,"Low","High")) %>%
  ggplot(aes(x = rwa_hilo_cntry_median, y = m, group = target)) +
  geom_point(aes(x = rwa_hilo_cntry_median, y = m,color=target), size=2) +
  geom_line(aes(x = rwa_hilo_cntry_median, y = m,color=target), size=1) +
  ylab("Prejudice") + 
  xlab("Traditionalism")+
  labs(color = "Prejudice Target") +
  facet_wrap(~cntry_lbl, strip.position = "bottom") + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(-0.75,0.75,0.25)) +
  scale_color_manual(values = c("#AA4499","#6699CC","#CC6677","#332288","#44AA99","#882255"))+
  scale_x_discrete(expand = c(0.1, 0.1)) +
  theme(panel.grid.minor = element_blank(),
        panel.spacing.y = unit(3,"lines"),
        legend.position = "bottom",
        strip.text.x = element_text(size = 11, face = "bold")) +
  geom_segment(aes(x = "Low", xend = "High", 
                   y = 0.6 , yend = 0.6), 
               color = "black", size = 1) +
  theme(axis.title = element_text(size = 14))

ggsave(file = "./4_plots/study_2.jpeg",width = 10,height = 8, type = "Cairo")

ess4 %>%
  select(idno,cntry_lbl,rwa_hilo_cntry_median,all_of(target.itms_scl)) %>%
  tidyr::pivot_longer(cols = all_of(target.itms_scl),
                      names_to = "target",
                      values_to = "rating") %>%
  group_by(cntry_lbl,rwa_hilo_cntry_median,target) %>%
  summarise(m = mean(rating,na.rm = T)) %>%
  filter(!is.na(rwa_hilo_cntry_median)) %>%
  #tidyr::pivot_wider(names_from = rwa_hilo_cntry_median, values_from = m) %>%
  filter(cntry_lbl == "Belgium"|cntry_lbl == "Netherlands"|cntry_lbl == "Estonia"|cntry_lbl == "Latvia") %>%
  tidyr::pivot_wider(names_from = cntry_lbl,values_from = m) %>%
  corrr::correlate()

# PLOT STUDY 3 ----


p1 <- sjPlot::plot_model(mlm_anti_mig_5_cntrls,
                   type = "std",
                    axis.labels = c(
                      "RWA", 
                      "Individual Prejudice",
                      "Contextual Prejudice", 
                      "Political Self-Placement",
                      "Age",
                      "Education",
                      "Gender [Female]")
                   )


df_plot <- 
sjPlot::plot_model(mlm_anti_mig_5_cntrls,
                   type = "std",
                   show.data = T)$data %>%
  mutate(Model = "Anti-Immigrant",
         term = case_when(term == "cwc_rwa" ~ "Traditionalism",
                          term == "cwc_anti_gay" ~ "Individual Prejudice",
                          term == "rgn_lvl_gay_att.gmc" ~ "Contextual Prejudice",
                          term == "lrscale" ~ "Political Self-Placement",
                          term == "agea" ~ "Age",
                          term == "eisced" ~ "Education",
                          term == "gndrFemale" ~ "Gender [Female]")) %>%
  bind_rows(
    sjPlot::plot_model(mlm_anti_gay_5_cntrls,
                       type = "std",
                       show.data = T)$data %>%
      mutate(Model = "Anti-Gay",
             term = case_when(term == "cwc_rwa" ~ "Traditionalism",
                              term == "cwc_anti_mig" ~ "Individual Prejudice",
                              term == "rgn_lvl_anti_mig.gmc" ~ "Contextual Prejudice",
                              term == "lrscale" ~ "Political Self-Placement",
                              term == "agea" ~ "Age",
                              term == "eisced" ~ "Education",
                              term == "gndrFemale" ~ "Gender [Female]"))
  )

df_plot <- 
  df_plot %>%
  mutate(term = forcats::fct_relevel(term,"Gender [Female]",
                                     "Education",
                                     "Age",
                                     "Political Self-Placement",
                                     "Contextual Prejudice",
                                     "Individual Prejudice",
                                     "Traditionalism"),
         term = forcats::fct_recode(term,
                                     "Traditionalism" = "Traditionalism"))


ggplot(df_plot, aes(x = estimate, y = term, color = Model)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +  # Separate dots
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.3), height = 0.2) +  # Error bars
  theme_minimal() +
  labs(x = "Standardized Coefficient", y = "Predictor Variables") +
  scale_color_manual(values = c("Anti-Immigrant" = "#336699", "Anti-Gay" = "#990000")) +  # Custom colors
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(size = 14))


ggsave(file = paste0("./4_plots/",Sys.Date(),"study_3.jpeg")
       ,width = 10,height = 8, type = "Cairo")
