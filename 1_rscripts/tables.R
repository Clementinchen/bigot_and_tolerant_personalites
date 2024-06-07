# TABLE 1 ----

ds1 %>%
  select(rwa,sdo,con,mi,polid,ta_lft,ta_rght) %>%
  rstatix::cor_mat() %>%
  rstatix::cor_mark_significant( cutpoints = c(0, 0.001, 0.01, 0.05, 1),symbols = c("***", "**", "*", "")) %>%
  mutate(across(2:7, ~ sub("0.", ".", as.character(.)))) %>%
  select(-ta_rght) %>% stats::setNames(c("","1","2","3","4","5","6"))

ds1 %>%
  select(rwa,sdo,con,mi,polid,ta_rght,ta_lft) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), names_to = "scale",values_to = "score") %>%
  group_by(scale) %>%
  summarise(M =  mean(score,na.rm = T),
            SD = sd(score,na.rm = T)) %>%
  mutate(across(c(M, SD), ~ round(.x, 2))) %>% slice(4,5,1,2,3,6,7)

ds2 %>%
  select(rwa,sdo,con,mi,hueg,polid,ta_lft,ta_rght) %>%
  rstatix::cor_mat() %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 2))) %>%
  rstatix::cor_mark_significant( cutpoints = c(0, 0.001, 0.01, 0.05, 1),symbols = c("***", "**", "*", "")) %>%
  mutate(across(2:7, ~ sub("0.", ".", as.character(.)))) %>%
  select(-ta_rght) %>% stats::setNames(c("","1","2","3","4","5","6","7"))

ds2 %>%
  select(rwa,sdo,con,mi,hueg,polid,ta_rght,ta_lft) %>%
  tidyr::pivot_longer(cols = 1:ncol(.), names_to = "scale",values_to = "score") %>%
  group_by(scale) %>%
  summarise(M =  mean(score,na.rm = T),
            SD = sd(score,na.rm = T)) %>%
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 2))) %>% slice(5,6,1,3,2,4,7,8)
