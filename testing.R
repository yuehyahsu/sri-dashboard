data %>% 
  filter(agency == "Caritas" & country == "Syria" & round == 1) %>% 
  select_at(vars("phase", eval(parse(text = str_c("domain8", "_vars"))))) %>% 
  mutate(phase = factor(phase, levels = c("targeting", "followup", "followup #2"))) %>% 
  reshape2::melt(id.vars = "phase") %>% 
  group_by(phase, variable) %>%
  mutate(prop = sum(value, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-value) %>% 
  unique()


summ_data <- data %>% 
  filter(survey %in% c("Caritas/Syria/Rd 1") & phase == "targeting") %>% 
  select_at(vars("survey", eval(parse(text = str_c("domain9", "_vars"))))) %>%
  group_by(survey) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, funs(percent(., 1)))
colnames(summ_data)[2:ncol(summ_data)] <- eval(parse(text = str_c("domain9", "_labels")))
summ_data

any(gsub("/.*", "", c("Caritas/Syria/Rd 1", "Bethany/hi/Rd3")) %in% "Caritas")
