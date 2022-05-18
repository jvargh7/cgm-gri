m1a_OR <- readRDS(paste0(path_gri_working,"/logistic regression with GRI SD.RDS"))
m1b_OR <- readRDS(paste0(path_gri_working,"/logistic regression with LGRI.RDS"))
m1c_OR <- readRDS(paste0(path_gri_working,"/logistic regression with HGRI.RDS"))
m1d_OR <- readRDS(paste0(path_gri_working,"/logistic regression with LGRI HGRI.RDS"))


table_or <- bind_rows(m1a_OR %>% mutate(model = "Model 1a"),
                      m1b_OR %>% mutate(model = "Model 1b"),
                      m1c_OR %>% mutate(model = "Model 1c"),
                      m1d_OR %>% mutate(model = "Model 1d")) %>%
  dplyr::select(y.level,term,type,output,model) %>% 
  pivot_wider(names_from=c(term,model),values_from=output)

write_csv(table_or,"paper/table odds ratios for exploratory.csv")