m1_OR <- readRDS(paste0(path_gri_working,"/logistic regression with GRI.RDS"))
m2_OR <- readRDS(paste0(path_gri_working,"/logistic regression with TNR.RDS"))
m3_OR <- readRDS(paste0(path_gri_working,"/logistic regression with TNR CV.RDS"))


table_or <- bind_rows(m1_OR %>% mutate(model = "Model 1"),
                      m2_OR %>% mutate(model = "Model 2"),
                      m3_OR %>% mutate(model = "Model 3")) %>%
  dplyr::select(y.level,term,type,output,model) %>% 
  pivot_wider(names_from=c(term,model),values_from=output)

write_csv(table_or,"paper/table odds ratios.csv")
