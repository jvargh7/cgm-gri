m_OR <- readRDS(paste0(path_gri_working,"/logistic regression with profiles.RDS"))

table_or <- m_OR %>%
  dplyr::select(y.level,term,type,output) %>% 
  pivot_wider(names_from=term,values_from=output)

write_csv(table_or,"paper/table profiles odds ratios.csv")
