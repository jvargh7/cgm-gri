gri_df <- readRDS(paste0(path_gri_working,"/gri_df.RDS"))

model_df <- gri_df %>% 
  mutate_at(vars(d_retinopathy,
                 d_nephropathy,
                 d_retinopathy_yn,
                 d_nephropathy_yn,
                 dkd
  ),function(x) case_when(x == "Missing" ~ NA_character_,
                          TRUE ~ as.character(x))) %>% 
  mutate_at(vars(d_retinopathy_yn,
                 d_nephropathy_yn,
                 dkd),
            function(x) case_when( x  == "0, No" ~ 0,
                                   x == "1, Yes" ~ 1,
                                   TRUE ~ NA_real_)) %>% 
  mutate(TNR10 = (100-percent_time_70_180)/10,
         GRI10 = GRI/10,
         CV36 = case_when(cv > 0.36 ~ 1,
                          TRUE ~ 0),
         NPDR = case_when(is.na(d_retinopathy) ~ NA_real_,
                          d_retinopathy == "1, NPDR" ~ 1,
                          TRUE ~ 0),
         PDR = case_when(is.na(d_retinopathy) ~ NA_real_,
                          d_retinopathy == "2, PDR" ~ 1,
                          TRUE ~ 0),
         MICRO = case_when(is.na(d_nephropathy) ~ NA_real_,
                           d_nephropathy == "1, MICRO" ~ 1,
                           TRUE ~ 0),
         MACRO = case_when(is.na(d_nephropathy) ~ NA_real_,
                           d_nephropathy == "2, MACRO" ~ 1,
                           TRUE ~ 0)
         ) 


source("models/classification_performance.R")
outcome_list <- c("d_retinopathy_yn",
                  "NPDR","PDR",
                  "d_nephropathy_yn",
                  "MICRO","MACRO",
                  "dkd")
summary_list = map(outcome_list,function(outcome){classification_performance(outcome,model_df)})
saveRDS(summary_list,paste0(path_gri_working,"/one vs all prediction list.RDS"))

summary_auc = map2_dfr(outcome_list,summary_list,
                  function(o,l){
                    l[[3]] %>% 
                      pivot_longer(cols=everything(),
                                   names_to = c("model","partition"),values_to="auc",names_sep = c("_")) %>% 
                      mutate(outcome = o) %>% 
                      return(.)
                  }) %>% 
  pivot_wider(names_from="model",values_from="auc") %>% 
  mutate(outcome = factor(outcome,levels=outcome_list)) %>% 
  arrange(partition,outcome) %>% 
  mutate_if(is.numeric,~round(.,2))

write_csv(summary_auc,"paper/prediction one vs all auc.csv")
