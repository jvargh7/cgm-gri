gri_df <- readRDS(paste0(path_gri_working,"/gri_df.RDS"))
require(compareGroups)
tab1_res <-  gri_df %>% 
  mutate(cv36 = case_when(cv > 0.36 ~ 1,
                          TRUE ~ 0)) %>% 
  dplyr::select(profile,
                average_sensor,gmi,
                cv36,
                percent_time_70_180,
                percent_time_under_54,
                percent_time_54_70,
                percent_time_180_250,
                percent_time_over_250,
                
                r_mage,standard_deviation,mage_sd,G,
                hbgi,lbgi,grade_hyper,grade_hypo,
                
                d_current_age, gender, ageonset, d_duration,
                bmi, d_bmi_category,
                hba,fbs,ppb,
                tri, hdl, ldl,
                insulin, 
                
                d_retinopathy,
                egfr,
                d_nephropathy,
                dkd,
                GRI, LGRI, HGRI) %>% 
  compareGroups::compareGroups( profile ~ .,
                                include.miss = FALSE,
                                data=.,
                                method = c(1,1,3,
                                           2,2,2,2,2,
                                           
                                           1,2,2,2,
                                           2,2,2,2,
                                           
                                           1,3,1,1,
                                           1,3,1,1,1,
                                           1,1,1,3,
                                           3,1,3,3,
                                           1,1,1)) %>% 
  createTable(.,digits=1,
              show.all = TRUE,
              show.p.overall = TRUE,
              show.n = TRUE,sd.type = 2) 
tab1_res %>% 
  export2xls(.,file = "paper/descriptive characteristics.xlsx")

