paper3_df <- readRDS(paste0(path_cgm_working,"/paper3/paper3_df.RDS"))

# From "~/code/cgm/paper3/paper3_table1_study population.R" ------------------
gri_df <- paper3_df %>% 
  dplyr::filter(!is.na(hba)) %>% 
  group_by(patient_id) %>% 
  arrange(record_date) %>% 
  mutate(d_p = sample(c(0,1),size=1,prob = c(0.33,0.67))) %>% 
  mutate(data_partition = case_when(d_p == 1 ~ "Training",
                                    d_p == 0 ~ "Validation")) %>% 
  # mutate(data_partition = case_when(year(record_date) >= 2018 ~ "Validation",
  #                                   year(record_date) < 2018 ~ "Training",
  #                                   TRUE ~ NA_character_)) %>% 
  dplyr::mutate(nvisit = n()) %>% 
  dplyr::mutate(nvisit = as.character(nvisit)) %>% 
  dplyr::filter(record_date == min(record_date)) %>%
  dplyr::select(-date_of_visit,-record_date) %>% 
  ungroup() %>% 
  mutate_at(vars(d_retinopathy, d_retinopathy_yn,d_nephropathy, d_nephropathy_yn,d_pvd_yn,
                 pdr, npdr, npdr_or_pdr,macroprotein, dkd), 
            function(x) {
              y = as.character(x);
              y = case_when(is.na(y) ~ "Missing",
                            TRUE ~ y)
            }) %>% 
  mutate(mage_sd = r_mage/standard_deviation) %>% 
  mutate(LGRI = 3*percent_time_under_54 + 2.4*percent_time_54_70,
         HGRI = 1.6*percent_time_over_250 + 0.8*percent_time_180_250) %>% 
  mutate(GRI = LGRI + HGRI) %>% 
  mutate(HighLGRI = case_when(LGRI > (3*1 + 2.4*4) ~ 1,
                                 TRUE ~ 0),
         HighHGRI = case_when(HGRI > (1.6*5 + 0.8*20) ~ 1,
                              TRUE ~ 0)) %>% 
  mutate(profile = case_when(HighLGRI == 0 & HighHGRI == 0 ~ 1,
                             HighLGRI == 1 & HighHGRI == 0 ~ 2,
                             HighLGRI == 0 & HighHGRI == 1 ~ 3,
                             HighLGRI == 1 & HighHGRI == 1 ~ 4,
                             TRUE ~ NA_real_)) %>% 
  mutate(profile = factor(profile,levels=c(1:4),labels=c("Well controlled",
                                                         "Hypoglycemia",
                                                         "Hyperglycemia",
                                                         "Brittle"))) %>% 
  left_join(readRDS(paste0(path_cgm_working,"/cgm_output/mdrf_cgm_summary_2021-02-22.rds")) %>% 
              dplyr::select(file_name,starts_with("grade"),G),
            by = "file_name") %>% 
  mutate_at(vars(starts_with("grade"),G),~as.numeric(.))

saveRDS(gri_df,paste0(path_gri_working,"/gri_df.RDS"))
