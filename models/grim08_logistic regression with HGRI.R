
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
  mutate(HGRI10 = HGRI/10) 

# Prevalent Retinopathy ~ GRI ----------------------

multinom_retinopathy = model_df %>% 
  dplyr::filter(data_partition == "Training") %>% 
  nnet::multinom(d_retinopathy ~ HGRI10 + ageonset + d_duration + gender + hba + bmi,
                 data=.)


# validation_retinopathy = model_df %>% 
#   dplyr::filter(data_partition == "Validation") %>% 
#   predict(multinom_nephropathy,newdata=.)

# Prevalent Nephropathy ~ GRI ------------------------

multinom_nephropathy = model_df %>% 
  dplyr::filter(data_partition == "Training") %>% 
  nnet::multinom(d_nephropathy ~ HGRI10 + ageonset + d_duration + gender+ hba + bmi,
                 data=.)

# Prevalent DKD ~ GRI ---------------------
glm_dkd = model_df %>% 
  dplyr::filter(data_partition == "Training") %>% 
  glm(dkd ~ HGRI10 + ageonset + d_duration + gender+ hba + bmi,
      family=binomial(),data=.)

summary_df = bind_rows(broom::tidy(multinom_nephropathy) %>% 
                         mutate(type = "multinom nephropathy"),
                       broom::tidy(multinom_retinopathy)%>% 
                         mutate(type = "multinom retinopathy"),
                       broom::tidy(glm_dkd) %>% 
                         mutate(type = "glm dkd")
) %>% 
  mutate(output = paste0("OR = ",exp(estimate) %>% round(.,2)," (",
                         exp(estimate - 1.96*std.error) %>% round(.,2),", ",
                         exp(estimate + 1.96*std.error) %>% round(.,2),")"
  )) %>% 
  dplyr::filter(str_detect(term,"HGRI10")) 

saveRDS(summary_df,paste0(path_gri_working,"/logistic regression with HGRI.RDS"))
