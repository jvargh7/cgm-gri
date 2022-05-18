require(pROC)


auc_ci = function(pROC_obj){
  
  auc = pROC_obj$ci[[2]]
  lci = pROC_obj$ci[[1]]
  uci = pROC_obj$ci[[3]]
  
  return(paste0(round(auc,2)," (",
                round(lci,2),", ",
                round(uci,2),")"))
}

classification_performance <- function(o,df){
  
  f1 <- paste0(o,"~ GRI10 + ageonset + d_duration + gender + hba + bmi")
  f2 <- paste0(o,"~ TNR10 + ageonset + d_duration + gender + hba + bmi")
  f3 <- paste0(o,"~ TNR10 + CV36 + ageonset + d_duration + gender + hba + bmi")
  
  train = df %>% dplyr::filter(data_partition == "Training")
  validation = df %>% dplyr::filter(data_partition == "Validation") %>% dplyr::filter_at(vars(one_of(o)),~!is.na(.))
  
  o_train = train %>% dplyr::select(one_of(o)) %>% pull() %>% na.omit()
  o_validation = validation %>% dplyr::select(one_of(o)) %>% pull() %>% na.omit()
  
  m1 <- glm(as.formula(f1),data=train,family=binomial())
  m2 <- glm(as.formula(f2),data=train,family=binomial())
  m3 <- glm(as.formula(f3),data=train,family=binomial())
  
  
  p1_train <- predict(m1,type="response")
  p2_train <- predict(m2,type="response")
  p3_train <- predict(m3,type="response")
  training = bind_rows(
    bind_cols(p1_train,o_train) %>% mutate(dp = "Training",model = 1),
    bind_cols(p2_train,o_train) %>% mutate(dp = "Training",model = 2),
    bind_cols(p3_train,o_train) %>% mutate(dp = "Training",model = 3),
  ) %>% 
    rename(pred = '...1',
           outcome = '...2') %>% 
    mutate(pred = as.numeric(pred))
  
  
  p1_validation <- predict(m1,newdata=validation,type="response")
  p2_validation <- predict(m2,newdata=validation,type="response")
  p3_validation <- predict(m3,newdata=validation,type="response")
  validation = bind_rows(
    bind_cols(p1_validation,o_validation) %>% mutate(dp = "Validation",model = 1),
    bind_cols(p2_validation,o_validation) %>% mutate(dp = "Validation",model = 2),
    bind_cols(p3_validation,o_validation) %>% mutate(dp = "Validation",model = 3),
  ) %>% 
    rename(pred = '...1',
           outcome = '...2')  %>% 
    mutate(pred = as.numeric(pred))
  
  
  
  auc <- data.frame(
    m1_train = auc_ci(roc(o_train ~ p1_train,ci=TRUE)),
    m2_train = auc_ci(roc(o_train ~ p2_train,ci=TRUE)),
    m3_train = auc_ci(roc(o_train ~ p3_train,ci=TRUE)),
    m1_validation = auc_ci(roc(o_validation ~ p1_validation,ci=TRUE)),
    m2_validation = auc_ci(roc(o_validation ~ p2_validation,ci=TRUE)),
    m3_validation = auc_ci(roc(o_validation ~ p3_validation,ci=TRUE))
  )
  
  output <- list(
  training,
  validation,
  auc
  )
  
  return(output)
  
}
