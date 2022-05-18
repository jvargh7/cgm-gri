summary_list <- readRDS(paste0(path_gri_working,"/one vs all prediction list.RDS"))

require(ggpubr)
outcome_list <- c("d_retinopathy_yn",
                  "NPDR","PDR",
                  "d_nephropathy_yn",
                  "MICRO","MACRO",
                  "dkd")


outcome_names <- c("NPDR or PDR",
                   "NPDR","PDR",
                   "Micro or Macroalbuminuria",
                   "Microalbuminuria","Macroalbuminuria",
                   "DKD")


pdf(file="paper/discrimination and calibration.pdf",width = 12,height=8)
for(o in 1:length(outcome_list)){
  # Training
  boxplot_A <- bind_rows(
    summary_list[[o]][[1]] %>% 
    dplyr::filter(model == 1),
    summary_list[[o]][[2]] %>% 
      dplyr::filter(model == 1)
    ) %>% 
    mutate(outcome = factor(outcome,levels=c(0,1),labels=c("No","Yes"))) %>% 
    ggplot(data=.,aes(fill = outcome,y=pred,x=dp)) +
    geom_boxplot(position = "dodge") +
    xlab("") +
    ylab("Probability of complication") +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_fill_manual(name = "",values=c("red","darkgreen")) +
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2))
  
  dotplot_B <- bind_rows(
    summary_list[[o]][[1]] %>% 
      dplyr::filter(model == 1),
    summary_list[[o]][[2]] %>% 
      dplyr::filter(model == 1)
  ) %>% 
   ggplot(data=.,aes(y=outcome,x=pred,col=dp)) +
    scale_y_continuous(limits = c(0,1),breaks=c(0,1)) +
    scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2)) +
    geom_smooth(aes(x=pred,y=outcome,group=dp),se=F,method = "loess") +
    geom_abline (linetype = 2,col="grey20") +
    theme_bw() +
    xlab("Predicted probability") +
    ylab("Complication") +
    theme(legend.position = "bottom") +
    scale_color_manual(name="",values=c("purple","orange"))
  
  
fig_common = ggarrange(boxplot_A,
            dotplot_B,
            ncol = 2,
            nrow = 1,
            labels=c("A","B"))

annotate_figure(fig_common,top=text_grob(paste0("SFig ",o,".",outcome_names[o]),
                                         color = "black",face = "bold",size=18)) %>% 
  print(.)
  
  
}

dev.off()
