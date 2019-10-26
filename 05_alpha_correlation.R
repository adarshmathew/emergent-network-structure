if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}



aggreg %>% 
  #subset(communication=="Delphi") %>%
  ggplot(aes(x=round(alpha_cor*5)/5, y=improve, color=analysis)) + 
    stat_summary(fun.y="mean", geom="point", size=0.75) +
    stat_summary(fun.y="mean", geom="line") +
    facet_grid(communication~.)+
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_vline(xintercept=0, linetype="dashed") +
    scale_y_continuous(lim=c(0,1))+
    scale_x_continuous(lim=c(-1,1))+
    nice_theme()

81+108+63+21
