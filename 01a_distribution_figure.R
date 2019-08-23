source("00c_data_prep_MERGE.R")


require(lme4)

d_sum = aggreg %>% 
  subset(prop_toward!=0.5) %>%
  mutate(
    majority_toward = cut(prop_toward, c(0,0.25,0.5,0.75,1), include.lowest=T)
  ) %>% 
  group_by(communication, majority_toward, analysis) %>%
  summarize(
      lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    , N=length(improve)
    , improve=mean(improve)
  )

levels(d_sum$majority_toward) = c("0-25%", "25-50%", "50-75%", "75-100%")

d_sum$communication = factor(d_sum$communication, levels=c("Discussion","Delphi"))


plot_data = function(this_analysis) {
  subset(d_sum, analysis==this_analysis) %>%
  ggplot(
    aes(x=majority_toward, y=improve)) +
    geom_hline(yintercept=0.5, linetype="dashed") +
    geom_point(position=position_dodge(0.5), size=4) +
    geom_errorbar(aes(ymin=lower, ymax=upper)
                  , size=1.15, width=0, position=position_dodge(0.5))+
    #geom_label(aes(label=paste0(N), y=label_location), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))+
    scale_y_continuous(expand = c(0,0), 
                       lim=c(-0.01,1.01)
                       , labels=pct_labels)+
    guides(color=F)+
    labs(x="Proportion Toward Truth", y="", color="") +
    facet_wrap(.~communication, scales="free")+
    nice_theme()
}



label_location =(d_sum$lower[d_sum$analysis=="replication"])

plot_data("replication") +
  geom_label(aes(label=paste0(N), y=label_location), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))

label_location =(d_sum$lower[d_sum$analysis=="reanalysis"]) + 
  c(0.6, 0.325, 0.275, 0.275
    , 0.25,-0.065, -0.065, -0.07)


plot_data("reanalysis") +
  geom_label(aes(label=paste0(N), y=label_location), size=3.5, position=position_dodge(0.5), label.padding=unit(0.15,"lines"))

ggsave("Figures/effect_of_distribution_reanalysis.png", width=6, height=3)
