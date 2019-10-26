if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}



aggreg %>%
  subset(communication=="Discussion") %>%
  ### remove those 5 trials with no chat data
  subset(!is.na(central_twd_truth)) %>%
  mutate(
    #improve = ifelse(improve, "Improve","Worse")
     central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>% 
  group_by(central_twd_truth) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    ,improve=mean(improve)
  ) %>%
  ggplot(aes(x=central_twd_truth, y=improve#, color=analysis
             )) +
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(0.5)) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_y_continuous(labels=pct_labels)+
  theme_test()

ggsave("Figures/Central Node Predicts.png", width=2, height=3)

aggreg %>%
  subset(communication=="Discussion" & analysis=="reanalysis") %>%
  mutate(
    improve = ifelse(improve, "Improve","Worse")
    , central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>%
  tidy_table(central_twd_truth, improve) %>%
  #prop.table(margin=1)
  prop.test
  
  

aggreg %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  mutate(
    improve = ifelse(improve, "Improve","Worse")
    , central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>%
  tidy_table(central_twd_truth, improve) %>%
  #prop.table(margin=1)
  prop.test


aggreg %>%
  subset(communication=="Discussion") %>%
  mutate(
     central_twd_truth = ifelse(central_twd_truth, "Toward","Away")
  ) %>%
  glmer(
    improve ~ central_twd_truth 
    + (1|task) 
    + (1|dataset)
    , family="binomial"
    , data=.
  ) %>% summary
  
glmer(toward_truth=="Toward" ~ count_chat 
      + (1|task)
    , data=d_valid %>% subset(analysis=="reanalysis")
    , family="binomial") %>%
  summary

glmer(toward_truth=="Toward" ~ count_chat 
      + (1|task)
      , data=d_valid %>% subset(analysis=="replication")
      , family="binomial") %>%
  summary


glm(toward_truth=="Toward" ~ alpha
      #+ (1|task)
      , data=d_valid %>% subset(is.finite(alpha) & analysis=="reanalysis")
      , family="binomial") %>%
  summary



glm(toward_truth=="Toward" ~ count_chat 
      , data=d_valid
      , family="binomial") %>%
  summary


glmer(toward_truth=="Toward" ~ err_norm
      + (1|task/dataset)
    , d_valid %>% subset(err_norm<5)
    , family="binomial") %>%
  summary


ggplot(aggreg, aes(x=central_diff_from_mu, y=change_mu_norm)) +
  geom_point() +
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0) +
  scale_y_continuous(lim=c(-2,2)) + 
  scale_x_continuous(lim=c(-3,3)) +
  geom_smooth(method='lm',formula=y~x, se=F)+
  labs(x="Diff. Btw Group & Central Person", y="Change in Group Belief")+
  facet_grid(.~analysis)+
  nice_theme()

ggsave("Figures/Central_Predicts_Group.png", width=3, height=3)

aggreg %>% 
  subset(change_mu_norm >(-1.06) & change_mu_norm<147) %$%
  cor.test(change_mu_norm, central_diff_from_mu)

lm(change_mu_norm ~ central_diff_from_mu,
   aggreg %>% subset(change_mu_norm >(-1.06) & change_mu_norm<147)
   ) %>% summary

quantile(aggreg$change_mu_norm, probs=c(0.01,0.5,0.99))

mean(aggreg$change_mu_norm<147)
mean(aggreg$change_mu_norm>(-1.07))
