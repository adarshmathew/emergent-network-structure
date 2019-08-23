#source("00c_data_prep_MERGE.R")
require(lme4)
require(RColorBrewer)






mod.reanalysis = aggreg %>%
  subset(communication=="Discussion" & analysis=="reanalysis") %>%
  ungroup %>%
  glmer(
    formula = improve ~ 
      prop_toward*gini_talkativeness
    + alpha_cor 
    + total_talkativeness
    + (1 | task)
    , data=.
    , family="binomial"
  )


mod.replication = aggreg %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  ungroup %>%
  glmer(
    data=.,
    formula = improve ~ 
      prop_toward*gini_talkativeness
    + alpha_cor 
    + total_talkativeness
    + (1 | task)
    , family="binomial"
  )


summary(mod.reanalysis)
summary(mod.replication)
summary(mod.combined)

alpha_val = mean(aggreg$alpha_cor[aggreg$communication=="Discussion"], na.rm=T)

rep_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="replication"], na.rm=T)

ren_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="reanalysis"], na.rm=T)


n=100000

sim_data.replication =
  data.frame(
    prop_toward = runif(n,0.05,0.95)
    , gini_talkativeness = runif(n,rep_gini_range[1],rep_gini_range[2])
    , alpha_cor = alpha_val#runif(n, -1, 1)
    , task = sample(unique(aggreg$task[aggreg$analysis=="replication" & aggreg$communication=="Discussion"]), n, replace=T)
    , total_talkativeness=50
  )

sim_data.reanalysis =
  data.frame(
      prop_toward = runif(n,0.05,0.95)
    , gini_talkativeness = runif(n,ren_gini_range[1],ren_gini_range[2])
    , alpha_cor = alpha_val#runif(n, -1, 1)
    , task = sample(unique(aggreg$task[aggreg$analysis=="reanalysis" & aggreg$communication=="Discussion"]), n, replace=T)
    , total_talkativeness=36
  )


log_odds_to_prob = function(x){
  exp(x)/(1+exp(x))
}

sim_data.replication$improve = predict(mod.replication, sim_data.replication)  %>% log_odds_to_prob

sim_data.reanalysis$improve = predict(mod.reanalysis, sim_data.reanalysis) %>% log_odds_to_prob


mypalette = mypalette<-brewer.pal(4,"RdYlGn")

x_range = round(range(aggreg$gini_talkativeness, na.rm=T),2)


sim_data.reanalysis %>%
  mutate(
    prop_toward = cut(prop_toward, breaks=seq(0,1,by=0.25), include.left=T) %>%
      factor() %>% recode(`(0,0.25]`="0-25%",`(0.25,0.5]`="25-50%",`(0.5,0.75]`="50-75%",`(0.75,1]`="75-100%")
    , gini_talkativeness = round(gini_talkativeness, 2)
  ) %>%
  group_by(prop_toward, gini_talkativeness) %>%
  summarize(
    improve=mean(improve)
  ) %>%
  ggplot(aes(x=gini_talkativeness, y=improve, color=prop_toward)) +
  geom_line() +
  scale_y_continuous(lim=c(0,1), labels=pct_labels)+
  scale_x_continuous(lim=x_range)+
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_color_manual(values=c('#ca0020','#f4a582','#92c5de','#0571b0')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)",
       title="Reanalysis Data") +
  nice_theme()


ggsave("Figures/effect_of_centralization_reanalysis.png", width=3.2, height=2.2)


sim_data.replication %>%
  mutate(
    prop_toward = cut(prop_toward, breaks=seq(0,1,by=0.25), include.left=T) %>%
      factor() %>% recode(`(0,0.25]`="0-25%",`(0.25,0.5]`="25-50%",`(0.5,0.75]`="50-75%",`(0.75,1]`="75-100%")
    , gini_talkativeness = round(gini_talkativeness, 2)
  ) %>%
  group_by(prop_toward, gini_talkativeness) %>%
  summarize(
    improve=mean(improve)
  ) %>%
  ggplot(aes(x=gini_talkativeness, y=improve, color=prop_toward)) +
  geom_line() +
  scale_y_continuous(lim=c(0,1), labels=pct_labels)+
  scale_x_continuous(lim=x_range)+
  geom_hline(yintercept=0.5, linetype="dashed")+
  scale_color_manual(values=c('#ca0020','#f4a582','#92c5de','#0571b0')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)"
       , title="Replication Data") +
  nice_theme()

ggsave("Figures/effect_of_centralization_replication.png", width=3.2, height=2.2)
