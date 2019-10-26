if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

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

alpha_val = mean(aggreg$alpha_cor[aggreg$communication=="Discussion"], na.rm=T)

rep_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="replication"], na.rm=T)

ren_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="reanalysis"], na.rm=T)


n=1000000

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
    prop_toward = ifelse(prop_toward>0.5,"High","Low")
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
  scale_color_manual(values=c('#0571b0','#ca0020')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)",
       title="Reanalysis Data") +
  nice_theme()


ggsave("Figures/effect_of_centralization_reanalysis__fortalks.png", width=3.2, height=2.2)


sim_data.replication %>%
  mutate(
    prop_toward = ifelse(prop_toward>0.5,"High","Low")
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
  scale_color_manual(values=c('#0571b0','#ca0020')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)"
       , title="Replication Data") +
  nice_theme()

ggsave("Figures/effect_of_centralization_replication__for_talks.png", width=3.2, height=2.2)






mod.replication = aggreg %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  ungroup %>%
  mutate(
    alpha_cor_scaled = alpha_cor-(mean(alpha_cor, na.rm=T)/sd(alpha_cor, na.rm=T))
    , prop_scaled = prop_toward-(mean(prop_toward, na.rm=T)/sd(prop_toward, na.rm=T))
    , gini_scaled = gini_talkativeness-(mean(gini_talkativeness, na.rm=T)/sd(gini_talkativeness, na.rm=T))
    , talk_scaled = total_talkativeness-(mean(total_talkativeness, na.rm=T)/sd(total_talkativeness, na.rm=T))
  ) %>%
  glmer(
    formula = improve ~ 
      prop_scaled*gini_scaled
    + alpha_cor_scaled
    + talk_scaled
    + (1 | task)
    , data=.
    , family="binomial"
  ) %>% summary






summary(mod.reanalysis)
summary(mod.replication)
summary(mod.combined)

alpha_val = mean(aggreg$alpha_cor[aggreg$communication=="Discussion"], na.rm=T)

rep_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="replication"], na.rm=T)

ren_gini_range = range(aggreg$gini_talkativeness[aggreg$analysis=="reanalysis"], na.rm=T)


n=1000000

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
    prop_toward = ifelse(prop_toward>0.5,"High","Low")
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
  scale_color_manual(values=c('#0571b0','#ca0020')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)",
       title="Reanalysis Data") +
  nice_theme()


ggsave("Figures/effect_of_centralization_reanalysis__fortalks.png", width=3.2, height=2.2)


sim_data.replication %>%
  mutate(
    prop_toward = ifelse(prop_toward>0.5,"High","Low")
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
  scale_color_manual(values=c('#0571b0','#ca0020')) +
  labs(y="Probability of Improving", x="Talkativeness Centralization (Gini)"
       , title="Replication Data") +
  nice_theme()

ggsave("Figures/effect_of_centralization_replication__for_talks.png", width=3.2, height=2.2)
