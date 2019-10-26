if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
require(lme4)

###
### REANALYSIS
###


aggreg %>% 
  subset(prop_toward!=0.5) %>%
  mutate(
    majority_toward = ifelse(prop_toward>0.5, "Majority Toward","Majority Away")
    #, majority_toward = cut(prop_toward, c(0,0.25,0.5,0.75,1), include.lowest=T)
  ) %>% 
  group_by(communication, majority_toward) %>%
  summarize(
    lower = 1-binom.test(table(improve))$conf.int[2]
    , upper = 1-binom.test(table(improve))$conf.int[1]
    , N=length(improve)
    , improve=mean(improve)
  ) %>%
  ggplot(aes(x=majority_toward, y=improve, color=communication
             )) +
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0,position=position_dodge(0.2)) +
  nice_theme()


### MAJORITY REVERSES EFFECTS IN DISUCSSION
aggreg %>%
  subset(prop_toward>0.5) %>% 
  subset(analysis=="reanalysis") %>%
  subset(communication=="Discussion") %>%
  tidy_table_univariate(improve) %>%
  #prop.table
  prop.test


aggreg %>%
  subset(prop_toward<0.5) %>% 
  subset(analysis=="reanalysis") %>%
  subset(communication=="Discussion") %>%
  tidy_table_univariate(improve) %>%
  prop.test


### TWO SAMPLE COMPARISON

aggreg %>%
  subset(prop_toward>0.75) %>% 
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary


aggreg %>%
  subset(prop_toward>0.75) %>% 
  subset(analysis=="reanalysis") %>%
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary

aggreg %>%
  subset(prop_toward<0.5) %>% 
  subset(analysis=="reanalysis") %>%
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary





###
### REPLICATION
###



### MAJORITY REVERSES EFFECTS IN DISUCSSION
aggreg %>%
  subset(prop_toward>0.5) %>% 
  subset(analysis=="replication") %>%
  subset(communication=="Discussion") %>%
  tidy_table_univariate(improve) %>%
  #prop.table
  prop.test


aggreg %>%
  subset(prop_toward<0.5) %>% 
  subset(analysis=="replication") %>%
  subset(communication=="Discussion") %>%
  tidy_table_univariate(improve) %>%
  #prop.table
  prop.test


### TWO SAMPLE COMPARISON

aggreg %>%
  subset(prop_toward>0.5) %>% 
  subset(analysis=="replication") %>%
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary


aggreg %>%
  subset(prop_toward>0.75) %>% 
  subset(analysis=="replication") %>%
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary

aggreg %>%
  subset(prop_toward<0.5) %>% 
  #subset(analysis=="replication") %>%
  glmer(
    formula = improve ~ 
      communication
    + (1|trial)
    , data=.
    , family="binomial"
  ) %>% summary



