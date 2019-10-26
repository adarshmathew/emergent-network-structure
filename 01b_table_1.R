if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}
require(lme4)


### DISCUSSION
mod1 = aggreg %>%
  ungroup %>%
  subset(communication=="Discussion" & analysis=="reanalysis") %>%
  glmer(
    improve ~ 
      prop_toward
    + (1|trial)
    , data=.
    ,family="binomial"
  )

mod2 = aggreg %>%
  ungroup %>%
  subset(communication=="Discussion" & analysis=="reanalysis") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|trial)
    , data=.
    ,family="binomial"
  )



### DELPHI
mod3 = aggreg %>%
  ungroup %>%
  subset(communication=="Delphi" & analysis=="reanalysis") %>%
  glmer(
    improve ~ 
      prop_toward
    + (1|trial)
    , data=.
    ,family="binomial"
  )

mod4 = aggreg %>%
  ungroup %>%
  subset(communication=="Delphi" & analysis=="reanalysis") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|trial)
    , data=.
    ,family="binomial"
  )

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


stargazer::stargazer(mod1, mod2, mod3, mod4
          , out="Figures/basic_test.html", type="html"
          , column.labels=c("Discussion","Numeric Exchange")
          , column.separate=c(2,2)
          , dep.var.labels.include=F
          , dep.var.caption="Binomial outcome:  mean closer to truth"
          , covariate.labels = c("<p>&phi;</p>","Stub./Err. Corr.")
          , star.cutoffs=c(0.1, 0.05,0.01,0.001)
          , star.char=c("+","*","**","***")
          , notes = c("<sup>+</sup>p<0.1  <sup>*</sup>p<0.05  <sup>**</sup>p<0.01  <sup>***</sup>p<0.001")
          , notes.append = F
)


############<3
############# REPLICATION
############<3


### DISCUSSION
rep.mod1 = aggreg %>%
  ungroup %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  glmer(
    improve ~ 
      prop_toward
    + (1|task)
    , data=.
    ,family="binomial"
  )

rep.mod2 = aggreg %>%
  ungroup %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|task)
    , data=.
    ,family="binomial"
  )


summary(rep.mod2)

### DELPHI
rep.mod3 = aggreg %>%
  ungroup %>%
  subset(communication=="Delphi" & analysis=="replication") %>%
  glmer(
    improve ~ 
      prop_toward
    + (1|task)
    , data=.
    ,family="binomial"
  )

rep.mod4 = aggreg %>%
  ungroup %>%
  subset(communication=="Delphi" & analysis=="replication") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|task)
    , data=.
    ,family="binomial"
  )




summary(rep.mod1)
summary(rep.mod2)
summary(rep.mod3)
summary(rep.mod4)

stargazer::stargazer(rep.mod1, rep.mod2, rep.mod3, rep.mod4
                     , out="Figures/basic_test_replication.html", type="html"
                     , column.labels=c("Discussion","Numeric Exchange")
                     , column.separate=c(2,2)
                     , dep.var.labels.include=F
                     , dep.var.caption="Binomial outcome:  mean closer to truth"
                     , covariate.labels = c("<p>&phi;</p>","Stub./Err. Corr.")
                     , star.cutoffs=c(0.1, 0.06,0.01,0.001)
                     , star.char=c("+","*","**","***")
                     , notes = c("<sup>+</sup>p<0.06  <sup>*</sup>p<0.05  <sup>**</sup>p<0.01  <sup>***</sup>p<0.001")
                     , notes.append = F
)



### COMBINED
aggreg %>%
  ungroup %>%
  subset(communication=="Delphi") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|task)
    , data=.
    ,family="binomial"
  ) %>% summary




aggreg %>%
  ungroup %>%
  subset(communication=="Discussion" & analysis=="replication") %>%
  glmer(
    improve ~ 
      prop_toward
    + alpha_cor
    + (1|task)
    , data=.
    ,family="binomial"
  ) %>% summary

