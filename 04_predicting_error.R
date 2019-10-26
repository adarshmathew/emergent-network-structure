if(!exists("data_loaded")){
  source("00c_data_prep_MERGE.R")
}

library(lme4)

disc_for_alpha = 
  d %>%
  subset(is.finite(alpha) & is.finite(count_chat)
           & communication=="Discussion") %>%
  group_by(communication, analysis) %>%
  mutate(
    alpha_quant = cut(stubborn_cent,
                      breaks=quantile(stubborn_cent), include.lowest=T) %>% as.numeric
    , count_quant = cut(count_chat,
                        breaks=quantile(count_chat), include.lowest=T) %>% as.numeric
  )

delph_for_alpha = 
  d %>%
  subset(is.finite(alpha) & communication=="Discussion") %>%
  group_by(communication, analysis) %>%
  mutate(
    alpha_quant = cut(stubborn_cent,
                      breaks=quantile(stubborn_cent), include.lowest=T) %>% as.numeric
  )


mod1=lmer(
  log(err_norm+0.01) ~ count_quant 
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="reanalysis")
)
summary(mod1)

mod2=lmer(
  log(abs(err_norm)+0.01) ~ alpha_quant
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="reanalysis")
)

mod3=lmer(
  log(abs(err_norm)+0.01) ~ count_quant + alpha_quant 
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="reanalysis")
)


mod4=lmer(
  log(abs(err_norm)+0.01) ~ alpha_quant 
  + (1|task)
  , data=delph_for_alpha %>% subset(analysis=="reanalysis")
)


stargazer::stargazer(mod1, mod2, mod3, mod4
          ,type="html"
          ,out="Figures/error_correlation.html"
          , dep.var.caption = "% Error (Logged)"
          , dep.var.labels=c("","")
          , covariate.labels=c("Talkativeness","Stubbornness","Intercept")
          , column.labels=c("Discussion","<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Delphi&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
          , column.separate=c(3,1)
          , model.numbers=F
          , digits=2
          , star.cutoffs=c(0.1, 0.05,0.01,0.001)
          , star.char=c("+","*","**","***")
          , notes = c("<sup>+</sup>p<0.1  <sup>*</sup>p<0.05  <sup>**</sup>p<0.01  <sup>***</sup>p<0.001")
          , notes.append = F
          
)





########
####### replication
##########3


rep.mod1=lmer(
  log(err_norm+0.01) ~ count_quant 
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="replication")
)

rep.mod2=lmer(
  log(abs(err_norm)+0.01) ~ alpha_quant
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="replication")
)

rep.mod3=lmer(
  log(abs(err_norm)+0.01) ~ count_quant + alpha_quant 
  + (1|task)
  , data=disc_for_alpha %>% subset(analysis=="replication")
)


rep.mod4=lmer(
  log(abs(err_norm)+0.01) ~ alpha_quant 
  + (1|task)
  , data=delph_for_alpha %>% subset(analysis=="replication")
)

summary(rep.mod1)
summary(rep.mod2)
summary(rep.mod3)
summary(rep.mod4)


stargazer::stargazer(rep.mod1, rep.mod2, rep.mod3, rep.mod4
                     ,type="html"
                     ,out="Figures/error_correlation_replication.html"
                     , dep.var.caption = "% Error (Logged)"
                     , dep.var.labels=c("","")
                     , covariate.labels=c("Talkativeness","Stubbornness","Intercept")
                     , column.labels=c("Discussion","<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Delphi&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                     , column.separate=c(3,1)
                     , model.numbers=F
                     , digits=2
                     , star.cutoffs=c(0.1, 0.05,0.01,0.001)
                     , star.char=c("+","*","**","***")
                     , notes = c("<sup>+</sup>p<0.1  <sup>*</sup>p<0.05  <sup>**</sup>p<0.01  <sup>***</sup>p<0.001")
                     , notes.append = F
                     
)
