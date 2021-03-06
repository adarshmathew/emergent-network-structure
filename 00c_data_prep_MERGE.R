rm(list=ls());gc()
require(tidyverse)
require(DescTools)
source("00a_data_prep_REANALYSIS.R")
source("00b_data_prep_REPLICATION.R")


cols=c("pre_influence","post_influence","truth","task","trial"
       ,"communication","count_chat","count_words","analysis","dataset"
       , "soc_info")

d = rbind(
    delphi_replication[,cols] %>% as.data.frame(stringsAsFactors=F)
  , disc_replication[,cols] %>% as.data.frame(stringsAsFactors=F)
  , delphi_reanalysis[,cols] %>% as.data.frame(stringsAsFactors=F)
  , disc_reanalysis[,cols] %>% as.data.frame(stringsAsFactors=F)
) %>% 
  ungroup %>% 
  mutate(
    mu1 = mean(pre_influence, na.rm=T)
    , alpha_back = (post_influence - soc_info)/(pre_influence-soc_info)
    , alpha = (pre_influence - post_influence)/(pre_influence-soc_info)
    
    #, alpha = ifelse(alpha<0, 0, alpha) 
    #, alpha = ifelse(alpha>1, 1, alpha)
  
    , stubborn_cent = 1-alpha
    , err = abs(pre_influence - truth)
    , err_norm = abs(err/truth)
    #, log_err_norm = log(pre_influence/truth)
  )

chat_stats = d %>%
  group_by(task, trial, communication, analysis, dataset) %>%
  ## get chat stats including ALL people participating in chat
  summarize(
      gini_talkativeness = Gini(count_chat)
    , gini_words = Gini(count_words)
    , mean_talkativeness = mean(count_chat)
    , mean_words = mean(count_words)
    , total_talkativeness=sum(count_chat)
    , total_words = sum(count_words)
    , count_in_convo = sum(count_words!=0)
  )

d_valid = d %>%
  ### get accuracy stats only for people answering at both time 1 and 2
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  group_by(task, trial, communication, analysis, dataset) %>%
  mutate(
    mu1 = mean(pre_influence)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  )

aggreg =  d_valid %>%
  group_by(task) %>%
  mutate(
    sd_pool = sd(pre_influence)
  ) %>%
  group_by(task, trial, communication, analysis, dataset) %>%
  summarize(
    truth=unique(truth)
    , N_valid = length(pre_influence)
    
    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    
    ## cal median
    , med1 = median(pre_influence)
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
    
    ## centralization
    , gini_alpha = Gini(stubborn_cent)
    , gini_alpha = ifelse(is.na(gini_alpha), 0, gini_alpha)
    
    ## other alpha
    , alpha_cor = cor.test(stubborn_cent[is.finite(stubborn_cent)], err[is.finite(stubborn_cent)], na.rm=T)$estimate
    , move_cor = cor.test(err, (pre_influence==post_influence)*1)$estimate
    
    , gini_talkativeness = Gini(count_chat)
    , gini_words = Gini(count_words)
    , mean_talkativeness = mean(count_chat)
    , mean_words = mean(count_words)
    , total_talkativeness=sum(count_chat)
    , total_words = sum(count_words)
    , count_in_convo = sum(count_words!=0)
    
    ### extra stuff
    , sd = sd(pre_influence)
    , sd_pool = unique(sd_pool)
    , change_mu_norm = (mu2-mu1)/sd
    
    ### is the most talkative person toward truth?
    #, central_twd_truth = ifelse(sum(!is.na(count_chat))==0, NA, toward_truth[which.max(count_chat)]=="Toward")
    , central_twd_truth = ifelse(sum(!is.na(count_chat))==0, NA, toward_truth[!is.na(toward_truth)][which.max(count_chat[!is.na(toward_truth)])]=="Toward")
    , central_pre_influence = ifelse(sum(!is.na(count_chat))==0, NA, pre_influence[which.max(count_chat)])
    , central_diff_from_mu =  (central_pre_influence-mu1)/sd
  ) %>%
  mutate(
     prop_toward_round=round(prop_toward,1)
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
    
  )


### convenient for plotting
nice_theme = function() {
  theme_test() +
    theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1.2)))
}

pct_labels = function(x) { paste0(x*100,"%")}


robust_glm = function(formula, cluster, dataset) {
  
  cluster = eval(substitute(cluster), dataset)
  
  rms::lrm(
    data=dataset
    , formula = formula
    , x=T, y=T
  ) %>%
    rms::robcov(cluster=cluster)
  
}

ag_rep = subset(aggreg, analysis=="replication") %>%
  ungroup %>%
  mutate(
    task = as.character(task)
  )

d_rep = subset(d, analysis=="replication") %>%
  ungroup %>%
  mutate(
    task = as.character(task)
  )


ag_gurc = subset(aggreg, analysis=="reanalysis" & communication=="Discussion")


tidy_table <- function(df, name1, name2){
  table(eval(substitute(name1), df),
        eval(substitute(name2), df)
  )
}


tidy_table_univariate <- function(df, name){
  table(eval(substitute(name), df))
}

