library(data.table)
library(dplyr)
library(lubridate)
library(caret)
library(gbm)
library(glmnet)
library(rattle)
library(tidyr)
library(ModelMetrics)
library(randomForest)
library(e1071)
library(rattle)

matches = read.table('matches.csv', header=TRUE, sep=',')
matches$match_hometeam_name=ifelse(matches$match_hometeam_name=="Manchester United","Manchester Utd",as.character(matches$match_hometeam_name))
matches$match_awayteam_name=ifelse(matches$match_awayteam_name=="Manchester United","Manchester Utd",as.character(matches$match_awayteam_name))
matches$match_hometeam_name=ifelse(matches$match_hometeam_name=="Aston Villa (Eng)","Aston Villa",as.character(matches$match_hometeam_name))
matches$match_awayteam_name=ifelse(matches$match_awayteam_name=="Aston Villa (Eng)","Aston Villa",as.character(matches$match_awayteam_name))
matches$match_hometeam_name=ifelse(matches$match_hometeam_name=="Newcastle Utd","Newcastle",as.character(matches$match_hometeam_name))
matches$match_awayteam_name=ifelse(matches$match_awayteam_name=="Newcastle Utd","Newcastle",as.character(matches$match_awayteam_name))
matches$match_hometeam_name=ifelse(matches$match_hometeam_name=="West Ham (Eng)","West Ham",as.character(matches$match_hometeam_name))
matches$match_awayteam_name=ifelse(matches$match_awayteam_name=="West Ham (Eng)","West Ham",as.character(matches$match_awayteam_name))

epl=filter(matches, league_id=="148")
epl=mutate(epl,epoch=as_datetime(epl$epoch))
final_data=epl[,c(2,1,4,5,3,7,8,9,10)]

finish=filter(final_data,match_status=='Finished')
#setting lose,draw,win as 1 0 
for(i in 1:length(finish$match_hometeam_score)){
  if(finish$match_hometeam_score[i]>finish$match_awayteam_score[i]){  
    finish$win[i]=1
    finish$lose[i]=0
    finish$draw[i]=0
  }
  if(finish$match_awayteam_score[i]>finish$match_hometeam_score[i]){  
    finish$lose[i]=1
    finish$win[i]=0
    finish$draw[i]=0
  }
  if(finish$match_hometeam_score[i]==finish$match_awayteam_score[i]){ 
    finish$draw[i]=1
    finish$win[i]=0
    finish$lose[i]=0
  }
}

# USING STATS DATA
stats = read.table('stats.csv', header=TRUE, sep=',')
stats$home_BallPossession=gsub( "%", "", as.character(stats$home_BallPossession))
stats$away_BallPossession=gsub( "%", "", as.character(stats$away_BallPossession))
stats_used=stats[,c(1,2,5,6,11,14,15,20,21)]

stats_used$ home_BallPossession=as.numeric(stats_used$ home_BallPossession)
stats_used$ away_BallPossession=as.numeric(stats_used$ away_BallPossession)

#Merging stats data with matches
sts=finish
stcol=ncol(sts)+1
for (i in 1:nrow(sts)){  
  for (j in 1: nrow(stats_used)){   
    if (sts$match_id[i]==stats_used$match_id[j]){
      sts[i,stcol]=stats_used[j,2] # or k, k+3 from 2 to 9
      sts[i,stcol+1]=stats_used[j,3]
      sts[i,stcol+2]=stats_used[j,4]
      sts[i,stcol+3]=stats_used[j,5]
      sts[i,stcol+4]=stats_used[j,6]
      sts[i,stcol+5]=stats_used[j,7]
      sts[i,stcol+6]=stats_used[j,8]
      sts[i,stcol+7]=stats_used[j,9]
    } 
  } 
} 

colnames_stats=colnames(stats_used)
colnames(sts)[13:20]<-c(colnames_stats[2],colnames_stats[3],colnames_stats[4],colnames_stats[5],colnames_stats[6],colnames_stats[7],colnames_stats[8],colnames_stats[9])

# USING BOOKING DATA 

book = read.table('booking.csv', header=TRUE, sep=',')
reds=book%>% filter(card=='red card')

home_Red=data.frame(reds[,c(1,3,4)],stringsAsFactors = FALSE)
home_Red$home_fault=as.character(home_Red$home_fault)
home_Red$card=as.integer(home_Red$card)
home_Red$home_fault[home_Red$home_fault==""] <- "NA"
home_Red=filter(home_Red,home_Red$home_fault!="NA")
reds_home<- home_Red%>%group_by(match_id)%>%summarize(reds_home=sum(card))

away_Red=reds[,c(1,5,4)]
away_Red$away_fault=as.character(away_Red$away_fault)
away_Red$card=as.integer(away_Red$card)
away_Red$away_fault[away_Red$away_fault==""] <- "NA"
away_Red=filter(away_Red,away_Red$away_fault!="NA")
reds_away<- away_Red%>%group_by(match_id)%>%summarize(reds_away=sum(card))

#Merging booking data with matches
new=sts
new$red_away=rep(0,nrow(new))
for (i in 1:nrow(new)){  
  for (j in 1: nrow(reds_away)){   
    if (new$match_id[i]==reds_away$match_id[j]){new$red_away[i]=reds_away$reds_away[j]
    }}}


new$red_home=rep(0,nrow(new))
for (i in 1:nrow(new)){  
  for (j in 1: nrow(reds_home)){   
    if (new$match_id[i]==reds_home$match_id[j]){new$red_home[i]=reds_home$reds_home[j]
    }}}

data=new[,-c(3,4)]


#Giving data its last shape combining historical data for each team, till here everything was match based

data<-data%>%group_by(match_hometeam_id,match_awayteam_id)%>%summarise(home_goal=sum(match_hometeam_score),away_goal=sum(match_awayteam_score),
                                                                       no_win=sum(win),no_lose=sum(lose),no_draw=sum(draw),
                                                                       home_possession=mean(home_BallPossession),away_possession=mean(away_BallPossession),
                                                                       home_keeper=sum(home_GoalkeeperSaves),away_keeper=sum(away_GoalkeeperSaves),
                                                                       home_attempt=sum(home_GoalAttempts),away_attempt=sum(away_GoalAttempts),
                                                                       home_block=sum(home_BlockedShots),away_block=sum(away_BlockedShots),
                                                                       red_home=sum(red_home),red_away=sum(red_away))



merg=final_data[,c(5,1,2,8,9)]

col=ncol(merg)+1
for (i in 1:nrow(merg)){  
  for (j in 1: nrow(data)){   
    if (merg$match_hometeam_id[i]==data$match_hometeam_id[j] && merg$match_awayteam_id[i]==data$match_awayteam_id[j] ){
      merg[i,col]=data[j,3] # or k, k+3 from 2 to 9
      merg[i,col+1]=data[j,4]
      merg[i,col+2]=data[j,5]
      merg[i,col+3]=data[j,6]
      merg[i,col+4]=data[j,7]
      merg[i,col+5]=data[j,8]
      merg[i,col+6]=data[j,9]
      merg[i,col+7]=data[j,10]
      merg[i,col+8]=data[j,11]
      merg[i,col+9]=data[j,12]
      merg[i,col+10]=data[j,13]
      merg[i,col+11]=data[j,14]
      merg[i,col+12]=data[j,15]
      merg[i,col+13]=data[j,16]
      merg[i,col+14]=data[j,17]
      
    } 
  } }

dat_y=merg[,c(1,4,5)]
dat_x=merg[,-c(4,5)]

y<- as.data.frame(lapply(dat_y, function(x) ifelse(is.na(x), 'empty', x)))
x<- as.data.frame(lapply(dat_x, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
data_All<-merge(x,y,by="match_id")

data_al=filter(data_All,data_All$match_hometeam_score!='empty')
result=ifelse(as.numeric(data_al[,19])>as.numeric(data_al[,20]),yes='home',no=ifelse(as.numeric(data_al[,19])<as.numeric(data_al[,20]),yes='away',no='draw'))
data_all=cbind(data_al[,1:18],result)

#RPS CALCULATION FUNCTIONS 

RPS_matrix<- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS)
}

rps_calc=function(data,colbegin)
{
  outcomes=matrix(0,nrow(data),3)
  for(i in 1:nrow(data))
  {
    if(test$result[i]=='home')
    {
      outcomes[i,1]=1
    }
    if(test$result[i]=='draw')
    {
      outcomes[i,2]=1
    }
    if(test$result[i]=='away')
    {
      outcomes[i,3]=1
    }
  }
  RPS_TEST=RPS_matrix(data,outcomes)
  return(RPS_TEST)
}




set.seed(123)

smp_siz = floor(0.8*nrow(data_all))

train_ind = sample(seq_len(nrow(data_all)),size = smp_siz)
train =data_all[train_ind,]
test=data_all[-train_ind,]

# SGB FITTING
fit_gbm<-train(train[,-c(1,19)],train[,19],method='gbm',trControl=trainControl(method = 'cv'))

fit_gbm$bestTune

estimate_gbm=predict(fit_gbm,test[,-c(1,19)],n.trees = 50)

gbm_prob=predict(fit_gbm,test[,-c(1,19)],n.trees = 50,type="prob")

prob_gbm=cbind(prob=gbm_prob,real=test[,19])

#RESULT OF PREDICTIONS AND REAL 
head(prob_gbm)

#RPS calculation for GBM

probs_gbm=cbind(home=as.numeric(gbm_prob[,1]),draw=as.numeric(gbm_prob[,2]),away=as.numeric(gbm_prob[,3]))
RPS_gbm=rps_calc(probs_gbm,3)
mean(RPS_gbm)

#For Accuracy
compare_gbm=cbind(estimate_gbm,test[,19])
COUNT=0
for (i in 1:nrow(compare_gbm)){if (compare_gbm[i,1]!=compare_gbm[i,2]){
  COUNT=COUNT +1
  COUNT
}}
error_gbm=COUNT/nrow(compare_gbm)

# RF FITTING
fit_rf<-train(train[,-c(1,19)],train[,19],method='rf',trControl=trainControl(method = 'cv'))

fit_rf$bestTune

estimate_rf=predict(fit_rf,test[,-c(1,19)],mtry=2)
rf_prob=predict(fit_rf,test[,-c(1,19)],mytry=2,type="prob")

prob_rf=cbind(prob=rf_prob,real=test[,19])
head(prob_rf)

#RPS calculation for RF

probs_rf=cbind(home=as.numeric(rf_prob[,1]),draw=as.numeric(rf_prob[,2]),away=as.numeric(rf_prob[,3]))
RPS_rf=rps_calc(probs_rf,3)
mean(RPS_rf)

#For Accuracy

compare_rf=cbind(estimate_rf,test[,19])
COUNT=0
for (i in 1:nrow(compare_rf)){if (compare_rf[i,1]!=compare_rf[i,2]){
  COUNT=COUNT +1
  COUNT
}}
error_rf=COUNT/nrow(compare_rf)

# LogR FITTING
x_train=train[,-c(1,19)]
y_train=train[,c(19)]
x_test=test[,-c(1,19)]
y_test=test[,c(19)]

x_train_scale<-scale(x_train, center = TRUE, scale = TRUE)
x_test_scale<-scale(x_test, center = TRUE, scale = TRUE)

fit_logR <- cv.glmnet(x_train_scale,y_train, alpha = 1,type.measure="class",family="multinomial")

estimate_logR=predict(fit_logR,x_test_scale,s=fit_logR$lambda.1se,type="class")

logR_prob=predict(fit_logR,x_test_scale,s=fit_logR$lambda.1se,type="response")

prob_logR=cbind(prob=logR_prob[, ,1],real=as.character(y_test))
head(prob_logR)

#RPS calculation for LogR
probs_logR=cbind(home=as.numeric(logR_prob[,1,]),draw=as.numeric(logR_prob[,2,]),away=as.numeric(logR_prob[,3,]))
RPS_LogR=rps_calc(probs_logR,3)
mean(RPS_LogR)

#For Accuracy
compare_logR=cbind(estimate_logR,as.character(y_test))
COUNT=0
for (i in 1:nrow(compare_logR)){if (compare_logR[i,1]!=compare_logR[i,2]){
  COUNT=COUNT +1
  COUNT
}}
error_logR=COUNT/nrow(compare_logR)
error_logR
prob_logR[,1]<-format(prob_logR[1,1,1], digits=3)
prob_logR[,2]<-format(prob_logR[,2], digits=1)
prob_logR[,3]<-format(prob_logR[,3], digits=1)

head(prob_logR)


## POISSON REGRESSION 

train_pos =data_al[train_ind,]
test_pos=data_al[-train_ind,]

poisson_model <- rbind(data.frame(goals=train_pos$match_hometeam_score,team=train_pos$match_hometeam_id, opponent=train_pos$match_awayteam_id,home=1),
                       data.frame(goals=train_pos$match_awayteam_score,team=train_pos$match_awayteam_id, opponent=train_pos$match_hometeam_id,home=0)) 

poisson_model$goals<-as.numeric(poisson_model$goals)
poisson_model$team<-as.factor(poisson_model$team)
poisson_model$opponent<-as.factor(poisson_model$opponent)
poisson_reg=poisson_model%>% glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)

result_match <- function(model, home, away, max_goals=10){
  home_goals<- predict(model,data.frame(home=1, team=home, opponent=away), type="response")
  away_goals <- predict(model,data.frame(home=0, team=away,opponent=home), type="response")
  
  dpois(0:max_goals, home_goals) %o% dpois(0:max_goals, away_goals) 
}


home=0
draw=0
away=0
for(i in 1:nrow(test)){
  match <- result_match(poisson_reg,  as.factor(test_pos[i,2]),as.factor(test_pos[i,3]), max_goals=10)
  # home win
  home[[i]]=sum(match[lower.tri(match)])
  # draw
  draw[[i]]=sum(diag(match))
  # away win
  away[[i]]=sum(match[upper.tri(match)])
}

prob_poisson=matrix(0,nrow(test),3)
prob_poisson=cbind(home,draw,away,real=as.character(y_test))
View(prob_poisson)

# PRS Calculation for poisson
probs_pos=prob_poisson[,c(1,2,3)]
probs_pos=as.matrix(probs_pos)
probs_pos=cbind(home=as.numeric(probs_pos[,1]),draw=as.numeric(probs_pos[,2]),away=as.numeric(probs_pos[,3]))
RPS_poisson=rps_calc(probs_pos,3)
mean(RPS_poisson)


#Accuracy for Poisson
result_poisson=ifelse(as.numeric(prob_poisson[,1])>as.numeric(prob_poisson[,3]), yes='home', no=ifelse(as.numeric(prob_poisson[,1]<prob_poisson[,3]),yes='away',no='draw'))
compare_poisson=cbind(result_poisson,real=as.character(y_test))

COUNT=0
for (i in 1:nrow(compare_poisson)){if (compare_poisson[i,1]!=compare_poisson[i,2]){
  COUNT=COUNT +1
  COUNT
}}
error_poisson=COUNT/nrow(compare_poisson)

error_poisson


###########SUMMARY OF THE RESULTS

table<-array(numeric(),c(4,3))
table<-data.frame(table)
colnames(table)[1] <- "Summary"
colnames(table)[2] <- "RPS"
names(table)[3] <- "Error"

table[1,]<-c('GBM',mean(RPS_gbm),error_gbm)
table[2,]<-c('RF',mean(RPS_rf),error_rf)
table[3,]<-c('LogR',mean(RPS_LogR),error_logR)
table[4,]<-c('Poisson',mean(RPS_poisson),error_poisson)
table[,2]<-as.numeric(table[,2])
table[,3]<-as.numeric(table[,3])
table[,2]<-format(table[,2], digits=2)
table[,3]<-format(table[,3], digits=2)
