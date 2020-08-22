#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
#+ Load packages -------------------
#' # Load packages and data sets
library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(reshape2)
library(knitr)
library(tidyr)
library(e1071)
library(lme4)
library(lmerTest)
library(jmv)
Ralpha <- function(n,alpha=0.05){
  ta <- qt(alpha/2,n-2)
  Rs <-ta^2/(n-2+ta^2)
  sqrt(Rs)
}

corMat <- function(dt.target){
  cm.raw=cor(dt.target, use = "pairwise.complete.obs")
  cm=cor(dt.target, use = "pairwise.complete.obs") %>% round(2)
  n=nrow(dt.target)
  for (i in 1:nrow(cm)){
    for (j in 1:ncol(cm)){
      a=cm[i,j]
      if ((!is.na(cm.raw[i,j])) && cm.raw[i,j]>Ralpha(n,0.05)){
        a=paste(cm[i,j],"*",sep = "")
      }
      if ((!is.na(cm.raw[i,j])) && cm.raw[i,j]>Ralpha(n,0.01)){
        a=paste(cm[i,j],"**",sep = "")
      }
      if ((!is.na(cm.raw[i,j])) && cm.raw[i,j]>Ralpha(n,0.001)){
        a=paste(cm[i,j],"***",sep = "")
      }
      if (j>=i){
        a=""
      }
      cm[i,j]=a
    }
  }
  cm
}

'%ni%' <- Negate('%in%')
#+ Accuracy -------------------
#' #  Accuracy
dt=read_csv("ACC_data.csv")
dt=subset(dt,exp!="prac")
#dt=subset(dt,con=="causal"|con=="non_causal")
dt=subset(dt,con=="MIN"|con=="SNT")
subdelete=c("3UN61F00HXSHLWOFO8OUN0C8WIYR5Q",
            "3OONKJ5DKDMGDZD8XQY3WR7VVR1BO3")
dt=subset(dt, dat_Subject %ni% subdelete)
a=unique(dt$dat_Subject)
#' ##  Overall Accuracy
dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con")) %>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))%>%
  ggplot(aes(x=con,y=dat_Accuracy,fill=con))+
    geom_bar(position=position_dodge(),
             stat="identity")+
    geom_errorbar(aes(ymin=dat_Accuracy-se, ymax=dat_Accuracy+se),
                  width=.2, 
                  position=position_dodge(.9))+
    ggtitle("ACC")

dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject"))%>%
  ggplot(aes(x=dat_Subject,y=dat_Accuracy))+
    geom_bar(position=position_dodge(),
             stat="identity")+
    ggtitle("ACC")+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))

dt%>%summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))

dt.pair=dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_Accuracy")

t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
lmer(dat_Accuracy ~ con +(1 |dat_Subject) +(1|id), data = dt) %>% summary()


dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  ggplot(aes(x=dat_Subject,y=dat_Accuracy,fill=con))+
    geom_bar(position=position_dodge(),
             stat="identity")+
    ggtitle("ACC")+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))

dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("id"))%>%
  ggplot(aes(x=id,y=dat_Accuracy))+
    geom_bar(position=position_dodge(),
             stat="identity")+
    ggtitle("ACC")+
    scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))

dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("id","con"))%>%
  ggplot(aes(x=id,y=dat_Accuracy,fill=con))+
    geom_bar(position=position_dodge(),
             stat="identity")+
    ggtitle("ACC")


#+ Online Measures -------------------
#' #  Online Measures
#dt=read_csv("Area_data.csv") 
dt=read_csv("window_data.csv") 
dt$interest=dt$dat_AreaSum-dt$dat_AreaPosition
#dt=subset(dt,con=="causal"|con=="non_causal")
dt=subset(dt,con=="MIN"|con=="SNT")
dt=subset(dt, dat_Subject %ni% subdelete)
dt=subset(dt,dat_LoopNumber==2)

#' ## Regression Out
#' ### Probability of Regression Out
dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaPosition","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==1)%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRO")

dt.pair$MIN=as.numeric(as.logical(dt.pair$MIN))
dt.pair$SNT=as.numeric(as.logical(dt.pair$SNT))

ttestPS(dt.pair, pairs = list(list(i1 = 'MIN', i2 = 'SNT')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")

dt.pair=dt%>%
  subset(dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRO")


dt.pair$MIN=as.numeric(as.logical(dt.pair$MIN))
dt.pair$SNT=as.numeric(as.logical(dt.pair$SNT))

ttestPS(dt.pair, pairs = list(list(i1 = 'MIN', i2 = 'SNT')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")


#' ## Regression In
#' ### Probability of Regression In
dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==1)%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRI")

dt.pair$MIN=as.numeric(as.logical(dt.pair$MIN))
dt.pair$SNT=as.numeric(as.logical(dt.pair$SNT))

ttestPS(dt.pair, pairs = list(list(i1 = 'MIN', i2 = 'SNT')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")


dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRI")

dt.pair$MIN=as.numeric(as.logical(dt.pair$MIN))
dt.pair$SNT=as.numeric(as.logical(dt.pair$SNT))

ttestPS(dt.pair, pairs = list(list(i1 = 'MIN', i2 = 'SNT')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")

#' ## Regression Path Duration
dt$mean_RPD=dt$dat_RPD
dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==1)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RPD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset(dat_AreaPosition==1)%>%
  subset(dat_AreaNumber==2)
lmer(mean_RPD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()
#lme
dt.lme=dt.final%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==1)
lmer(mean_RPD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RPD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)


dt.pair=dt%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==2|dat_AreaNumber==1)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_Subject","con","dat_AreaNumber"))%>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_RPD")

skewness(log(dt.pair$SNT_1))
skewness(log(dt.pair$SNT_2))
skewness(log(dt.pair$MIN_1))
skewness(log(dt.pair$MIN_2))

write.csv(dt.pair,"interation.csv")


#' ## Total Reading Time
dt$mean_TRT=dt$dat_TRT

dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_TRT")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_TRT")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset(dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)
lmer(mean_TRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()
#lme
dt.lme=dt.final%>%
  subset(interest==2)%>%
  subset(dat_AreaNumber==2)
lmer(mean_TRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()


#' ## Rereading Time
dt$mean_RRT=dt$dat_RRT

dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==1)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RRT")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

dt.pair=dt%>%
  subset(dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RRT")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)
#' ## Gaze Duration
dt$mean_GD=dt$dat_GD

dt.final=dt
dt.se=dt.final%>%subset(interest==0|interest==1|interest==2)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","dat_Subject","interest"))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","interest"))

dt.se=dt.final%>%subset(dat_AreaPosition==1|dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(interest==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_GD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

dt.pair=dt%>%
  subset(dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_GD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

dt.pair=dt%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==2|dat_AreaNumber==1)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_Subject","con","dat_AreaNumber"))%>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_GD")

skewness(log(dt.pair$SNT_1))
skewness(log(dt.pair$SNT_2))
skewness(log(dt.pair$MIN_1))
skewness(log(dt.pair$MIN_2))

#lme
dt.lme=dt.final%>%
  subset(dat_AreaPosition==2)%>%
  subset(dat_AreaNumber==2)
lmer(mean_GD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()
#lme
dt.lme=dt.final%>%
  subset(interest==0)%>%
  subset(dat_AreaNumber==1)
lmer(mean_GD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

# write.csv(dt.pair,"interation_GD.csv")
