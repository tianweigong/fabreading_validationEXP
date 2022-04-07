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
library(effsize)
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
dt=subset(dt,con=="causal"|con=="non_causal")
# dt=subset(dt,con=="MIN"|con=="SNT")
subdelete=c("37ZHEEHM6XP4PYHW1B2V3MNOHPO37I",
            "3HYA4D452SMSJ90JFUM284VD9Z9F2P",
            "3I0BTBYZAYORNQE05XACE19EICCY0I",
            "3PB5A5BD0W95JATKWQ9FMGCXLUT7GU",
            "3XUHV3NRVL14W2ACS3C09WA294ZH58",
            "3S96KQ6I9N7P5OG073B8LR1IXZ8DT6",
            "392CY0QWG2UMWEHWKGBB4G5395UI4C")
dt=subset(dt, dat_Subject %ni% subdelete)
a=unique(dt$dat_Subject)
#' ##  Overall Accuracy

dt%>%summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))

dt.pair=dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_Accuracy")

t.test(dt.pair$causal,dt.pair$non_causal,paired = TRUE)
cohen.d(d=c(dt.pair$causal,dt.pair$non_causal), rep(c("Treatment","Control"),each=nrow(dt.pair)),paired = TRUE)

#lme
glmer(dat_Accuracy ~ con +(1 |dat_Subject) +(1|id),family = "binomial", data = dt) %>% summary()


#+ Online Measures -------------------
#' #  Online Measures
dt=read_csv("window_data.csv") 
#dt=read_csv("window_data.csv") 
#dt$interest=dt$dat_AreaSum-dt$dat_AreaPosition
dt=subset(dt,con=="causal"|con=="non_causal")
# dt=subset(dt,con=="MIN"|con=="SNT")
dt=subset(dt, dat_Subject %ni% subdelete)
dt=subset(dt,dat_LoopNumber==2)

dt$mean_GD=dt$dat_GD
dt$mean_RRT=dt$dat_RRT
dt$mean_TRT=dt$dat_TRT
dt$mean_RPD=dt$dat_RPD

dt.final=dt

#' ## Regression Out
#' ### Probability of Regression Out

dt.se=dt.final%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaPosition","con"))


dt.pair=dt%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRO")

ttestPS(dt.pair, pairs = list(list(i1 = 'causal', i2 = 'non_causal')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")

#' ## Regression In
#' ### Probability of Regression In
dt.se=dt.final%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaPosition","con"))


dt.pair=dt%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRI")

ttestPS(dt.pair, pairs = list(list(i1 = 'causal', i2 = 'non_causal')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")

#' ## Regression Path Duration
dt.se=dt.final%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaPosition","con"))

dt.se%>%
  mutate(dat_AreaPosition=factor(dat_AreaPosition,level=c(1:2),labels=c("W1","W2")),
         con=factor(con,levels = c("causal","non_causal"),labels = c("Causal","Non-causal")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=dat_AreaPosition,y=mean_RPD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_RPD-se, ymax=mean_RPD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("Regression Path Duration")+
  xlab("Window")+
  scale_y_continuous(limits = c(0,3100))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

ggsave(file="exp3_RPD.pdf",width =4,height = 3)


dt.pair=dt%>%
  subset(dat_AreaPosition==2)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RPD")

skewness(dt.pair$causal)
skewness(dt.pair$non_causal)

skewness(log(dt.pair$causal))
skewness(log(dt.pair$non_causal))

t.test(log(dt.pair$causal),log(dt.pair$non_causal),paired = TRUE)
# t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset(dat_AreaPosition==1)
lmer(mean_RPD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

#' ## Total Reading Time
dt.se=dt.final%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_TRT")



t.test(log(dt.pair$causal),log(dt.pair$non_causal),paired = TRUE)
# t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset(dat_AreaPosition==2)
lmer(mean_TRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

#' ## Rereading Time
dt.se=dt.final%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaPosition","con"))

dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RRT")


#' ## Gaze Duration
dt.se=dt.final%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaPosition","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaPosition","con"))


dt.se%>%
  mutate(dat_AreaPosition=factor(dat_AreaPosition,level=c(1:2),labels=c("W1","W2")),
         con=factor(con,levels = c("causal","non_causal"),labels = c("Causal","Non-causal")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=dat_AreaPosition,y=mean_GD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_GD-se, ymax=mean_GD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("First-pass Reading Time")+
  xlab("Window")+
  scale_y_continuous(limits = c(0,3100))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

ggsave(file="exp3_GD.pdf",width =4,height = 3)



dt.pair=dt%>%
  subset(dat_AreaPosition==1)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_GD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)
