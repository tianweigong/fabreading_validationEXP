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

dt%>%summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))

dt.pair=dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_Accuracy")

t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
glmer(dat_Accuracy ~ con +(1 |dat_Subject) +(1|id),family = "binomial", data = dt) %>% summary()

#+ Online Measures -------------------
#' #  Online Measures
#dt=read_csv("Area_data.csv") 
dt=read_csv("window_data.csv") 
dt$interest=dt$dat_AreaSum-dt$dat_AreaPosition
#dt=subset(dt,con=="causal"|con=="non_causal")
dt=subset(dt,con=="MIN"|con=="SNT")
dt=subset(dt, dat_Subject %ni% subdelete)
dt=subset(dt,dat_LoopNumber==2)

blis=c("b1","b1p1","b1p2","b2m2","b2m1","b2")
dt$bound=NA
dt$bound[intersect(which(dt$dat_AreaNumber==1),which(dt$interest==0))]="b1"
dt$bound[intersect(which(dt$dat_AreaNumber==2),which(dt$dat_AreaPosition==1))]="b1p1"
dt$bound[intersect(which(dt$dat_AreaNumber==2),which(dt$dat_AreaPosition==2))]="b1p2"
dt$bound[intersect(which(dt$dat_AreaNumber==2),which(dt$interest==2))]="b2m2"
dt$bound[intersect(which(dt$dat_AreaNumber==2),which(dt$interest==1))]="b2m1"
dt$bound[intersect(which(dt$dat_AreaNumber==2),which(dt$interest==0))]="b2"

dt$mean_RPD=dt$dat_RPD
dt$mean_GD=dt$dat_GD
dt$mean_RRT=dt$dat_RRT
dt$mean_TRT=dt$dat_TRT

dt.final=dt

#' ## Regression Out
#' ### Probability of Regression Out
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("bound","con"))

dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
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
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("bound","con"))

dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_PRI")

dt.pair$MIN=as.numeric(as.logical(dt.pair$MIN))
dt.pair$SNT=as.numeric(as.logical(dt.pair$SNT))

ttestPS(dt.pair, pairs = list(list(i1 = 'MIN', i2 = 'SNT')), students = FALSE, bf = FALSE, bfPrior = 0.707,
        wilcoxon = TRUE, hypothesis = "different", norm = FALSE,
        qq = FALSE, meanDiff = FALSE, effectSize = FALSE, ci = FALSE,
        ciWidth = 95, desc = FALSE, plots = FALSE, miss = "perAnalysis")

#' ## Regression Path Duration
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("bound","con"))

dt.se%>%
  mutate(bound=factor(bound,level=c("b1","b1p1","b1p2","b2m2","b2m1","b2"),
                      labels=c("B1","B1+1","B1+2","B2-2","B2-1","B2")),
         con=factor(con,levels = c("MIN","SNT"),labels = c("Unmarked","Marked")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=bound,y=mean_RPD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_RPD-se, ymax=mean_RPD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("Regression Path Duration")+
  xlab("Boundary")+
  scale_y_continuous(limits = c(0,510))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

ggsave(file="exp2_RPD.pdf",width =4,height = 3)

dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
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
  subset(bound=="b1")
lmer(mean_RPD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()


#' ## Total Reading Time
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("bound","con"))

dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
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
  subset(bound=="b1")
lmer(mean_TRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

#' ## Rereading Time
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("bound","con"))

dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_RRT")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#' ## Gaze Duration
dt.se=dt.final%>%subset(bound %in% blis)%>%
  summarySE(measurevar = "mean_GD",groupvars = c("bound","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("bound","con"))

dt.se%>%
  mutate(bound=factor(bound,level=c("b1","b1p1","b1p2","b2m2","b2m1","b2"),
                      labels=c("B1","B1+1","B1+2","B2-2","B2-1","B2")),
         con=factor(con,levels = c("MIN","SNT"),labels = c("Unmarked","Marked")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=bound,y=mean_GD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_GD-se, ymax=mean_GD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("First-pass Reading Time")+
  xlab("Boundary")+
  scale_y_continuous(limits = c(0,510))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

ggsave(file="exp2_GD.pdf",width =4,height = 3)


dt.pair=dt%>%
  subset(bound=="b1")%>% #switch between different boundaries in blis
  summarySE(measurevar = "mean_GD",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "mean_GD")

skewness(dt.pair$MIN)
skewness(dt.pair$SNT)

skewness(log(dt.pair$MIN))
skewness(log(dt.pair$SNT))

t.test(log(dt.pair$MIN),log(dt.pair$SNT),paired = TRUE)
t.test(dt.pair$MIN,dt.pair$SNT,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset(bound=="b1")
lmer(mean_GD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()