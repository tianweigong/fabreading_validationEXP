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
library(psy)
library(lme4)
library(lmerTest)
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

#+ Accuracy -------------------
#' #  Accuracy
dt=read_csv("ACC_data.csv")

#' ##  Target Accuracy
dt=subset(dt,con=="G"|con=="C")

dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))

dt.pair=dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%
  dcast(dat_Subject~con,value.var = "dat_Accuracy")

#t-test
skewness(dt.pair$G)
skewness(dt.pair$C)
t.test(dt.pair$C,dt.pair$G,paired = TRUE)
cohen.d(d=c(dt.pair$C,dt.pair$G), rep(c("Treatment","Control"),each=nrow(dt.pair)),paired = TRUE)
#lme
glmer(dat_Accuracy ~ con +(1 |dat_Subject) +(1|id), family = "binomial",data = dt) %>% summary()

#' ##  Target Accuracy & English Ability
dt.sub=read_csv("English_ability.csv")

skewness(dt.sub$ofscore)
skewness(dt.sub$selfreport)
skewness(dt.sub$age)
skewness(dt.sub$reading)
skewness(dt.sub$writing)
skewness(dt.sub$speaking)
skewness(dt.sub$listening)

dt.acc.cor=subset(dt.sub,select = c(age,female,starting,selfreport,ofscore,reading,writing,speaking,listening));
dt.se=summarySE(dt,measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%subset(con=="C")
dt.acc.cor$C_ACC=dt.se$dat_Accuracy
dt.se=summarySE(dt,measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con"))%>%subset(con=="G")
dt.acc.cor$G_ACC=dt.se$dat_Accuracy
dt.acc.cor$GC_ACC=dt.acc.cor$G_ACC-dt.acc.cor$C_ACC
corMat(dt.acc.cor)%>% kable()

cor.test(dt.acc.cor$ofscore,dt.acc.cor$G_ACC)
cor.test(dt.acc.cor$ofscore,dt.acc.cor$C_ACC)
a=data.frame(dt.sub$listening,dt.sub$speaking,dt.sub$writing,dt.sub$reading)
cronbach(a)$alpha 
#+ Online Measures -------------------
#' #  Online Measures
dt=read_csv("Area_data.csv")
dt=subset(dt,con=="G"|con=="C")
dt=subset(dt,dat_LoopNumber==2)
dt.rt.cor=subset(dt.sub,select = c(age,female,starting,selfreport,ofscore,reading,writing,speaking,listening))

dt$mean_RPD=dt$dat_RPD/dt$dat_AreaSum
dt$mean_TRT=dt$dat_TRT/dt$dat_AreaSum
dt$mean_GD=dt$dat_GD/dt$dat_AreaSum
dt$mean_RRT=dt$dat_RRT/dt$dat_AreaSum
dt.final=dt

#' ## Regression Out
#' ### Probability of Regression Out
dt.se=summarySE(dt.final,measurevar = "dat_PRO",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "dat_PRO")

dt.se$dat_Subject=NULL;
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()
qplot(dt.rt.cor$ofscore,dt.se$GC_3,main="Prob of regression out",xlab = "oxford",ylab = "Garden minus Control: Area 3")

cor.test(dt.rt.cor$ofscore,dt.se$GC_4)

dt.pair=dt.final%>%
  summarySE(measurevar = "dat_PRO",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==5 & con=="C")|(dat_AreaNumber==4 & con=="G") )%>%
  dcast(dat_Subject~con,value.var = "dat_PRO")

skewness(dt.pair$C)
skewness(dt.pair$G)

t.test(dt.pair$G,dt.pair$C,paired = TRUE)
cohen.d(d=c(dt.pair$C,dt.pair$G), rep(c("Treatment","Control"),each=nrow(dt.pair)),paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==5 & con=="C")|(dat_AreaNumber==4 & con=="G"))
lmer(dat_PRO ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

pic.of$index2=dt.se$GC_4

p=data.frame(dt.rt.cor$ofscore,dt.se$G_4)
p_sub=p %>% subset(dt.rt.cor.ofscore>0.776) 
mean(p_sub$dt.se.G_4)
sd(p_sub$dt.se.G_4)

#' ## Regression In
#' ### Probability of Regression In
dt.pair=dt.final%>%
  summarySE(measurevar = "dat_PRI",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==3 & con=="C")|(dat_AreaNumber==2 & con=="G") )%>%
  dcast(dat_Subject~con,value.var = "dat_PRI")

t.test(dt.pair$G,dt.pair$C,paired = TRUE)
cohen.d(d=c(dt.pair$C,dt.pair$G), rep(c("Treatment","Control"),each=nrow(dt.pair)),paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==1 & con=="C")|(dat_AreaNumber==1 & con=="G"))
lmer(dat_PRI ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

dt.se=summarySE(dt.final,measurevar = "dat_PRI",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "dat_PRI")
dt.se$dat_Subject=NULL;
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()
qplot(dt.rt.cor$ofscore,dt.se$GC_3,main="Prob of regression in",xlab = "oxford",ylab = "Garden minus Control: Area 3")

cor.test(dt.rt.cor$ofscore,dt.se$GC_1)
pic.of$index3=dt.se$GC_1

p=data.frame(dt.rt.cor$ofscore,dt.se$G_1)
p_sub=p %>% subset(dt.rt.cor.ofscore>0.776) 
mean(p_sub$dt.se.G_1)
sd(p_sub$dt.se.G_1)

pic.of.final=melt(pic.of, id.vars = "OPT", measure.vars = c("index1", "index2","index3"))
pic.of.final$cor=NA
for (i in 1:nrow(pic.of.final)){
  if (pic.of.final$variable[i]=="index1"){
    pic.of.final$cor[i]="r = .52"
  }
  if (pic.of.final$variable[i]=="index2"){
    pic.of.final$cor[i]="r = .48"
  }
  if (pic.of.final$variable[i]=="index3"){
    pic.of.final$cor[i]="r = .51"
  }
}

pic.label=c("index1"="Rereading Time: Region 5","index2"="Probability of Regression Out: Region 5","index3"="Probability of Regression In: Region 1")

ggplot(data=pic.of.final)+
  geom_point(aes(x=OPT,y=value),color="black")+
  geom_smooth(method='lm',aes(x=OPT,y=value),se=F,color="#8c8c8c")+
  facet_wrap(~variable,nrow=1,scales = "free",labeller = as_labeller(pic.label))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_text(data=pic.of.final, aes(fontface=3,label=cor, size=1),colour="black", x=0.56, y=Inf, vjust=1.5, show_guide=FALSE)+
  ylab("Control minus Garden-path")+
  xlab("OPT Score")

mean(pic.of.final$OPT)
ggsave(filename = "opt.pdf",width=9, height=3)


#' ## Regression Path Duration
dt.se=dt.final%>% 
  mutate(dat_AreaNumber=ifelse((con=="G"&dat_AreaNumber!=1),dat_AreaNumber+1,dat_AreaNumber))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con"))

dt.se%>%
  subset(dat_AreaNumber!=2)%>%
  mutate(dat_AreaNumber=factor(dat_AreaNumber,level=c(1,3,4,5),labels=c("R1","R3","R4","R5")),
         con=factor(con,levels = c("C","G"),labels = c("Control","Garden-path")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=dat_AreaNumber,y=mean_RPD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_RPD-se, ymax=mean_RPD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("Regression Path Duration")+
  xlab("Region")+
  scale_y_continuous(limits = c(0,4100))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

# ggsave(file="exp1_RPD.pdf",width =4,height = 3)


dt.pair=dt.final%>%
  summarySE(measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==1&con=="G")|(dat_AreaNumber==1&con=="C"))%>%
  dcast(dat_Subject~con,value.var = "mean_RPD")

skewness(dt.pair$C)
skewness(dt.pair$G)

skewness(log(log(log(log(dt.pair$C)))))
skewness(log(log(log(log(dt.pair$G)))))

t.test(log(log(log(log(dt.pair$G)))),log(log(log(log(dt.pair$C)))),paired = TRUE)
t.test(dt.pair$G,dt.pair$C,paired = TRUE)

cohen.d(d=c(dt.pair$C,dt.pair$G), rep(c("Treatment","Control"),each=nrow(dt.pair)),paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==5 & con=="C")|(dat_AreaNumber==4 & con=="G"))
lmer(mean_RPD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()

dt.se=summarySE(dt.final,measurevar = "mean_RPD",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_RPD")
dt.se$dat_Subject=NULL;
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()

#' ## Total Reading Time
dt.pair=dt.final%>%
  summarySE(measurevar = "mean_TRT",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==4&con=="G")|(dat_AreaNumber==5&con=="C"))%>%
  dcast(dat_Subject~con,value.var = "mean_TRT")

skewness(dt.pair$C)
skewness(dt.pair$G)

t.test(dt.pair$G,dt.pair$C,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==1 & con=="C")|(dat_AreaNumber==1 & con=="G"))
lmer(mean_TRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()


#english ability
dt.se=summarySE(dt.final,measurevar = "mean_TRT",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_TRT")
dt.se$dat_Subject=NULL;
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()

#' ## Rereading Time
dt.pair=dt.final%>%
  summarySE(measurevar = "mean_RRT",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==4&con=="G")|(dat_AreaNumber==5&con=="C"))%>%
  dcast(dat_Subject~con,value.var = "mean_RRT")

skewness(log(dt.pair$C+100))
skewness(log(dt.pair$G+100))

skewness(dt.pair$C-dt.pair$G)
cor.test(dt.pair$C-dt.pair$G,dt.sub$ofscore)

t.test(log(dt.pair$C+100),log(dt.pair$G+100),paired = TRUE)

t.test(dt.pair$G,dt.pair$C,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==5 & con=="C")|(dat_AreaNumber==4 & con=="G"))
lmer(mean_RRT ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()


#english ability
dt.se=summarySE(dt.final,measurevar = "mean_RRT",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_RRT")
dt.se$dat_Subject=NULL;
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()
qplot(dt.rt.cor$ofscore,dt.se$GC_4,main="Rereading time",xlab = "oxford",ylab = "Garden minus Control: Area 4")

cor.test(dt.rt.cor$ofscore,dt.se$GC_4)
dt.rt.cor$ofscore

p=data.frame(dt.rt.cor$ofscore,dt.se$G_4)
p_sub=p %>% subset(dt.rt.cor.ofscore>0.776) 
mean(p_sub$dt.se.G_4)
sd(p_sub$dt.se.G_4)


pic.of=as.data.frame(matrix(NA,ncol=5,nrow=32)) %>%
  setNames(c("subject","OPT","index1","index2","index3"))

pic.of$subject=unique(dt.sub$subject)
pic.of$OPT=dt.rt.cor$ofscore
pic.of$index1=dt.se$GC_4

#' ## Gaze Duration
dt.se=dt.final%>% 
  mutate(dat_AreaNumber=ifelse((con=="G"&dat_AreaNumber!=1),dat_AreaNumber+1,dat_AreaNumber))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con"))

dt.se%>%
  subset(dat_AreaNumber!=2)%>%
  mutate(dat_AreaNumber=factor(dat_AreaNumber,level=c(1,3,4,5),labels=c("R1","R3","R4","R5")),
         con=factor(con,levels = c("C","G"),labels = c("Control","Garden-path")))%>%
  # tidyr::complete(dat_AreaNumber, con)%>%
  ggplot(aes(x=dat_AreaNumber,y=mean_GD,fill=con))+
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  geom_errorbar(aes(ymin=mean_GD-se, ymax=mean_GD+se),
                width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  ylab("First-pass Reading Time")+
  xlab("Region")+
  scale_y_continuous(limits = c(0,1300))+
  scale_fill_manual("Condition",values=c("#E69F00","#999999"))+
  theme(text = element_text(size=12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.margin=margin(0,0,-5,0))

# ggsave(file="exp1_GD.pdf",width =4,height = 3)

dt.pair=dt.final%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==3&con=="G")|(dat_AreaNumber==4&con=="C"))%>%
  dcast(dat_Subject~con,value.var = "mean_GD")

skewness(dt.pair$C)
skewness(dt.pair$G)

skewness(log(dt.pair$C))
skewness(log(dt.pair$G))

t.test(log(dt.pair$G),log(dt.pair$C),paired = TRUE)

t.test(dt.pair$G,dt.pair$C,paired = TRUE)

#lme
dt.lme=dt.final%>%
  subset((dat_AreaNumber==1 & con=="C")|(dat_AreaNumber==1 & con=="G"))
lmer(mean_GD ~ con +(1 |dat_Subject) +(1|id), data = dt.lme) %>% summary()


#english ability
dt.se=summarySE(dt.final,measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","dat_Subject")) %>%
  dcast(dat_Subject~con+dat_AreaNumber,value.var = "mean_GD")
dt.se$dat_Subject=NULL
dt.se$GC_1=dt.se$G_1-dt.se$C_1
dt.se$GC_2=dt.se$G_2-dt.se$C_3
dt.se$GC_3=dt.se$G_3-dt.se$C_4
dt.se$GC_4=dt.se$G_4-dt.se$C_5
data.frame(dt.rt.cor,dt.se)%>% corMat()%>% kable()

dt.se=dt.final%>%
  summarySE(measurevar = "mean_GD",groupvars = c("dat_AreaNumber","con","dat_Subject"))%>%
  subset((dat_AreaNumber==4&con=="G")|(dat_AreaNumber==5&con=="C"))
dt.pair=dcast(dt.se,dat_Subject~con,value.var = "mean_GD")
t.test(dt.pair$G,dt.pair$C,paired = TRUE)

