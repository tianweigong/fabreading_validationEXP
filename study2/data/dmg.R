library(readr)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(reshape2)
library(knitr)
library(tidyr)
library(e1071)

'%ni%' <- Negate('%in%')

dt=read_csv("ACC_data.csv") 

dt=subset(dt,exp!="prac")

a=dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject"))

subdelete=c("3UN61F00HXSHLWOFO8OUN0C8WIYR5Q",
            "3AAPLD8UCDK6H4L2TZUYCCGGQFPHTK")

dt=subset(dt, dat_Subject %ni% subdelete)

dt%>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("dat_Subject","con")) %>%
  summarySE(measurevar = "dat_Accuracy",groupvars = c("con"))

a=unique(dt$dat_Subject)

dt.sub=read_csv("questiondata.csv") 
dt.sub.v=dt.sub%>%subset(id %in% a)
dt.sub.gender=dt.sub.v%>%subset(item=="gender")

dt.sub.age=dt.sub.v%>%subset(item=="age")
mean(as.numeric(dt.sub.age$answer))
sd(dt.sub.age$answer)
# write.csv(dt.sub.age, "age.csv")
