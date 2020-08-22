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

subdelete=c("37ZHEEHM6XP4PYHW1B2V3MNOHPO37I",
            "3HYA4D452SMSJ90JFUM284VD9Z9F2P",
            "3I0BTBYZAYORNQE05XACE19EICCY0I",
            "3PB5A5BD0W95JATKWQ9FMGCXLUT7GU",
            "3XUHV3NRVL14W2ACS3C09WA294ZH58",
            "3S96KQ6I9N7P5OG073B8LR1IXZ8DT6",
            "392CY0QWG2UMWEHWKGBB4G5395UI4C")

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
write.csv(dt.sub.age, "age.csv")
