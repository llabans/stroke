library(plyr)
library(dplyr) 
library(foreign)
library(readstata13)
library(ggplot2)
library(gridExtra)
library(gtools)
library(Hmisc)
library(magrittr)
library(survival)
library(ggplot2)
library(coxme)
library(tidyr)
library(lattice)
library(broom)
library(survival)
library(survminer)
library(lubridate)

setwd("~/Documentos/R/Stroke/death/")
y2016deaths <- read.csv("yearsdeaths.cvs")
levels(as.factor(y2016deaths$cid10)) #4
str(y2016deaths)
table(y2016deaths$cid10,y2016deaths$agecat,y2016deaths$condicion)
y2016deaths$edad <- as.numeric(y2016deaths$edad)
summarise(y2016deaths$edad)
tapply(edad,condicion,sd)
sd(edad)


y2016deaths$agecat <- ifelse(y2016deaths$edad<55, 0, ifelse(y2016deaths$edad>=55 & y2016deaths$edad<75, 1, ifelse(y2016deaths$edad>=75, 2, NA))) %>%  
  factor(y2016deaths$agecat, levels = c(0,1,2), labels = c("<55", "55-74", "75+"))

###GENERATE EXPOSURE
y2016deaths$exposure <- ifelse(y2016deaths$cid10=="I63",0,
                               ifelse(y2016deaths$cid10=="I60",1,
                                      ifelse(y2016deaths$cid10=="I61",2,
                                             ifelse(y2016deaths$cid10=="I64",3,NA))))
y2016deaths$exposure <- factor(y2016deaths$exposure, levels = c(0,1,2,3), labels = c("I63", "I60", "I61","I64"))
sum(is.na(y2016deaths$exposure)) #0

y2016deaths$edad <- as.numeric(y2016deaths$edad)
str(y2016deaths$exposure)

# subarachnoid haemorrhage (ICD-10 code I60),
#intra-cerebral haemorrhage (ICD-10 code I61),
#cerebral infarction (ICD-10 code I63)
#stroke not specified as haemorrhage or infarction (ICD-10 code I64).

##GENERATE SURVIVAL OBJECT
y2016deaths$survivalobj0 <- with(y2016deaths,Surv(los,condicion=="Death"))
summary(y2016deaths$survivalobj0)
###CHECK PROPORTIONAL HAZARD ASSUMPTION 
##(p<0.05 indicates a violation of the assumption) IN CRUDE
prop.res.cox1<-cox.zph(coxph(survivalobj0 ~ exposure+strata(regions, level),data=y2016deaths))
prop.res.cox1 #VIOLATION OF THE ASSUMPTION

#ADJUSTED MODEL
prop.res.cox2 <- cox.zph(coxph(survivalobj0 ~ exposure+sexo+agecat+strata(regions, level),data=y2016deaths))
prop.res.cox2 #VIOLATION OF THE ASSUMPTION 

########################
###### WEIBULL MODEL####
########################
library(eha)

#Model not adjusted
wb1 <- weibreg(survivalobj0 ~ exposure+strata(regions, level),
               data = y2016deaths) #strata
#I60
#exp(0.850 + 1.96*0.125)
#exp(0.850 -1.96*0.125)
#I61
#exp(0.636 +1.96*0.117)
#exp(0.636 -1.96*0.117)
#I64
#exp(0.393 +1.96*0.113)
#exp(0.393 -1.96*0.113)
#y2016deaths$sexo <- as.numeric(y2016deaths$sexo)
#numeric covariables (edad, sex)

#Adjusted model
ehawb2 <- weibreg(survivalobj0 ~ exposure+sexo+edad+strata(regions,level),
                  data = y2016deaths) #ajusted~strata
#I60
#exp(0.895 + 1.96*0.127)
#exp(0.895 -1.96*0.127)
#I61
#exp(0.670 +1.96*0.117)
#exp(0.670 -1.96*0.117)
#I64
#exp(0.374 +1.96*0.113)
#exp(0.374 -1.96*0.113)

#################################################################################
#Creating survival plot

install.packages('survminer')

library(survminer)
library(ggpubr)
library(survival)

y2016deaths$cid10 <- factor(y2016deaths$cid10, levels = c("I60","I61","I63","I64"), labels = c("Subarachnoid Hemorrhage (I60)", "Intra-cerebral Hemorrhage (I61)", "Cerebral Infarction (I63)", "Stroke not specified (I64)"))
names(y2016deaths)[names(y2016deaths) == "cid10"] <- 'ICD-10'
losfit <- survfit(Surv(y2016deaths$los, y2016deaths$condicion=="Death") ~ y2016deaths$'ICD-10',
                  data = y2016deaths)

# Visualize with survminer
ggsurvplot(losfit, data = y2016deaths, main = "Survival curve", pval = TRUE, size = 0.5, pval.coord =c(2,0),
           submain = "Based on Kaplan-Meier estimates", legend = c(0.2, 0.2),
           font.submain = 18,
           #font.x =  16,
           #font.y = 16,
           #font.tickslab = 16,
           font.legend = 13,
           #font.caption = 16,
           #risk.table.fontsize = 6,
           #risk.table.height = 0.45,
           #font.family = 14,
           #fontsize =6,
           risk.table = TRUE, xlim = c(0,30), ylim =c(0.6,1),xlab = "Time in days",
           legend.labs = c("Cerebral Infarction (I63) (n=122)","Intra-cerebral Hemorrhage (I61) (n=193)","Stroke not specified as hemorrhage or infarction (I64) (n=297)","Subarachnoid Hemorrhage (I60) (n=145)"), 
           break.time.by = 5, risk.table.y.text.col = T, risk.table.y.text = FALSE, conf.int = TRUE,conf.int.style=c("ribbon"), ggtheme = theme_bw())


#comment

library(survival)
library(MASS)
data(gehan)
gehansurv=Surv(gehan$time, gehan$cens)
plot(survfit(gehansurv ~ gehan$treat), col=c("black", "red"), fun="cloglog")
ggcoxzph(test.ph1)
