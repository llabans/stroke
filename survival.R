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
levels(y2016deaths$cid10) #4
str(y2016deaths)
table(y2016deaths$cid10,y2016deaths$agecat,y2016deaths$condicion)



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

##COX MODELS
###### Weibull model####
#######################eha
install.packages("eha")
library(eha)
ehawb1 <- aftreg(survivalobj0 ~ exposure+strata(regions, level),dist = "weibull",
                 data = y2016deaths)
ehawb1
exp(confint(ehawb1))

ehawb2 <- aftreg(survivalobj0 ~ exposure+agecat+sexo+strata(regions,level), dist="weibull",
                  data = y2016deaths) #ajusted~strata
ehawb2
exp(confint(ehawb2))

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

palette()               # obtain the current palette

(palette(gray(seq(0,.9,len = 25)))) # gray scales; print old palette
matplot(outer(1:100, 1:30), type = "l", lty = 1,lwd = 2, col = 1:30,
        main = "Gray Scales Palette",
        sub = "palette(gray(seq(0, .9, len=25)))")
palette("default")      # reset back to the default

# Visualize with survminer
ggsurvplot(losfit, data = y2016deaths, main = "Survival curve",
           submain = "Based on Kaplan-Meier estimates", legend = c(0.1, 0.2),
           legend.title = "Strata",  risk.table = TRUE, xlim = c(0,30),
           break.time.by = 5, risk.table.y.text.col = T, risk.table.y.text = FALSE, conf.int = TRUE,conf.int.style=c("ribbon"), pval = TRUE, palette = gray(0:4/4))



