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



table(y2016deaths$cid10,y2016deaths$condicion, y2016deaths$agecat)



#       Discharged Death
#I60        529   145
145/(529+145)
#I61       1098   193
193/(1098+193)
#I63       1643   122
122/(1643+122)
#I64       2539   297
297/(2539+297)

table(y2016deaths$cid10,y2016deaths$condicion, y2016deaths$regions)
###### Lima/Callao
#I60        296    81
#I61        578    75
#I63       1003    68
#I64        895    84
######Resto Costa
#I60         99    19
#I61        353    72
#I63        396    19
#I64        744    93
######Sierra
#I60         83    29
#I61        124    35
#I63        165    25
#I64        558    91
######Selva
#I60         51    16
#I61         43    11
#I63         79    10
#I64        342    29

table(y2016deaths$cid10,y2016deaths$condicion, y2016deaths$level)

####I, II
#I60        143    45
45/(143+45)
#I61        306    76
76/(306+76)
#I63        391    48
48/(391+48)
#I64       1657   219
219/(1657+219)
###III
#I60        386   100
100/(100+386)
#I61        792   117
117/(117+792)
#I63       1252    74
74/(1252+74)
#I64        881    78
78/(881+78)

y2016deaths <- read.csv("death/yearsdeaths.cvs")
levels(y2016deaths$cid10) #4
###GENERATE EXPOSURE
y2016deaths$exposure <- ifelse(y2016deaths$cid10=="I63",0,
                               ifelse(y2016deaths$cid10=="I60",1,
                                      ifelse(y2016deaths$cid10=="I61",2,
                                             ifelse(y2016deaths$cid10=="I64",3,NA))))
y2016deaths$exposure <- factor(y2016deaths$exposure, levels = c(0,1,2,3), labels = c("I63", "I60", "I61","I64"))
sum(is.na(y2016deaths$exposure)) #0

y2016deaths$edad <- as.numeric(y2016deaths$edad)
str(y2016deaths$edad)

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
cox1 <- coxph(survivalobj0 ~ exposure++strata(regions, level),data=y2016deaths)
summary(cox1)

cox2 <- coxph(survivalobj0 ~ exposure+edad+sexo+strata(regions, level),data=y2016deaths)
summary(cox2)
str(y2016deaths$edad)
###### Weibull model####
wbmod1 <- survreg(survivalobj0 ~ exposure+strata(regions, level), data = y2016deaths)
broom::tidy(wbmod1)
wbmod2 <- survreg(survivalobj0 ~ exposure+agecat+sexo+strata(regions,level), dist="weibull",
                  data = y2016deaths
                  ) #ajusted~strata
broom::tidy(wbmod2)

#Existe un 18% menos de riesgo de mortalidad en pacientes con I60 vs I63.
#Existe un 39% menos de riesgo de mortalidad en pacientes con I61 vs I63.
#Existe un 54% menos de riesgo de mortalidad en pacientes con I64 vs I63.



