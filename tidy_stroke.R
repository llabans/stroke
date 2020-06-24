#Import Egresos 2002-2017
# Load libraries 
library(readxl)
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(foreign)
library(readstata13)
library(ggplot2)
library(gridExtra)
library(gtools)
library(Hmisc)
library(magrittr)

setwd("~/Documentos/R/Stroke/")

# Load excel by sheets and # transform variable names (all to lowercase) 

stroke <- read.csv("datastroke_2002-2017.csv") %>% 
  rename_all(funs(stringr::str_to_lower(.))) %>% mutate_if(is.factor, as.character) 

table(stroke$condicion,stroke$anio)
table(y2016deaths$anio==2017,y2016deaths$cid10)

###MAKE A NEW DATASET FOR Y2016-2017 WITH DEATH CASES
#------------------------------------------------
y2016deaths <- stroke %>% filter(anio %in% c("2016","2017")) %>% mutate(cid10=substr(cod_enf,1,3)) %>% 
               filter(cid10 %in% c("I60","I61","I63","I64") & condicion %in% c("1","5") & t_edad==1 & edad>=35) %>% 
               mutate(condicion=factor(condicion, levels = c(1,5), labels = c("Discharged", "Death")))
y2016deaths$agecat <- ifelse(y2016deaths$edad<55, 0, ifelse(y2016deaths$edad>=55 & y2016deaths$edad<75, 1, ifelse(y2016deaths$edad>=75, 2, NA))) %>%  
                            factor(y2016deaths$agecat, levels = c(0,1,2), labels = c("<55", "55-74", "75+"))
y2016deaths$sexo <- factor(y2016deaths$sexo, levels = c(1,2), labels = c("Men", "Women"))
y2016deaths$level <- ifelse(y2016deaths$level=="I-" | y2016deaths$level=="II-", 0, ifelse(y2016deaths$level=="III", 1,NA))
y2016deaths$level <- factor(y2016deaths$level, levels = c(0,1), labels = c("I, II", "III"))
y2016deaths$regions <- ifelse(y2016deaths$dpto_eess=="LIMA" | y2016deaths$dpto_eess=="CALLAO",0, 
                              ifelse(y2016deaths$dpto_eess=="TUMBES" | y2016deaths$dpto_eess=="PIURA" | y2016deaths$dpto_eess=="LAMBAYEQUE" | y2016deaths$dpto_eess=="LA LIBERTAD" | y2016deaths$dpto_eess=="ANCASH" | y2016deaths$dpto_eess=="ICA" | y2016deaths$dpto_eess=="AREQUIPA" | y2016deaths$dpto_eess=="MOQUEGUA" | y2016deaths$dpto_eess=="TACNA",1,
                                     ifelse(y2016deaths$dpto_eess=="CAJAMARCA" | y2016deaths$dpto_eess=="HUANUCO" | y2016deaths$dpto_eess=="PASCO" | y2016deaths$dpto_eess=="JUNIN" | y2016deaths$dpto_eess=="HUANCAVELICA" | y2016deaths$dpto_eess=="AYACUCHO" | y2016deaths$dpto_eess=="APURIMAC" | y2016deaths$dpto_eess=="CUSCO" | y2016deaths$dpto_eess=="PUNO",2,
                                            ifelse(y2016deaths$dpto_eess=="AMAZONAS" | y2016deaths$dpto_eess=="SAN MARTIN" | y2016deaths$dpto_eess=="LORETO" | y2016deaths$dpto_eess=="UCAYALI" | y2016deaths$dpto_eess=="MADRE DE DIOS",3,NA))))
y2016deaths$regions <- factor(y2016deaths$regions, levels = c(0,1,2,3), 
                              labels = c("Lima/Callao", "Resto Costa", "Sierra", "Selva"))
y2016deaths$ingreso <- y2016deaths$f_ingre %>% ymd()
y2016deaths$egreso <- y2016deaths$f_egres %>% ymd()
y2016deaths <- subset(y2016deaths, select = -c(f_ingre, f_egres))
y2016deaths$los <- as.numeric(difftime(y2016deaths$egreso, y2016deaths$ingreso, units = c("days")))
y2016deaths$los <- ifelse(y2016deaths$los<0, as.numeric(difftime(y2016deaths$ingreso,y2016deaths$egreso, units = c("days"))), y2016deaths$los)
quantile(y2016deaths$los, c(.01, .99), na.rm = TRUE)
y2016deaths <- y2016deaths[which(y2016deaths$los>0 & y2016deaths$los<60 & !is.na(y2016deaths$los)), ]
y2016deaths <- subset(y2016deaths, select = c(sexo, edad, cid10, agecat, level, regions, condicion, los,anio)) 

write.csv(y2016deaths, "./death/yearsdeaths.cvs", row.names=FALSE)

#-----------------------------------
###KEEP CEREBROVASCULAR DISEASES:
#---------------------------------
###(I60[subarachnoid hemorrhage], 
###I61[intracerebral hemorrhage], 
###I63[cerebral infarction], 
###I64[stroke])
###http://www.who.int/healthinfo/statistics/bod_cerebrovasculardiseasestroke.pdf 

stroke$cid10 <- substr(stroke$cod_enf,1,3)
stroke <- stroke[which(stroke$cid10=="I60" | stroke$cid10=="I61" | stroke$cid10=="I63" | stroke$cid10=="I64"), ]
stroke$cid10 <- factor(stroke$cid10, levels = c("I60","I61","I63","I64"), labels = c("I60","I61","I63","I64"))
stroke %>% count(cid10) #57153
#1 I60  6417
#2 I61 10278
#3 I63 11594
#4 I64 28864
#98605- 57153 -> NO DCV
#----------------------------------------------------
###KEEP PATIENTS THAT WHERE DISCHARGED BY A PHYSICIAN
#----------------------------------------------------
stroke <- stroke[which(stroke$condicion==c("1")), ] 
dplyr::count(stroke) #47168
levels(as.factor(stroke$condicion))
#57153-47168 -> NON PHYSICIAN
#---------------------------
#DISCHARGED BY NON PHYSICIAN
#---------------------------
#1=Alta Médica
#2=Alta Voluntaria
#3=Transferido / Referido
#4=Fugado
#5=Fallecido
strokenonphysic <- stroke %>% 
  filter(condicion %in% c("2","3","4","5","6")) %>%
  mutate(condicion=factor(condicion,levels = c(2,3,4,5,6),labels = c("Alta medica","Alta voluntaria","Transferido","Fugado","Fallecido")))
strokenonphysic <- stroke %>% mutate(cid10=substr(cod_enf,1,3)) %>% 
  filter(cid10 %in% c("I60","I61","I63","I64") & t_edad==1 & edad>=35) 
strokenonphysic$agecat <- ifelse(strokenonphysic$edad<55, 0, ifelse(strokenonphysic$edad>=55 & strokenonphysic$edad<75, 1, ifelse(strokenonphysic$edad>=75, 2, NA))) %>%  
  factor(strokenonphysic$agecat, levels = c(0,1,2), labels = c("<55", "55-74", "75+"))
strokenonphysic$sexo <- factor(strokenonphysic$sexo, levels = c(1,2), labels = c("Men", "Women"))
strokenonphysic$level <- ifelse(strokenonphysic$level=="I-" | strokenonphysic$level=="II-", 0, ifelse(strokenonphysic$level=="III", 1,NA)) 
strokenonphysic$level <- factor(strokenonphysic$level, levels = c(0,1), labels = c("I, II", "III"))
strokenonphysic$regions <- ifelse(strokenonphysic$dpto_eess=="LIMA" | strokenonphysic$dpto_eess=="CALLAO",0, 
                                  ifelse(strokenonphysic$dpto_eess=="TUMBES" | strokenonphysic$dpto_eess=="PIURA" | strokenonphysic$dpto_eess=="LAMBAYEQUE" | strokenonphysic$dpto_eess=="LA LIBERTAD" | strokenonphysic$dpto_eess=="ANCASH" | strokenonphysic$dpto_eess=="ICA" | strokenonphysic$dpto_eess=="AREQUIPA" | strokenonphysic$dpto_eess=="MOQUEGUA" | strokenonphysic$dpto_eess=="TACNA",1,
                                         ifelse(strokenonphysic$dpto_eess=="CAJAMARCA" | strokenonphysic$dpto_eess=="HUANUCO" | strokenonphysic$dpto_eess=="PASCO" | strokenonphysic$dpto_eess=="JUNIN" | strokenonphysic$dpto_eess=="HUANCAVELICA" | strokenonphysic$dpto_eess=="AYACUCHO" | strokenonphysic$dpto_eess=="APURIMAC" | strokenonphysic$dpto_eess=="CUSCO" | strokenonphysic$dpto_eess=="PUNO",2,
                                                ifelse(strokenonphysic$dpto_eess=="AMAZONAS" | strokenonphysic$dpto_eess=="SAN MARTIN" | strokenonphysic$dpto_eess=="LORETO" | strokenonphysic$dpto_eess=="UCAYALI" | strokenonphysic$dpto_eess=="MADRE DE DIOS",3,NA))))
strokenonphysic$regions <- factor(strokenonphysic$regions, levels = c(0,1,2,3), 
                                  labels = c("Lima/Callao", "Resto Costa", "Sierra", "Selva"))
strokenonphysic$ingreso <- strokenonphysic$f_ingre %>% ymd()
strokenonphysic$egreso <- strokenonphysic$f_egres %>% ymd()
strokenonphysic <- subset(strokenonphysic, select = -c(f_ingre, f_egres))
strokenonphysic$los <- as.numeric(difftime(strokenonphysic$egreso, strokenonphysic$ingreso, units = c("days")))
strokenonphysic$los <- ifelse(strokenonphysic$los<0, as.numeric(difftime(strokenonphysic$ingreso,strokenonphysic$egreso, units = c("days"))), strokenonphysic$los)

dplyr::count(strokenonphysic)
summary(strokenonphysic$condicion)
describe(stroke$condicion)
summary(strokenonphysic$los)
table(strokenonphysic$condicion,strokenonphysic$los)

strokenonphysic <- strokenonphysic %>% filter(anio==c("2011","2012","2013","2014","2015","2016","2017"))
by_year <- strokenonphysic %>%
  group_by(anio) %>%
  mutate(los=as.numeric(los)) %>%
  dplyr::summarise("Q1" = quantile(los, probs=0.25,na.rm = TRUE), 
                   "Median" = quantile(los, probs=0.5,na.rm = TRUE), 
                   "Q3" = quantile(los, probs=0.75,na.rm = TRUE), 
                   "n"=n())
losallnonp <- ggplot(by_year, aes(x=anio, y=Median))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(by_year, mapping = aes(ymin=Q1, ymax=Q3), width=0.2, size=2)+
  annotate("text",x=2011,y=0,label="2011")+
  annotate("text",x=2012,y=0,label="2012")+
  annotate("text",x=2013,y=0,label="2013")+
  annotate("text",x=2014,y=0,label="2014")+
  annotate("text",x=2015,y=0,label="2015")+
  annotate("text",x=2016,y=0,label="2016")+
  annotate("text",x=2017,y=0,label="2017")

plot(losallnonp)
strokenonphysic %>% glimpse()
table(strokenonphysic$condicion==4,strokenonphysic$anio)
52563-29354 

#==========================================
###KEEP SUBJECTS WITH AGE MEASURED IN YEARS
#==========================================
stroke <- stroke[which(stroke$t_edad==1), ]
dplyr::count(stroke) #46992
#47168-46992 -> NON YEARS

#=================================
###KEEP ONLY PEOPLE AGED 35+ YEARS
#=================================
stroke <- stroke[which(stroke$edad>=35), ]
dplyr::count(stroke) #43383
#46992-43383 -> NON AGED 35+ YEARS
#============
###CHECK AGES
#============
summary(stroke$edad,stroke$anio)

#=========================
###GENERATE AGE CATEGORIES 
#=========================
stroke$agecat <- ifelse(stroke$edad<55, 0, 
                       ifelse(stroke$edad>=55 & stroke$edad<75, 1, 
                              ifelse(stroke$edad>=75, 2, NA))) %>% 
                                    factor(stroke$agecat, levels = c(0,1,2), labels = c("<55", "55-74", "75+"))
summary(stroke$agecat)
#<55 55-74   75+ 
#8174 19434 15775
#===========
###LABEL SEX
#===========
stroke$sexo <- factor(stroke$sexo, levels = c(1,2), labels = c("Men", "Women"))

#========================
###EXTRACT HOSPITAL LEVEL
#========================
stroke$level <- ifelse(stroke$level=="I-" | stroke$level=="I-3" | stroke$level=="I-4" | stroke$level=="II-", 0, 
                      ifelse(stroke$level=="III", 1, NA))
stroke$level <- factor(stroke$level, levels = c(0,1), labels = c("I, II", "III"))

#====================
###CHECK DEPARTAMENTO
#====================
stroke$dpto_eess <- as.factor(stroke$dpto_eess)

#==================
###GENERATE REGIONS
#==================
stroke$regions <- ifelse(stroke$dpto_eess=="LIMA" | stroke$dpto_eess=="CALLAO",0, 
                        ifelse(stroke$dpto_eess=="TUMBES" | stroke$dpto_eess=="PIURA" | stroke$dpto_eess=="LAMBAYEQUE" | stroke$dpto_eess=="LA LIBERTAD" | stroke$dpto_eess=="ANCASH" | stroke$dpto_eess=="ICA" | stroke$dpto_eess=="AREQUIPA" | stroke$dpto_eess=="MOQUEGUA" | stroke$dpto_eess=="TACNA",1,
                               ifelse(stroke$dpto_eess=="CAJAMARCA" | stroke$dpto_eess=="HUANUCO" | stroke$dpto_eess=="PASCO" | stroke$dpto_eess=="JUNIN" | stroke$dpto_eess=="HUANCAVELICA" | stroke$dpto_eess=="AYACUCHO" | stroke$dpto_eess=="APURIMAC" | stroke$dpto_eess=="CUSCO" | stroke$dpto_eess=="PUNO",2,
                                      ifelse(stroke$dpto_eess=="AMAZONAS" | stroke$dpto_eess=="SAN MARTIN" | stroke$dpto_eess=="LORETO" | stroke$dpto_eess=="UCAYALI" | stroke$dpto_eess=="MADRE DE DIOS",3,NA))))
stroke$regions <- factor(stroke$regions, levels = c(0,1,2,3), 
                        labels = c("Lima/Callao", "Resto Costa", "Sierra", "Selva"))

#============================================
###TRANSFORM CHARACTER DATES INTO DATE FORMAT
#============================================

stroke$ingreso <- stroke$f_ingre %>% ymd()
stroke$egreso <- stroke$f_egres %>% ymd()
stroke <- subset(stroke, select = -c(f_ingre, f_egres))

#=======================================
###GENERATE TIME - LENGHT OF STAY  (LOS)
#=======================================

stroke$los <- as.numeric(difftime(stroke$egreso, stroke$ingreso, units = c("days")))
strokeanios <- stroke %>% filter(anio %in% c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011"))
stroke <- stroke %>% filter(anio %in% c("2012","2013","2014","2015","2016","2017"))
#========================
###CHECK INGRESO < EGRESO
#========================
stroke$los <- ifelse(stroke$los<0, as.numeric(difftime(stroke$ingreso,stroke$egreso, units = c("days"))), stroke$los)

#LOS
stroke <- stroke %>% select(-c(t_estanc,ingreso,egreso))
sum(is.na(stroke$los))
strokeanios <- subset(strokeanios, select = -c(los,egreso,ingreso))
names(strokeanios)[names(strokeanios)=="t_estanc"]<-"los"
sum(is.na(strokeanios$los)) #0
strokeanios$los <- as.numeric(strokeanios$los)

#JOIN LOS
stroke <- stroke %>% full_join(strokeanios)
levels(as.factor(stroke$anio))
levels(as.factor(stroke$los))
sum(is.na(stroke$los)) 
describe(stroke$los)

#===============
###SUMMARYZE LOS
#===============
summary(stroke$los) #na 1
#========================================
###EXCLUDE 1ST AND 99TH PERCENTILE OF LOS
#========================================
#Stroke con v.e
#stroken_ve <- stroke #43383
count(stroke) #43383
quantile(stroke$los, c(.01, .99),na.rm = TRUE)
stroke <- stroke[which(stroke$los>1 & stroke$los<56), ]
dplyr::count(stroke) #39810

#N? exclude by LOS below and above and 1st and 99th percentile
#43383-39810 -> EXCLUDED 

#===========================
###APPEND ALL IN ONE DATASET
#===========================
stroke$year <- as.numeric(stroke$anio)

#stroken_ve$year <- as.numeric(stroken_ve$anio)
summary(stroke)
sum(is.na(stroke)) # 10
###REMOVE VARIABLES THAT WILL NOT BE USED
stroke <- subset(stroke, select = c(year,sexo, edad, cid10, agecat, level, regions, los,condicion))
#stroken_ve <- subset(stroken_ve, select = c(year,sexo, edad, cid10, agecat, level, regions, los,condicion))
write.csv(stroke, "./stroke.csv", row.names=FALSE)
#write.csv(stroken_ve, "./stroke_ve.csv" )
rm(strokeanios)
#sum(is.na(stroke$cid10))
