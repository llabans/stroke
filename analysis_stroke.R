############################################################
###LENGTH OF HOSPITALIZATION FOR STROKE AND MI MINSA PERU###
############################################################
###LIBRARIES
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
library(tidyverse)
library(purrr)
library(gglop)
library(EnvStats)

###FUNCTIONS
number_ticks <- function(n) {function(limits) pretty(limits, n)}

g_legend<-function(a.gplot){
  tmp<-ggplot_gtable(ggplot_build(a.gplot))
  leg<-which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend<-tmp$grobs[[leg]]
  return(legend)}

###LOAD DATA
setwd("~/Documentos/R/Stroke/")
stroke <- read.csv("./stroke.csv")
stroke$los <- as.numeric(stroke$los)
stroke$year <- as.factor(stroke$year)
levels(as.factor(stroke$year))


View(strokesex)

###SUMMARY OF LOS BY YEAR
by_year <- stroke %>%
  group_by(year) %>%
  mutate(los=as.numeric(los)) %>%
  dplyr::summarise("Q1" = quantile(los, probs=0.25, na.rm = TRUE), 
            "Median" = quantile(los, probs=0.5,na.rm = TRUE), 
            "Q3" = quantile(los, probs=0.75,na.rm = TRUE), 
            "n"=n())
by_year

#Count
stroke %>% dplyr::count(los)

#Variables
stroke %>% glimpse()
losall <- ggplot2::ggplot(by_year, aes(x=year, y=Median,colour=year), size(year))+
  labs(color="Year")+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(by_year, mapping = aes(ymin=Q1, ymax=Q3), width=0.2, size=2)

plot(losall)

strokemale <- subset(stroke, stroke$sexo=="Men", select = c(year, los))
strokemale <- strokemale%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokefemale <- subset(stroke, stroke$sexo=="Women", select = c(year, los))
strokefemale <- strokefemale%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                          losq1=quantile(los, c(.25)),
                                                          losq3=quantile(los, c(.75)))
strokemale$Sex <- 0
strokefemale$Sex <- 1
strokesex <- rbind(strokemale, strokefemale)
strokesex$Sex <- factor(strokesex$Sex, levels = c(0,1), labels = c("Men", "Women"))
rm(strokemale, strokefemale)
strokesex
strokesex$year <- ifelse(strokesex$year==2002 & strokesex$Sex=="Men", 2002.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2003 & strokesex$Sex=="Men", 2003.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2004 & strokesex$Sex=="Men", 2004.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2005 & strokesex$Sex=="Men", 2005.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2006 & strokesex$Sex=="Men", 2006.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2007 & strokesex$Sex=="Men", 2007.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2008 & strokesex$Sex=="Men", 2008.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2009 & strokesex$Sex=="Men", 2009.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2010 & strokesex$Sex=="Men", 2010.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2011 & strokesex$Sex=="Men", 2011.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2012 & strokesex$Sex=="Men", 2012.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2013 & strokesex$Sex=="Men", 2013.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2014 & strokesex$Sex=="Men", 2014.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2015 & strokesex$Sex=="Men", 2015.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2016 & strokesex$Sex=="Men", 2016.5, strokesex$year)
strokesex$year <- ifelse(strokesex$year==2017 & strokesex$Sex=="Men", 2017.5, strokesex$year)
plot(sexstroke)

sexstroke <- ggplot(strokesex, aes(x=year, y=losmediaan, color=Sex))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(strokesex, mapping = aes(ymin=losq1, ymax=losq3), size=2)+
  annotate("text",x=2002.25,y=0,label="2002")+
  annotate("text",x=2003.25,y=0,label="2003")+
  annotate("text",x=2004.25,y=0,label="2004")+
  annotate("text",x=2005.25,y=0,label="2005")+
  annotate("text",x=2006.25,y=0,label="2006")+
  annotate("text",x=2007.25,y=0,label="2007")+
  annotate("text",x=2008.25,y=0,label="2008")+
  annotate("text",x=2009.25,y=0,label="2009")+
  annotate("text",x=2010.25,y=0,label="2010")+
  annotate("text",x=2011.25,y=0,label="2011")+
  annotate("text",x=2012.25,y=0,label="2012")+
  annotate("text",x=2013.25,y=0,label="2013")+
  annotate("text",x=2014.25,y=0,label="2014")+
  annotate("text",x=2015.25,y=0,label="2015")+
  annotate("text",x=2016.25,y=0,label="2016")+
  annotate("text",x=2017.25,y=0,label="2017")
strokesex

plot(sexstroke)

strokeage1 <- subset(stroke, stroke$agecat=="<55", select = c(year, los))
strokeage1 <- strokeage1%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokeage2 <- subset(stroke, stroke$agecat=="55-74", select = c(year, los))
strokeage2 <- strokeage2%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokeage3 <- subset(stroke, stroke$agecat=="75+", select = c(year, los))
strokeage3 <- strokeage3%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokeage1$agecat <- 1
strokeage2$agecat <- 2
strokeage3$agecat <- 3
strokeage <- rbind(strokeage1, strokeage2, strokeage3)
strokeage$agecat <- factor(strokeage$agecat, levels = c(1,2,3), labels = c("<55", "55-74", "75+"))
rm(strokeage1, strokeage2, strokeage3)

strokeage$year <- ifelse(strokeage$year==2002 & strokeage$agecat=="55-74", 2002.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2002 & strokeage$agecat=="75+", 2002.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2003 & strokeage$agecat=="55-74", 2003.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2003 & strokeage$agecat=="75+", 2003.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2004 & strokeage$agecat=="55-74", 2004.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2004 & strokeage$agecat=="75+", 2004.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2005 & strokeage$agecat=="55-74", 2005.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2005 & strokeage$agecat=="75+", 2005.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2006 & strokeage$agecat=="55-74", 2006.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2006 & strokeage$agecat=="75+", 2006.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2007 & strokeage$agecat=="55-74", 2007.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2007 & strokeage$agecat=="75+", 2007.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2008 & strokeage$agecat=="55-74", 2008.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2008 & strokeage$agecat=="75+", 2008.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2009 & strokeage$agecat=="55-74", 2009.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2009 & strokeage$agecat=="75+", 2009.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2010 & strokeage$agecat=="55-74", 2010.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2010 & strokeage$agecat=="75+", 2010.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2011 & strokeage$agecat=="55-74", 2011.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2011 & strokeage$agecat=="75+", 2011.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2012 & strokeage$agecat=="55-74", 2012.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2012 & strokeage$agecat=="75+", 2012.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2013 & strokeage$agecat=="55-74", 2013.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2013 & strokeage$agecat=="75+", 2013.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2014 & strokeage$agecat=="55-74", 2014.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2014 & strokeage$agecat=="75+", 2014.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2015 & strokeage$agecat=="55-74", 2015.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2015 & strokeage$agecat=="75+", 2015.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2016 & strokeage$agecat=="55-74", 2016.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2016 & strokeage$agecat=="75+", 2016.6, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2017 & strokeage$agecat=="55-74", 2017.3, strokeage$year)
strokeage$year <- ifelse(strokeage$year==2017 & strokeage$agecat=="75+", 2017.6, strokeage$year)

names(strokeage)[names(strokeage)=="agecat"]<-"Age"

agestroke <- ggplot(strokeage, aes(x=year, y=losmediaan, color=Age))+
  scale_color_hue(labels = c("<55", "55-74 (p<0.001)", "75+ (p<0.001)"))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(strokeage, mapping = aes(ymin=losq1, ymax=losq3), size=2)+
  annotate("text",x=2002.3,y=0,label="2002")+
  annotate("text",x=2003.3,y=0,label="2003")+
  annotate("text",x=2004.3,y=0,label="2004")+
  annotate("text",x=2005.3,y=0,label="2005")+
  annotate("text",x=2006.3,y=0,label="2006")+
  annotate("text",x=2007.3,y=0,label="2007")+
  annotate("text",x=2008.3,y=0,label="2008")+
  annotate("text",x=2009.3,y=0,label="2009")+
  annotate("text",x=2010.3,y=0,label="2010")+
  annotate("text",x=2011.3,y=0,label="2011")+
  annotate("text",x=2012.3,y=0,label="2012")+
  annotate("text",x=2013.3,y=0,label="2013")+
  annotate("text",x=2014.3,y=0,label="2014")+
  annotate("text",x=2015.3,y=0,label="2015")+
  annotate("text",x=2016.3,y=0,label="2016")+
  annotate("text",x=2017.3,y=0,label="2017")
plot(agestroke)

strokeregion1 <- subset(stroke, stroke$regions=="Lima/Callao", select = c(year, los))
strokeregion1 <- strokeregion1%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                            losq1=quantile(los, c(.25)),
                                                            losq3=quantile(los, c(.75)))
strokeregion2 <- subset(stroke, stroke$regions=="Resto Costa", select = c(year, los))
strokeregion2 <- strokeregion2%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                            losq1=quantile(los, c(.25)),
                                                            losq3=quantile(los, c(.75)))
strokeregion3 <- subset(stroke, stroke$regions=="Selva", select = c(year, los))
strokeregion3 <- strokeregion3%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                            losq1=quantile(los, c(.25)),
                                                            losq3=quantile(los, c(.75)))
strokeregion4 <- subset(stroke, stroke$regions=="Sierra", select = c(year, los))
strokeregion4 <- strokeregion4%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                            losq1=quantile(los, c(.25)),
                                                            losq3=quantile(los, c(.75)))
strokeregion1$region <- 1
strokeregion2$region <- 2
strokeregion3$region <- 3
strokeregion4$region <- 4
strokeregion <- rbind(strokeregion1, strokeregion2, strokeregion3, strokeregion4)
strokeregion$region <- factor(strokeregion$region, levels = c(1,2,3,4), labels = c("Lima/Callao", "Rest Coast", "Amazon", "Highlands"))
rm(strokeregion1, strokeregion2, strokeregion3, strokeregion4)

strokeregion$year <- ifelse(strokeregion$year==2002 & strokeregion$region=="Rest Coast", 2002.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2002 & strokeregion$region=="Highlands", 2002.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2002 & strokeregion$region=="Amazon", 2002.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2003 & strokeregion$region=="Rest Coast", 2003.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2003 & strokeregion$region=="Highlands", 2003.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2003 & strokeregion$region=="Amazon", 2003.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2004 & strokeregion$region=="Rest Coast", 2004.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2004 & strokeregion$region=="Highlands", 2004.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2004 & strokeregion$region=="Amazon", 2004.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2005 & strokeregion$region=="Rest Coast", 2005.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2005 & strokeregion$region=="Highlands", 2005.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2005 & strokeregion$region=="Amazon", 2005.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2006 & strokeregion$region=="Rest Coast", 2006.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2006 & strokeregion$region=="Highlands", 2006.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2006 & strokeregion$region=="Amazon", 2006.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2007 & strokeregion$region=="Rest Coast", 2007.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2007 & strokeregion$region=="Highlands", 2007.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2007 & strokeregion$region=="Amazon", 2007.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2008 & strokeregion$region=="Rest Coast", 2008.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2008 & strokeregion$region=="Highlands", 2008.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2008 & strokeregion$region=="Amazon", 2008.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2009 & strokeregion$region=="Rest Coast", 2009.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2009 & strokeregion$region=="Highlands", 2009.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2009 & strokeregion$region=="Amazon", 2009.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2010 & strokeregion$region=="Rest Coast", 2010.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2010 & strokeregion$region=="Highlands", 2010.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2010 & strokeregion$region=="Amazon", 2010.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2011 & strokeregion$region=="Rest Coast", 2011.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2011 & strokeregion$region=="Highlands", 2011.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2011 & strokeregion$region=="Amazon", 2011.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2012 & strokeregion$region=="Rest Coast", 2012.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2012 & strokeregion$region=="Highlands", 2012.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2012 & strokeregion$region=="Amazon", 2012.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2013 & strokeregion$region=="Rest Coast", 2013.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2013 & strokeregion$region=="Highlands", 2013.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2013 & strokeregion$region=="Amazon", 2013.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2014 & strokeregion$region=="Rest Coast", 2014.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2014 & strokeregion$region=="Highlands", 2014.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2014 & strokeregion$region=="Amazon", 2014.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2015 & strokeregion$region=="Rest Coast", 2015.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2015 & strokeregion$region=="Highlands", 2015.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2015 & strokeregion$region=="Amazon", 2015.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2016 & strokeregion$region=="Rest Coast", 2016.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2016 & strokeregion$region=="Highlands", 2016.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2016 & strokeregion$region=="Amazon", 2016.6, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2017 & strokeregion$region=="Rest Coast", 2017.2, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2017 & strokeregion$region=="Highlands", 2017.4, strokeregion$year)
strokeregion$year <- ifelse(strokeregion$year==2017 & strokeregion$region=="Amazon", 2017.6, strokeregion$year)

names(strokeregion)[names(strokeregion)=="region"]<-"Region"
regionstroke <- ggplot(strokeregion, aes(x=year, y=losmediaan, color=Region))+
  scale_color_hue(labels = c("Lima/Callao","Rest Coast (p<0.001)", "Amazon (p<0.001)", "Highlands (p<0.001)"))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(strokeregion, mapping = aes(ymin=losq1, ymax=losq3), size=2)+
  annotate("text",x=2002.3,y=0,label="2002")+
  annotate("text",x=2003.3,y=0,label="2003")+
  annotate("text",x=2004.3,y=0,label="2004")+
  annotate("text",x=2005.3,y=0,label="2005")+
  annotate("text",x=2006.3,y=0,label="2006")+
  annotate("text",x=2007.3,y=0,label="2007")+
  annotate("text",x=2008.3,y=0,label="2008")+
  annotate("text",x=2009.3,y=0,label="2009")+
  annotate("text",x=2010.3,y=0,label="2010")+
  annotate("text",x=2011.3,y=0,label="2011")+
  annotate("text",x=2012.3,y=0,label="2012")+
  annotate("text",x=2013.3,y=0,label="2013")+
  annotate("text",x=2014.3,y=0,label="2014")+
  annotate("text",x=2015.3,y=0,label="2015")+
  annotate("text",x=2016.3,y=0,label="2016")+
  annotate("text",x=2017.3,y=0,label="2017")
plot(regionstroke)

strokecie1 <- subset(stroke, stroke$cid10=="I60", select = c(year, los))
strokecie1 <- strokecie1%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie2 <- subset(stroke, stroke$cid10=="I61", select = c(year, los))
strokecie2 <- strokecie2%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie3 <- subset(stroke, stroke$cid10=="I63", select = c(year, los))
strokecie3 <- strokecie3%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie4 <- subset(stroke, stroke$cid10=="I64", select = c(year, los))
strokecie4 <- strokecie4%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie1$icd <- 1
strokecie2$icd <- 2
strokecie3$icd <- 3
strokecie4$icd <- 4
strokecie <- rbind(strokecie1, strokecie2, strokecie3, strokecie4)
strokecie$icd <- factor(strokecie$icd, levels = c(1,2,3,4), labels = c("Subarachnoid Hemorrhage (I60)", "Intra-cerebral Hemorrhage (I61)", "Cerebral Infarction (I63)", "Stroke not specified (I64)"))
rm(strokecie1, strokecie2, strokecie3, strokecie4)

strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2002.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="Cerebral Infarction (I63)", 2002.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="Stroke not specified (I64)", 2002.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2003.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="Cerebral Infarction (I63)", 2003.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="Stroke not specified (I64)", 2003.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2004.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="Cerebral Infarction (I63)", 2004.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="Stroke not specified (I64)", 2004.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2005.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="Cerebral Infarction (I63)", 2005.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="Stroke not specified (I64)", 2005.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2006.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="Cerebral Infarction (I63)", 2006.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="Stroke not specified (I64)", 2006.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2007.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="Cerebral Infarction (I63)", 2007.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="Stroke not specified (I64)", 2007.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2008.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="Cerebral Infarction (I63)", 2008.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="Stroke not specified (I64)", 2008.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2009.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="Cerebral Infarction (I63)", 2009.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="Stroke not specified (I64)", 2009.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2010.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="Cerebral Infarction (I63)", 2010.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="Stroke not specified (I64)", 2010.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2011.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="Cerebral Infarction (I63)", 2011.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="Stroke not specified (I64)", 2011.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2012.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="Cerebral Infarction (I63)", 2012.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="Stroke not specified (I64)", 2012.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2013.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="Cerebral Infarction (I63)", 2013.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="Stroke not specified (I64)", 2013.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2014.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="Cerebral Infarction (I63)", 2014.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="Stroke not specified (I64)", 2014.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2015.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="Cerebral Infarction (I63)", 2015.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="Stroke not specified (I64)", 2015.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2016.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="Cerebral Infarction (I63)", 2016.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="Stroke not specified (I64)", 2016.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="Intra-cerebral Hemorrhage (I61)", 2017.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="Cerebral Infarction (I63)", 2017.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="Stroke not specified (I64)", 2017.6, strokecie$year)

names(strokecie)[names(strokecie)=="icd"]<-"ICD10"

icdstroke <- ggplot(strokecie, aes(x=year, y=losmediaan, color=ICD10))+
  scale_color_hue(labels = c("Subarachnoid Hemorrhage (I60)", "Intra-cerebral Hemorrhage (I61)", "Cerebral Infarction (I63, p<0.001)*", "Stroke not specified (I64, p<0.001)*"))+
  labs(color="ICD-10")+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24), limits = c(0,24))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x  = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(strokecie, mapping = aes(ymin=losq1, ymax=losq3),size=2)+
  annotate("text",x=2002.3,y=0,label="2002")+
  annotate("text",x=2003.3,y=0,label="2003")+
  annotate("text",x=2004.3,y=0,label="2004")+
  annotate("text",x=2005.3,y=0,label="2005")+
  annotate("text",x=2006.3,y=0,label="2006")+
  annotate("text",x=2007.3,y=0,label="2007")+
  annotate("text",x=2008.3,y=0,label="2008")+
  annotate("text",x=2009.3,y=0,label="2009")+
  annotate("text",x=2010.3,y=0,label="2010")+
  annotate("text",x=2011.3,y=0,label="2011")+
  annotate("text",x=2012.3,y=0,label="2012")+
  annotate("text",x=2013.3,y=0,label="2013")+
  annotate("text",x=2014.3,y=0,label="2014")+
  annotate("text",x=2015.3,y=0,label="2015")+
  annotate("text",x=2016.3,y=0,label="2016")+
  annotate("text",x=2017.3,y=0,label="2017")

plot(icdstroke)

###ONlY CIE-10

strokecie1 <- subset(stroke, stroke$cid10=="I60", select = c(year, los))
strokecie1 <- strokecie1%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie2 <- subset(stroke, stroke$cid10=="I61", select = c(year, los))
strokecie2 <- strokecie2%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie3 <- subset(stroke, stroke$cid10=="I63", select = c(year, los))
strokecie3 <- strokecie3%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie4 <- subset(stroke, stroke$cid10=="I64", select = c(year, los))
strokecie4 <- strokecie4%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                      losq1=quantile(los, c(.25)),
                                                      losq3=quantile(los, c(.75)))
strokecie1$icd <- 1
strokecie2$icd <- 2
strokecie3$icd <- 3
strokecie4$icd <- 4
strokecie <- rbind(strokecie1, strokecie2, strokecie3, strokecie4)
strokecie$icd <- factor(strokecie$icd, levels = c(1,2,3,4), labels = c("I60", "I61", "I63", "I64"))
rm(strokecie1, strokecie2, strokecie3, strokecie4)

strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="I61", 2002.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="I63", 2002.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2002 & strokecie$icd=="I64", 2002.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="I61", 2003.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="I63", 2003.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2003 & strokecie$icd=="I64", 2003.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="I61", 2004.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="I63", 2004.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2004 & strokecie$icd=="I64", 2004.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="I61", 2005.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="I63", 2005.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2005 & strokecie$icd=="I64", 2005.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="I61", 2006.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="I63", 2006.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2006 & strokecie$icd=="I64", 2006.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="I61", 2007.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="I63", 2007.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2007 & strokecie$icd=="I64", 2007.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="I61", 2008.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="I63", 2008.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2008 & strokecie$icd=="I64", 2008.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I61", 2009.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I63", 2009.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I64", 2009.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I61", 2010.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I63", 2010.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I64", 2010.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I61", 2011.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I63", 2011.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2009 & strokecie$icd=="I64", 2011.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I61", 2012.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I63", 2012.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2010 & strokecie$icd=="I64", 2012.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="I61", 2011.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="I63", 2011.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2011 & strokecie$icd=="I64", 2011.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="I61", 2012.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="I63", 2012.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2012 & strokecie$icd=="I64", 2012.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="I61", 2013.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="I63", 2013.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2013 & strokecie$icd=="I64", 2013.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="I61", 2014.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="I63", 2014.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2014 & strokecie$icd=="I64", 2014.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="I61", 2015.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="I63", 2015.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2015 & strokecie$icd=="I64", 2015.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="I61", 2016.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="I63", 2016.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2016 & strokecie$icd=="I64", 2016.6, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="I61", 2017.2, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="I63", 2017.4, strokecie$year)
strokecie$year <- ifelse(strokecie$year==2017 & strokecie$icd=="I64", 2017.6, strokecie$year)


icdstroke <- ggplot2::ggplot(strokecie, aes(x=year, y=losmediaan, color=icd))+
  labs(color="ICD-10")+
  scale_color_hue(labels = c("I60","I61","I63 (p<0.001)*", "I64 (p<0.001)*"))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24), limits = c(0,24))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x  = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  geom_errorbar(strokecie, mapping = aes(ymin=losq1, ymax=losq3),size=2)+
  annotate("text",x=2002.3,y=0,label="2002")+
  annotate("text",x=2003.3,y=0,label="2003")+
  annotate("text",x=2004.3,y=0,label="2004")+
  annotate("text",x=2005.3,y=0,label="2005")+
  annotate("text",x=2006.3,y=0,label="2006")+
  annotate("text",x=2007.3,y=0,label="2007")+
  annotate("text",x=2008.3,y=0,label="2008")+
  annotate("text",x=2009.3,y=0,label="2009")+
  annotate("text",x=2010.3,y=0,label="2010")+
  annotate("text",x=2011.3,y=0,label="2011")+
  annotate("text",x=2012.3,y=0,label="2012")+
  annotate("text",x=2013.3,y=0,label="2013")+
  annotate("text",x=2014.3,y=0,label="2014")+
  annotate("text",x=2015.3,y=0,label="2015")+
  annotate("text",x=2016.3,y=0,label="2016")+
  annotate("text",x=2017.3,y=0,label="2017")
plot(icdstroke)

###SUMMARY OF LOS BY HOSPITAL LEVEL
strokelevel1 <- subset(stroke, stroke$level=="I, II", select = c(year, los))
strokelevel1 <- strokelevel1%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                          losq1=quantile(los, c(.25)),
                                                          losq3=quantile(los, c(.75)))
strokelevel2 <- subset(stroke, stroke$level=="III", select = c(year, los))
strokelevel2 <- strokelevel2%>%group_by(year)%>%dplyr::summarise(losmediaan=median(los,na.rm=TRUE), 
                                                          losq1=quantile(los, c(.25)),
                                                          losq3=quantile(los, c(.75)))
strokelevel1$level <- 1
strokelevel2$level <- 2
strokelevel <- rbind(strokelevel1, strokelevel2)
strokelevel$level <- factor(strokelevel$level, levels = c(1,2), labels = c("I,II", "III"))
rm(strokelevel1, strokelevel2)

strokelevel$year <- ifelse(strokelevel$year==2002 & strokelevel$level=="III", 2002.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2003 & strokelevel$level=="III", 2003.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2004 & strokelevel$level=="III", 2004.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2005 & strokelevel$level=="III", 2005.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2006 & strokelevel$level=="III", 2006.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2007 & strokelevel$level=="III", 2007.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2008 & strokelevel$level=="III", 2008.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2009 & strokelevel$level=="III", 2009.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2010 & strokelevel$level=="III", 2010.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2011 & strokelevel$level=="III", 2011.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2012 & strokelevel$level=="III", 2012.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2013 & strokelevel$level=="III", 2013.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2014 & strokelevel$level=="III", 2014.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2015 & strokelevel$level=="III", 2015.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2016 & strokelevel$level=="III", 2016.2, strokelevel$year)
strokelevel$year <- ifelse(strokelevel$year==2017 & strokelevel$level=="III", 2017.2, strokelevel$year)

names(strokelevel)[names(strokelevel)=="level"]<-"Level"
levelstroke <- ggplot(strokelevel, aes(x=year, y=losmediaan, color=Level))+
  scale_color_hue(labels = c("I,II (p<0.001)","III"))+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  geom_errorbar(strokelevel, mapping = aes(ymin=losq1, ymax=losq3),size=2)+
  annotate("text",x=2002.1,y=0,label="2002")+
  annotate("text",x=2003.1,y=0,label="2003")+
  annotate("text",x=2004.1,y=0,label="2004")+
  annotate("text",x=2005.1,y=0,label="2005")+
  annotate("text",x=2006.1,y=0,label="2006")+
  annotate("text",x=2007.1,y=0,label="2007")+
  annotate("text",x=2008.1,y=0,label="2008")+
  annotate("text",x=2009.1,y=0,label="2009")+
  annotate("text",x=2010.1,y=0,label="2010")+
  annotate("text",x=2011.1,y=0,label="2011")+
  annotate("text",x=2012.1,y=0,label="2012")+
  annotate("text",x=2013.1,y=0,label="2013")+
  annotate("text",x=2014.1,y=0,label="2014")+
  annotate("text",x=2015.1,y=0,label="2015")+
  annotate("text",x=2016.1,y=0,label="2016")+
  annotate("text",x=2017.1,y=0,label="2017")
plot(levelstroke)

# Load libraries to use grid.arrange 

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)

grid.arrange(ggarrange(losall, sexstroke,agestroke,icdstroke + rremove("x.text"), 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2),left=textGrob("Median Length of Hospitalization [days (IQR)]", rot = 90, vjust = 1))

###SUPPLEMENTARY MATERIAL

grid.arrange(ggarrange(regionstroke, levelstroke + rremove("x.text"), 
                       labels = c("E", "F"),
                       ncol = 2, nrow = 1),left=textGrob("Median Length of Hospitalization [days (IQR)]", rot = 90, vjust = 1))

plot(losall)
plot(sexstroke)
plot(agestroke)
plot(regionstroke)
plot(levelstroke)
plot(icdstroke)

###DESCRIPTIVE ANALYSIS
attach(y2016deaths)
table(year)
table(sexo)
tapply(sexo, year, table)
sd(edad)
summary(edad)
sd(edad)
tapply(edad,year,summary)
tapply(agecat,year,table)
tapply(edad,year,sd)
addmargins(table(cid10))
summary(los)
tapply(los, year,summary)
tapply(condicion, agecat, summary)
detach(y2016deaths)

###COMPARATION I60-1~ I63 ~ I64
#levels(as.factor(stroke$cid10))
#strokeI60_I61 <- stroke %>% dplyr::filter(cid10 %in% c("I60","I61"))
#strokeI63 <- stroke %>% filter(cid10=="I63")
#strokeI64 <- stroke %>% filter(cid10=="I64")

library(dplyr)

stroke$cid102 <- ifelse(stroke$cid10=="I60" | stroke$cid10=="I61",0, ifelse(stroke$cid10=="I63",1,ifelse(stroke$cid10=="I64",2,NA))) %>% 
  factor(levels = c(0,1,2),labels = c("I60-I61","I63","I64"))
str(stroke$cid102)
levels(as.factor(stroke$cid102))
stroke$los <- as.numeric(stroke$los)

stroke$cid102
kruskal.test(los~cid102, data = stroke)

str(stroken_ve)

levels(as.factor(stroken_ve$cid10))
5307 +  5810 + 10700

#normalidad
#library(nortest)
#ad.test(strokeI601$los) # p<0.05
#ad.test(strokeI63$los) # p<0.05
#ad.test(strokeI64$los) # p<0.05

#test Kruskal–Wallis test

kruskal.test(los ~ cid10,
             data = stroke)

###DUNT TEST FOR MULTPLE COMPARATIONS  

#CIE10~los
library(dunn.test)
attach(stroke)
View(stroke)
stroke$cid10 <- as.factor(stroke$cid10)
dunn.test(los, cid10, method="bonferroni", list=TRUE)

#Year~los
kruskal.test(los ~ year,
             data = stroke)
stroke$year <- as.factor(stroke$year)
dunn.test(los, year, method="bonferroni", list=TRUE)

#Age~los
kruskal.test(los ~ agecat,
             data = stroke)
stroke$agecat <- as.factor(stroke$agecat)
dunn.test(los, agecat, method="bonferroni", list=TRUE)

#sex~los

wilcox.test(los ~ sexo, data = stroke, exact = FALSE)

#region~los
kruskal.test(los ~ regions,
             data = stroke)
dunn.test(los, regions, method="bonferroni", list=TRUE)

#level~los
wilcox.test(los ~ level, data = stroke, exact = FALSE)

#Bonferroni adjusted p-values
#p.adjust(2.2e-16, n=3)
#pairwise.t.test(los, cid10, p.adjust = "bonferroni")

##MORTALITY ANALYSIS
##Case fatality rate ~ CI

library(epitools)
pois.approx(757, pt = 6566, conf.level = 0.95)
pois.approx(331, pt = 3305, conf.level = 0.95) #men
pois.approx(426, pt = 3261, conf.level = 0.95) #women


table(y2016deaths$sexo, y2016deaths$condicion)
table(y2016deaths$sexo)

#757 6566 0.1152909 0.107078 0.1235038       0.95

#CDR~sex
installed.packages(rateratio.test)
#library(rateratio.test)
#rateratio.test(c(331,426),c(3305,3261),conf.level = 0.95)
pois.approx(331, pt = 3305, conf.level = 0.95)
pois.approx(426, pt = 3261, conf.level = 0.95)
#Hombres: 331+2974 3305
#Mujeres: 426+2835 3261

#CDR-ages
#<55     122       1054  1176
#55-74   323       2588  2911
#75+     312       2167  2479
pois.approx(122, pt = 1176, conf.level = 0.95)
pois.approx(323, pt = 2911, conf.level = 0.95)
pois.approx(312, pt = 2479, conf.level = 0.95)

###LOAD DATA

y2016deaths <- read.csv("./death/y2016_7deaths.cvs")

###GENERATE EXPOSURE
y2016deaths$exposure <- ifelse(y2016deaths$cid10=="I63",0,
                               ifelse(y2016deaths$cid10=="I60",1,
                                      ifelse(y2016deaths$cid10=="I61",2,NA)))
y2016deaths$exposure <- factor(y2016deaths$exposure, levels = c(0,1,2), labels = c("I63", "I60", "I61"))

###DESCRIPTIVE
addmargins(table(y2016deaths$condicion))
table(y2016deaths$sexo, y2016deaths$condicion)
summary(table(y2016deaths$sexo, y2016deaths$condicion))
table(y2016deaths$agecat, y2016deaths$condicion)
summary(table(y2016deaths$agecat, y2016deaths$condicion))
table(y2016deaths$regions, y2016deaths$condicion)
summary(table(y2016deaths$regions, y2016deaths$condicion))
table(y2016deaths$level, y2016deaths$condicion)
summary(table(y2016deaths$level, y2016deaths$condicion))
table(y2016deaths$cid10, y2016deaths$condicion)
summary(table(y2016deaths$cid10, y2016deaths$condicion))
tapply(y2016deaths$los, y2016deaths$condicion, summary)
kruskal.test(los ~ condicion, data = y2016deaths) 


##SURVIVAL ANALYSIS

##GENERATE SURVIVAL OBJECT
y2016deaths$survivalobj0 <- with(y2016deaths,Surv(los,condicion=="Death"))
summary(y2016deaths$survivalobj0)

###CHECK PROPORTIONAL HAZARD ASSUMPTION 
##(p<0.05 indicates a violation of the assumption) IN CRUDE
prop.res.cox1<-cox.zph(coxph(survivalobj0 ~ exposure+strata(regions, level),data=y2016deaths))
prop.res.cox1 #VIOLATION OF THE ASSUMPTION

prop.res.cox2 <- cox.zph(coxph(survivalobj0 ~ exposure+sexo+edad+strata(regions, level),data=y2016deaths))
prop.res.cox2 #VIOLATION OF THE ASSUMPTION

##COX MODELS
cox1 <- coxph(survivalobj0 ~ exposure++strata(regions, level),data=y2016deaths)
summary(cox1)

cox2 <- coxph(survivalobj0 ~ exposure+sexo+edad+strata(regions, level),data=y2016deaths)
summary(cox2)

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
  
     