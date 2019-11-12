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

###FUNCTIONS
number_ticks <- function(n) {function(limits) pretty(limits, n)}

g_legend<-function(a.gplot){
  tmp<-ggplot_gtable(ggplot_build(a.gplot))
  leg<-which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend<-tmp$grobs[[leg]]
  return(legend)}

###LOAD DATA
setwd("/home/init5/R/Stroke/")
stroke <- read.csv("./stroke.csv")
levels(as.factor(stroke$year))
###SUMMARY OF LOS BY YEAR
by_year <- stroke %>%
  group_by(year) %>%
  mutate(los=as.numeric(los)) %>%
  dplyr::summarise("Q1" = quantile(los, probs=0.25, na.rm = TRUE), 
            "Median" = quantile(los, probs=0.5,na.rm = TRUE), 
            "Q3" = quantile(los, probs=0.75,na.rm = TRUE), 
            "n"=n())
#Count
stroke %>% dplyr::count(los)

#Variables
stroke %>% glimpse()
losall <- ggplot2::ggplot(by_year, aes(x=year, y=Median,colour=factor(year)))+
  scale_color_hue(labels = c("2002 (p=1.0)","2003 (p=1.0)","2004 (p=1.0)","2005 (p=0.410)","2006 (p=1.0)","2007 (p=1.0)","2008 (p=1.0)","2009 (p=1.0)","2010 (p<0.001)","2011 (p=1.0)","2012 (p=0.488)","2013 (p=0.280)", "2014 (p=1.0)","2015 (p=0.017)","2016", "2017 (p=1.0)"))+
  labs(color="Year")+
  #scale_x_discrete(limits=c("2011 (p<0.05)","2012 (p<0.001)","2013 (p<0.001)", "2014 (p>0.05)","2015 (p<0.001)","2016 (p>0.05)", "2017"))+
  geom_point(size=4)+
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
  geom_errorbar(by_year, mapping = aes(ymin=Q1, ymax=Q3), width=0.2, size=2)+
  annotate("text",x=2002,y=0,label="2002")+
  annotate("text",x=2003,y=0,label="2003")+
  annotate("text",x=2004,y=0,label="2004")+
  annotate("text",x=2005,y=0,label="2005")+
  annotate("text",x=2006,y=0,label="2006")+
  annotate("text",x=2007,y=0,label="2007")+
  annotate("text",x=2008,y=0,label="2008")+
  annotate("text",x=2009,y=0,label="2009")+
  annotate("text",x=2010,y=0,label="2010")+
  annotate("text",x=2011,y=0,label="2011")+
  annotate("text",x=2012,y=0,label="2012")+
  annotate("text",x=2013,y=0,label="2013")+
  annotate("text",x=2014,y=0,label="2014")+
  annotate("text",x=2015,y=0,label="2015")+
  annotate("text",x=2016,y=0,label="2016")+
  annotate("text",x=2017,y=0,label="2017")
plot(losall)
###SUMMARY OF LOS BY SEX
#mimale <- subset(mi, mi$sexo=="Men", select = c(year, los))
#mimale <- mimale%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#mifemale <- subset(mi, mi$sexo=="Women", select = c(year, los))
#mifemale <- mifemale%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#mimale$sex <- 0
#mifemale$sex <- 1
#misex <- rbind(mimale, mifemale)
#misex$sex <- factor(misex$sex, levels = c(0,1), labels = c("Men", "Women"))
#rm(mimale, mifemale)

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

#misex$year <- ifelse(misex$year==2011 & misex$sex=="Men", 2011.5, misex$year)
#misex$year <- ifelse(misex$year==2012 & misex$sex=="Men", 2012.5, misex$year)
#misex$year <- ifelse(misex$year==2013 & misex$sex=="Men", 2013.5, misex$year)
#misex$year <- ifelse(misex$year==2014 & misex$sex=="Men", 2014.5, misex$year)
#misex$year <- ifelse(misex$year==2015 & misex$sex=="Men", 2015.5, misex$year)
#misex$year <- ifelse(misex$year==2016 & misex$sex=="Men", 2016.5, misex$year)
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

#sexmi <- ggplot(misex, aes(x=year, y=losmediaan, color=sex))+
#  geom_point(size=4)+
#  #geom_point(aes(color=Disease), alpha=1, size=5)+
#  #geom_text(label=losmistroke$Median, size=3, color="white")
#  #geom_line()+
#  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
#  theme_bw()+
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#  #scale_x_continuous(breaks=number_ticks(12))+
#  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
#  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
#  geom_errorbar(misex, mapping = aes(ymin=losq1, ymax=losq3), size=2)+
#  annotate("text",x=2011.3,y=0,label="2011")+
#  annotate("text",x=2012.3,y=0,label="2012")+
#  annotate("text",x=2013.3,y=0,label="2013")+
#  annotate("text",x=2014.3,y=0,label="2014")+
#  annotate("text",x=2015.3,y=0,label="2015")+
#  annotate("text",x=2016.3,y=0,label="2016")+
#  annotate("text",x=2011.5,y=21,label="MI")

sexstroke <- ggplot(strokesex, aes(x=year, y=losmediaan, color=Sex))+
  scale_color_hue(labels = c("Men (p=0.051)", "Women"))+
    geom_point(size=4)+
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
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
plot(sexstroke)
#sexlegend <- g_legend(sexmi)

#sex <- grid.arrange(arrangeGrob(sexmi+theme(legend.position = "none"),
#                         sexstroke+theme(legend.position = "none"),
#                         nrow=1), sexlegend,nrow=2,heights=c(5,1))

###SUMMARY OF LOS BY AGECAT
#miage1 <- subset(mi, mi$agecat=="<55", select = c(year, los))
#miage1 <- miage1%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miage2 <- subset(mi, mi$agecat=="55-74", select = c(year, los))
#miage2 <- miage2%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miage3 <- subset(mi, mi$agecat=="75+", select = c(year, los))
#miage3 <- miage3%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miage1$agecat <- 1
#miage2$agecat <- 2
#miage3$agecat <- 3
#miage <- rbind(miage1, miage2, miage3)
#miage$agecat <- factor(miage$agecat, levels = c(1,2,3), labels = c("<55", "55-74", "75+"))
#rm(miage1, miage2, miage3)

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

#miage$year <- ifelse(miage$year==2011 & miage$agecat=="55-74", 2011.3, miage$year)
#miage$year <- ifelse(miage$year==2011 & miage$agecat=="75+", 2011.6, miage$year)
#miage$year <- ifelse(miage$year==2012 & miage$agecat=="55-74", 2012.3, miage$year)
#miage$year <- ifelse(miage$year==2012 & miage$agecat=="75+", 2012.6, miage$year)
#miage$year <- ifelse(miage$year==2013 & miage$agecat=="55-74", 2013.3, miage$year)
#miage$year <- ifelse(miage$year==2013 & miage$agecat=="75+", 2013.6, miage$year)
#miage$year <- ifelse(miage$year==2014 & miage$agecat=="55-74", 2014.3, miage$year)
#miage$year <- ifelse(miage$year==2014 & miage$agecat=="75+", 2014.6, miage$year)
#miage$year <- ifelse(miage$year==2015 & miage$agecat=="55-74", 2015.3, miage$year)
#miage$year <- ifelse(miage$year==2015 & miage$agecat=="75+", 2015.6, miage$year)
#miage$year <- ifelse(miage$year==2016 & miage$agecat=="55-74", 2016.3, miage$year)
#miage$year <- ifelse(miage$year==2016 & miage$agecat=="75+", 2016.6, miage$year)

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

#names(miage)[names(miage)=="agecat"]<-"age"
names(strokeage)[names(strokeage)=="agecat"]<-"Age"

#agemi <- ggplot(miage, aes(x=year, y=losmediaan, color=age))+
#  geom_point(size=4)+
#  #geom_point(aes(color=Disease), alpha=1, size=5)+
#  #geom_text(label=losmistroke$Median, size=3, color="white")
#  #geom_line()+
#  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
#  theme_bw()+
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#  #scale_x_continuous(breaks=number_ticks(12))+
#  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
#  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
#  geom_errorbar(miage, mapping = aes(ymin=losq1, ymax=losq3), size=2)+
#  annotate("text",x=2011.3,y=0,label="2011")+
#  annotate("text",x=2012.3,y=0,label="2012")+
#  annotate("text",x=2013.3,y=0,label="2013")+
#  annotate("text",x=2014.3,y=0,label="2014")+
#  annotate("text",x=2015.3,y=0,label="2015")+
#  annotate("text",x=2016.3,y=0,label="2016")+
#  annotate("text",x=2011.5,y=21,label="MI")

agestroke <- ggplot(strokeage, aes(x=year, y=losmediaan, color=Age))+
  scale_color_hue(labels = c("<55", "55-74 (p<0.001)", "75+ (p<0.001)"))+
  geom_point(size=4)+
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18), limits = c(0,18))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
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
#agelegend <- g_legend(agemi)

#age <- grid.arrange(arrangeGrob(agemi+theme(legend.position = "none"),
#                                agestroke+theme(legend.position = "none"),
#                                nrow=1), agelegend,nrow=2,heights=c(5,1))

###SUMMARY OF LOS BY REGIONS
#miregion1 <- subset(mi, mi$regions=="Lima/Callao", select = c(year, los))
#miregion1 <- miregion1%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miregion2 <- subset(mi, mi$regions=="Resto Costa", select = c(year, los))
#miregion2 <- miregion2%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miregion3 <- subset(mi, mi$regions=="Selva", select = c(year, los))
#miregion3 <- miregion3%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miregion4 <- subset(mi, mi$regions=="Sierra", select = c(year, los))
#miregion4 <- miregion4%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                              losq1=quantile(los, c(.25)),
#                                              losq3=quantile(los, c(.75)))
#miregion1$region <- 1
#miregion2$region <- 2
#miregion3$region <- 3
#miregion4$region <- 4
#miregion <- rbind(miregion1, miregion2, miregion3, miregion4)
#miregion$region <- factor(miregion$region, levels = c(1,2,3,4), labels = c("Lima/Callao", "Rest Coast", "Amazon", "Highlands"))
#rm(miregion1, miregion2, miregion3, miregion4)

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

#miregion$year <- ifelse(miregion$year==2011 & miregion$region=="Rest Coast", 2011.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2011 & miregion$region=="Highlands", 2011.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2011 & miregion$region=="Amazon", 2011.6, miregion$year)
#miregion$year <- ifelse(miregion$year==2012 & miregion$region=="Rest Coast", 2012.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2012 & miregion$region=="Highlands", 2012.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2012 & miregion$region=="Amazon", 2012.6, miregion$year)
#miregion$year <- ifelse(miregion$year==2013 & miregion$region=="Rest Coast", 2013.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2013 & miregion$region=="Highlands", 2013.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2013 & miregion$region=="Amazon", 2013.6, miregion$year)
#miregion$year <- ifelse(miregion$year==2014 & miregion$region=="Rest Coast", 2014.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2014 & miregion$region=="Highlands", 2014.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2014 & miregion$region=="Amazon", 2014.6, miregion$year)
#miregion$year <- ifelse(miregion$year==2015 & miregion$region=="Rest Coast", 2015.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2015 & miregion$region=="Highlands", 2015.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2015 & miregion$region=="Amazon", 2015.6, miregion$year)
#miregion$year <- ifelse(miregion$year==2016 & miregion$region=="Rest Coast", 2016.2, miregion$year)
#miregion$year <- ifelse(miregion$year==2016 & miregion$region=="Highlands", 2016.4, miregion$year)
#miregion$year <- ifelse(miregion$year==2016 & miregion$region=="Amazon", 2016.6, miregion$year)

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

#regionmi <- ggplot(miregion, aes(x=year, y=losmediaan, color=region))+
#  geom_point(size=4)+
#  #geom_point(aes(color=Disease), alpha=1, size=5)+
#  #geom_text(label=losmistroke$Median, size=3, color="white")
#  #geom_line()+
#  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
#  theme_bw()+
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#  #scale_x_continuous(breaks=number_ticks(12))+
#  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
#  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
#  geom_errorbar(miregion, mapping = aes(ymin=losq1, ymax=losq3),size=2)+
#  annotate("text",x=2011.3,y=0,label="2011")+
#  annotate("text",x=2012.3,y=0,label="2012")+
#  annotate("text",x=2013.3,y=0,label="2013")+
#  annotate("text",x=2014.3,y=0,label="2014")+
#  annotate("text",x=2015.3,y=0,label="2015")+
#  annotate("text",x=2016.3,y=0,label="2016")+
#  annotate("text",x=2011.5,y=21,label="MI")
names(strokeregion)[names(strokeregion)=="region"]<-"Region"
regionstroke <- ggplot(strokeregion, aes(x=year, y=losmediaan, color=Region))+
  scale_color_hue(labels = c("Lima/Callao","Rest Coast (p<0.001)", "Amazon (p<0.001)", "Highlands (p<0.001)"))+
  geom_point(size=4)+
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x =  element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
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
#regionlegend <- g_legend(regionmi)

#region <- grid.arrange(arrangeGrob(regionmi+theme(legend.position = "none"),
#                                regionstroke+theme(legend.position = "none"),
#                                nrow=1), regionlegend,nrow=2,heights=c(5,1))

###SUMMARY OF LOS BY ICD10
#micie1 <- subset(mi, mi$cid10=="I20", select = c(year, los))
#micie1 <- micie1%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                                    losq1=quantile(los, c(.25)),
#                                                    losq3=quantile(los, c(.75)))
#micie2 <- subset(mi, mi$cid10=="I21", select = c(year, los))
#micie2 <- micie2%>%group_by(year)%>%summarise(losmediaan=median(los,na.rm=TRUE), 
#                                                    losq1=quantile(los, c(.25)),
#                                                    losq3=quantile(los, c(.75)))
#micie1$icd <- 1
#micie2$icd <- 2
#micie <- rbind(micie1, micie2)
#micie$icd <- factor(micie$icd, levels = c(1,2), labels = c("I20", "I21"))
#rm(micie1, micie2)

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

#micie$year <- ifelse(micie$year==2011 & micie$icd=="I20", 2011.3, micie$year)
#micie$year <- ifelse(micie$year==2011 & micie$icd=="I21", 2011.6, micie$year)
#micie$year <- ifelse(micie$year==2012 & micie$icd=="I20", 2012.3, micie$year)
#micie$year <- ifelse(micie$year==2012 & micie$icd=="I21", 2012.6, micie$year)
#micie$year <- ifelse(micie$year==2013 & micie$icd=="I20", 2013.3, micie$year)
#micie$year <- ifelse(micie$year==2013 & micie$icd=="I21", 2013.6, micie$year)
#micie$year <- ifelse(micie$year==2014 & micie$icd=="I20", 2014.3, micie$year)
#micie$year <- ifelse(micie$year==2014 & micie$icd=="I21", 2014.6, micie$year)
#micie$year <- ifelse(micie$year==2015 & micie$icd=="I20", 2015.3, micie$year)
#micie$year <- ifelse(micie$year==2015 & micie$icd=="I21", 2015.6, micie$year)
#micie$year <- ifelse(micie$year==2016 & micie$icd=="I20", 2016.3, micie$year)
#micie$year <- ifelse(micie$year==2016 & micie$icd=="I21", 2016.6, micie$year)

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

#icdmi <- ggplot(micie, aes(x=year, y=losmediaan, color=icd))+
#  geom_point(size=4)+
#  #geom_point(aes(color=Disease), alpha=1, size=5)+
#  #geom_text(label=losmistroke$Median, size=3, color="white")
#  #geom_line()+
#  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
#  theme_bw()+
#  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#  #scale_x_continuous(breaks=number_ticks(12))+
#  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
#  labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
#  geom_errorbar(micie, mapping = aes(ymin=losq1, ymax=losq3),size=2)+
#  annotate("text",x=2011.3,y=0,label="2011")+
#  annotate("text",x=2012.3,y=0,label="2012")+
#  annotate("text",x=2013.3,y=0,label="2013")+
#  annotate("text",x=2014.3,y=0,label="2014")+
#  annotate("text",x=2015.3,y=0,label="2015")+
#  annotate("text",x=2016.3,y=0,label="2016")+
#  annotate("text",x=2011.5,y=21,label="MI")

names(strokecie)[names(strokecie)=="icd"]<-"ICD10"

icdstroke <- ggplot(strokecie, aes(x=year, y=losmediaan, color=ICD10))+
  scale_color_hue(labels = c("Subarachnoid Hemorrhage (I60)", "Intra-cerebral Hemorrhage (I61)", "Cerebral Infarction (I63, p<0.001)*", "Stroke not specified (I64, p<0.001)*"))+
  labs(color="ICD-10")+
  geom_point(size=4)+
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24), limits = c(0,24))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x  = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
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

#icd <- grid.arrange(arrangeGrob(icdmi,
#                                icdstroke,
#                                nrow=1))

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
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21,24), limits = c(0,24))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x  = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
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
  #geom_point(aes(color=Disease), alpha=1, size=5)+
  #geom_text(label=losmistroke$Median, size=3, color="white")
  #geom_line()+
  scale_y_continuous(breaks = c(0,3,6,9,12,15,18,21), limits = c(0,21))+
  theme_bw()+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank())+
  #scale_x_continuous(breaks=number_ticks(12))+
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016))+
  #labs(x="Years", y="Median Length of Hospitalization [days (IQR)]")+
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

#grid.arrange(arrangeGrob(losall, agestroke,
                        # sexstroke, icdstroke,top = textGrob("A","B","C","D"),
                        # nrow=2),
             #left = textGrob("Median Length of Hospitalization [days (IQR)]", rot = 90, vjust = 1))

grid.arrange(ggarrange(losall, sexstroke,agestroke,icdstroke + rremove("x.text"), 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2),left=textGrob("Median Length of Hospitalization [days (IQR)]", rot = 90, vjust = 1))

###SUPPLEMENTARY MATERIAL

#grid.arrange(arrangeGrob(regionstroke, levelstroke,
                         #nrow=1),
             #left = textGrob("Median Length of Hospitalization [days (IQR)]", rot = 90, vjust = 1)) 
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
stroke$cid10 <- ifelse(stroke$cid10=="I60" | stroke$cid10=="I61",0, ifelse(stroke$cid10=="I63",1,ifelse(stroke$cid10=="I64",2,NA)))
stroke$cid10 <- factor(stroke$cid10,levels = c(0,1,2),labels = c("I60-I61","I63","I64"))
dplyr::count(stroke)
levels(as.factor(stroke$cid10))

stroken_ve$cid10 <- ifelse(stroken_ve$cid10=="I60" | stroken_ve$cid10=="I61",0, ifelse(stroken_ve$cid10=="I63",1,ifelse(stroken_ve$cid10=="I64",2,NA)))
stroken_ve$cid10 <- factor(stroken_ve$cid10,levels = c(0,1,2),labels = c("I60-I61","I63","I64"))
levels(as.factor(stroken_ve$cid10))

5307 +  5810 + 10700
#normalidad
#library(nortest)
#ad.test(strokeI601$los) # p<0.05
#ad.test(strokeI63$los) # p<0.05
#ad.test(strokeI64$los) # p<0.05

#http://www.biostathandbook.com/linearregression.html
#test Kruskal–Wallis test

kruskal.test(los ~ cid10,
             data = stroke)

#p-value < 2.2e-16
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

############################
#####MORTALITY ANALYSIS#####
############################

###Case fatality rate ~ CI
#===========================
installed.packages("epitools")
library(epitools)
pois.approx(757, pt = 6566, conf.level = 0.95)
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


###SURVIVAL ANALYSIS

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

# Visualize with survminer
ggsurvplot(losfit, data = y2016deaths, main = "Survival curve",
           submain = "Based on Kaplan-Meier estimates", legend = c(0.1, 0.2),
           legend.title = "Strata",  risk.table = TRUE, xlim = c(0,30),
           break.time.by = 5, risk.table.y.text.col = T, risk.table.y.text = FALSE)
attach(stroke)
tapply(stroke$los,stroke$region,summary)
tapply(y2016deaths$condicion,y2016deaths$agecat,summary)

#Create trend
#modelo lineal?
describetrend <- stroke %>% ggplot(aes(year, los, group = sexo)) +
  geom_line(alpha = 1/3)
describetrend


men <- filter(stroke, sexo == "Men")
men %>% 
  ggplot(aes(factor(year), los)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

#LINEAL REGRESS ~ TRENDS
library(moderndive)
library(infer)
library(tidyverse)
library(broom)
library(modelr)
library(sandwich)
library(dagitty)

# Observed effect δ∗ b1
#slope_obs <- lm %>% 
#  specify(n ~ sexo) %>% 
#  calculate(stat = "slope",order = c("Women", "Men")) #women control
#slope_obs

#help("specify")

#Distribution of δ under  H0
#null_slope_distn <- lm %>% 
#  specify(n~ sexo) %>%
#  hypothesize(null = "independence") %>% 
#  generate(reps = 10000) %>% 
#  calculate(stat = "slope",order = c("Women", "Men"))

#null_slope_distn %>% 
# visualize(obs_stat = slope_obs, direction = "greater")

#p-value
#null_slope_distn %>% 
#  get_pvalue(obs_stat = slope_obs, direction = "greater") #0.449

lm2 <- stroke %>% group_by(year) %>% count()
names(lm2)[names(lm2)=="n"]<-"N"
lm <- stroke %>% 
  group_by(year) %>%
  select(year,sexo) %>% count(sexo)
lm <- left_join(lm, lm2, by = "year")
lm <- lm %>% spread(sexo,n)
View(lm)

lm$Men <- as.numeric(lm$Men)
lm$Women <- as.numeric(lm$Women)
lm$N <- as.numeric(lm$N)
lm$year <- as.numeric(lm$year)
###negative binomial
#var>mean
var(lm$Men) > mean(lm$Men) #overdispertion
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))
#Model 1: Quasi-poisson 

year_sex_lm <- lm(Men~year+Women+N,data=lm)
year_sex_lm <- glm(n ~ men+women,family="poisson",data=lm)
summary(year_sex_lm)
get_regression_table(year_sex_lm)

tidy(year_sex_lm)
summary(year_sex_lm)
plot(year_sex_lm)
sandwich(year_sex_lm)
test.
summary.glm(lm,dispersion=n)
var(lm$n)
mean(lm$n)

####plot#####
ggplot(lmedian.income ,burglaries) 
curve(exp(coef(standard.fit )[1] +
            coef(standard.fit )[2]*x),add =TRUE , col="blue")

#############


coeff <- glm(n ~ year + sexo, family="poisson",data = lm) %>% coef() %>% as.numeric()
coeff
slopes <- lm %>%
  group_by(sexo) %>%
  summarise(min = min(year), max = max(year)) %>%
  mutate(intercept = coeff[1]) %>%
  mutate(intercept = ifelse(sexo == "Men", intercept + coeff[3], intercept)) %>%
  gather(point, year, -c(sexo, intercept)) %>%
  mutate(y_hat = intercept + year * coeff[2])
slopes

ggplot(lm, aes(x = year, y = n, col = sexo)) +
  geom_jitter() +
  labs(x = "Year", y = "Stroke cases", color = "Sex") +
  geom_line(data = slopes, aes(y = y_hat), size = 1)#+


#Model 2: Includes an interaction term
year_sex_model <- lm(n ~ year*sexo,data=lm)

ggplot(lm, aes(x = year, y = n, col = sexo)) +
  geom_jitter() +
  labs(x = "Year", y = "Stroke cases", color = "Sex") +
  geom_smooth(method = "lm", se = FALSE)

get_regression_table(year_sex_lm)
get_regression_table(year_sex_model)

#Table no interaction
n_model_2 <- lm(n ~ year + sexo, data = lm)
get_regression_table(n_model_2) %>% 
  knitr::kable(
    digits = 3,
    caption = "Model 1: Regression table with no interaction effect included", 
    booktabs = TRUE
  )
coef(year_sex_lm)
summary(year_sex_lm)

#Table interaction
n_model_1 <- lm(n ~ year * sexo, data = lm)
get_regression_table(n_model_1) %>% 
  knitr::kable(
    digits = 3,
    caption = "Model 2: Regression table with interaction effect included", 
    booktabs = TRUE)

mean(lm$n)
var(lm$n)

#DAG

#trends comparation
library(trend)







