
library(tidyverse)
strokemale <- subset(stroke, stroke$sexo=="Men", select = c(year))
strokemale <- strokemale%>%group_by(year)%>%dplyr::summarise(frecuencia= n())

strokefemale <- subset(stroke, stroke$sexo=="Women", select = c(year))
strokefemale <- strokefemale%>%group_by(year)%>%dplyr::summarise(frecuencia= n())
strokemale$Sex <- 0
strokefemale$Sex <- 1
strokesex <- rbind(strokemale, strokefemale)
rm(strokemale, strokefemale)

stroke14 <- strokesex %>% filter(year %in% c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))
stroke15 <- strokesex %>% filter(year %in% c("2015","2016","2017"))


library(trend)
partial.cor.trend.test(stroke14$frecuencia,stroke14$Sex, method = "spearman")
partial.cor.trend.test(stroke15$frecuencia,stroke15$Sex, method = "spearman")
