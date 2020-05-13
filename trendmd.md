Trends
================
LMLS
2020-05-13

## DB

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
stroke <- read.csv("stroke.csv")
stroke_sexo <- subset(stroke, select = c(year,sexo)) 
stroke_s1 <- stroke_sexo %>%
  group_by(year) %>%
  summarise(total = n())
```

## Base

``` r
stroke_sexo[1:16,]
```

    ##    year  sexo
    ## 1  2012   Men
    ## 2  2012   Men
    ## 3  2017 Women
    ## 4  2017   Men
    ## 5  2017   Men
    ## 6  2017 Women
    ## 7  2017 Women
    ## 8  2017   Men
    ## 9  2017   Men
    ## 10 2017   Men
    ## 11 2017 Women
    ## 12 2017 Women
    ## 13 2017 Women
    ## 14 2017 Women
    ## 15 2017   Men
    ## 16 2017   Men

``` r
stroke_s1[1:16,]
```

    ## # A tibble: 16 x 2
    ##     year total
    ##    <int> <int>
    ##  1  2002  1424
    ##  2  2003  1503
    ##  3  2004  1625
    ##  4  2005  2192
    ##  5  2006  2285
    ##  6  2007  2269
    ##  7  2008  2425
    ##  8  2009  2518
    ##  9  2010  2988
    ## 10  2011  2834
    ## 11  2012  2836
    ## 12  2013  3154
    ## 13  2014  3250
    ## 14  2015  2953
    ## 15  2016  2599
    ## 16  2017  2955

## Datset

``` r
strokefemale <- subset(stroke, stroke$sexo=="Women", select = c(year))
strokefemale <- strokefemale%>%group_by(year)%>%dplyr::summarise(women = n())
strokemale <- subset(stroke, stroke$sexo=="Men", select = c(year))
strokemale <- strokemale%>%group_by(year)%>%dplyr::summarise(men = n())

stroketrendsex <- full_join(stroke_s1,strokefemale)
```

    ## Joining, by = "year"

``` r
stroketrendsex <- full_join(stroketrendsex, strokemale)
```

    ## Joining, by = "year"

``` r
stroketrendsex <- stroketrendsex %>% 
  mutate(percent1=women/total) %>% 
  mutate(percent2=men/total)

stroke14 <- stroketrendsex %>% filter(year %in% c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))
stroke14$percent1 <- as.ts(stroke14$percent1)
stroke14$percent2 <- as.ts(stroke14$percent2)

stroke15 <- stroketrendsex %>% filter(year %in% c("2015","2016","2017"))
stroke15$percent1 <- as.ts(stroke15$percent1)
stroke15$percent2 <- as.ts(stroke15$percent2)

stroke14
```

    ## # A tibble: 13 x 6
    ##     year total women   men percent1  percent2 
    ##    <int> <int> <int> <int> <ts>      <ts>     
    ##  1  2002  1424   740   684 0.5196629 0.4803371
    ##  2  2003  1503   746   757 0.4963407 0.5036593
    ##  3  2004  1625   826   799 0.5083077 0.4916923
    ##  4  2005  2192  1171  1021 0.5342153 0.4657847
    ##  5  2006  2285  1219  1066 0.5334792 0.4665208
    ##  6  2007  2269  1200  1069 0.5288673 0.4711327
    ##  7  2008  2425  1272  1153 0.5245361 0.4754639
    ##  8  2009  2518  1254  1264 0.4980143 0.5019857
    ##  9  2010  2988  1497  1491 0.5010040 0.4989960
    ## 10  2011  2834  1413  1421 0.4985886 0.5014114
    ## 11  2012  2836  1395  1441 0.4918900 0.5081100
    ## 12  2013  3154  1579  1575 0.5006341 0.4993659
    ## 13  2014  3250  1531  1719 0.4710769 0.5289231

``` r
stroke15
```

    ## # A tibble: 3 x 6
    ##    year total women   men percent1  percent2 
    ##   <int> <int> <int> <int> <ts>      <ts>     
    ## 1  2015  2953  1472  1481 0.4984761 0.5015239
    ## 2  2016  2599  1265  1334 0.4867257 0.5132743
    ## 3  2017  2955  1434  1521 0.4852792 0.5147208

## Trend test

``` r
library(trend)
partial.cor.trend.test(stroke14$percent1,stroke14$percent2, method = "spearman")
```

    ## 
    ##  Spearman's Partial Correlation Trend Test
    ## 
    ## data:  t AND  stroke14$percent1  .  stroke14$percent2
    ## t = 0, df = 11, p-value = 1
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ## r(tstroke14$percent1.stroke14$percent2) 
    ##                                       0

``` r
partial.cor.trend.test(stroke15$percent1,stroke15$percent2, method = "spearman")
```

    ## 
    ##  Spearman's Partial Correlation Trend Test
    ## 
    ## data:  t AND  stroke15$percent1  .  stroke15$percent2
    ## t = 0, df = 1, p-value = 1
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ## r(tstroke15$percent1.stroke15$percent2) 
    ##                                       0
