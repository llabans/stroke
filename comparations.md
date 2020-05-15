Comparations
================
LMLS
2020-05-14

## Base

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library()
```

    ## Warning in library(): libraries '/usr/local/lib/R/site-library', '/usr/lib/R/
    ## site-library' contain no packages

``` r
setwd("~/Documentos/R/Stroke/")
stroke <- read.csv("stroke.csv")
glimpse(stroke)
```

    ## Rows: 39,810
    ## Columns: 9
    ## $ year      <int> 2012, 2012, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017,…
    ## $ sexo      <chr> "Men", "Men", "Women", "Men", "Men", "Women", "Women", "Men…
    ## $ edad      <int> 75, 73, 70, 80, 66, 57, 56, 64, 64, 37, 90, 73, 98, 85, 92,…
    ## $ cid10     <chr> "I64", "I60", "I64", "I64", "I64", "I64", "I64", "I60", "I6…
    ## $ agecat    <chr> "75+", "55-74", "55-74", "75+", "55-74", "55-74", "55-74", …
    ## $ level     <chr> "I, II", "I, II", "I, II", "III", "III", "III", "III", "III…
    ## $ regions   <chr> "Sierra", "Sierra", "Sierra", "Sierra", "Sierra", "Sierra",…
    ## $ los       <int> 5, 5, 2, 28, 14, 2, 7, 10, 3, 12, 11, 12, 5, 4, 5, 8, 19, 5…
    ## $ condicion <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…

``` r
strokerelos <- subset(stroke, select = c(year,regions,los)) 
levels(as.factor(strokerelos$regions))
```

    ## [1] "Lima/Callao" "Resto Costa" "Selva"       "Sierra"

``` r
strokerelos$regions <- ifelse(strokerelos$regions=="Lima/Callao" | strokerelos$regions=="Resto Costa", 0, 
                      ifelse(strokerelos$regions=="Sierra" | strokerelos$regions=="Selva", 1, NA)) %>% 
                      factor(strokerelos$regions, levels = c(0,1), labels = c("Lima-Restocosta", "Sierra y Selva"))

strokerelos[1:20,]
```

    ##    year        regions los
    ## 1  2012 Sierra y Selva   5
    ## 2  2012 Sierra y Selva   5
    ## 3  2017 Sierra y Selva   2
    ## 4  2017 Sierra y Selva  28
    ## 5  2017 Sierra y Selva  14
    ## 6  2017 Sierra y Selva   2
    ## 7  2017 Sierra y Selva   7
    ## 8  2017 Sierra y Selva  10
    ## 9  2017 Sierra y Selva   3
    ## 10 2017 Sierra y Selva  12
    ## 11 2017 Sierra y Selva  11
    ## 12 2017 Sierra y Selva  12
    ## 13 2017 Sierra y Selva   5
    ## 14 2017 Sierra y Selva   4
    ## 15 2017 Sierra y Selva   5
    ## 16 2017 Sierra y Selva   8
    ## 17 2017 Sierra y Selva  19
    ## 18 2017 Sierra y Selva   5
    ## 19 2017 Sierra y Selva   6
    ## 20 2017 Sierra y Selva   6

## Kruskal-Wallis test & LOS by regions

``` r
stroke$cid10 <- as.factor(stroke$cid10)
levels(stroke$cid10)
```

    ## [1] "I60" "I61" "I63" "I64"

``` r
kruskal.test(los ~ regions, data = stroke)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  los by regions
    ## Kruskal-Wallis chi-squared = 3273.8, df = 3, p-value < 2.2e-16

``` r
pairwise.wilcox.test(stroke$los, stroke$regions, p.adjust.method = "BH")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  stroke$los and stroke$regions 
    ## 
    ##             Lima/Callao Resto Costa Selva 
    ## Resto Costa <2e-16      -           -     
    ## Selva       <2e-16      <2e-16      -     
    ## Sierra      <2e-16      <2e-16      <2e-16
    ## 
    ## P value adjustment method: BH

## Kruskal-Wallis test & LOS by facilities

``` r
strokefacilities <- subset(stroke, select = c(level,los))
strokefacilities$level <- as.factor(strokefacilities$level)
levels(strokefacilities$level)
```

    ## [1] "I, II" "III"

``` r
strokefacilities[1:20,]
```

    ##    level los
    ## 1  I, II   5
    ## 2  I, II   5
    ## 3  I, II   2
    ## 4    III  28
    ## 5    III  14
    ## 6    III   2
    ## 7    III   7
    ## 8    III  10
    ## 9    III   3
    ## 10   III  12
    ## 11   III  11
    ## 12   III  12
    ## 13   III   5
    ## 14   III   4
    ## 15   III   5
    ## 16   III   8
    ## 17   III  19
    ## 18   III   5
    ## 19   III   6
    ## 20   III   6

``` r
kruskal.test(los ~ level, data = strokefacilities)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  los by level
    ## Kruskal-Wallis chi-squared = 4636.8, df = 1, p-value < 2.2e-16

## Years ~ figures

``` r
stroke$cid10 <- as.factor(stroke$cid10)
levels(stroke$cid10)
```

    ## [1] "I60" "I61" "I63" "I64"

``` r
kruskal.test(los ~ cid10, data = stroke)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  los by cid10
    ## Kruskal-Wallis chi-squared = 2881, df = 3, p-value < 2.2e-16

``` r
pairwise.wilcox.test(stroke$los, stroke$year, p.adjust.method = "BH")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  stroke$los and stroke$year 
    ## 
    ##      2002    2003    2004    2005    2006    2007    2008    2009    2010   
    ## 2003 0.09685 -       -       -       -       -       -       -       -      
    ## 2004 0.89123 0.07174 -       -       -       -       -       -       -      
    ## 2005 0.54892 0.01606 0.65627 -       -       -       -       -       -      
    ## 2006 0.40968 0.34814 0.32204 0.09508 -       -       -       -       -      
    ## 2007 0.28606 0.49069 0.20669 0.05174 0.79998 -       -       -       -      
    ## 2008 0.08957 0.81729 0.05857 0.00739 0.39549 0.55035 -       -       -      
    ## 2009 0.05580 0.94533 0.03880 0.00385 0.28606 0.40968 0.81006 -       -      
    ## 2010 2.4e-07 0.00238 4.8e-08 2.9e-11 1.2e-06 5.2e-06 4.3e-05 0.00010 -      
    ## 2011 0.07781 0.87187 0.04900 0.00534 0.33979 0.51180 0.92623 0.89112 4.3e-05
    ## 2012 0.00050 0.16440 0.00016 2.3e-06 0.00385 0.00938 0.04661 0.07782 0.04661
    ## 2013 0.00018 0.14188 7.4e-05 5.7e-07 0.00215 0.00533 0.02914 0.05439 0.04093
    ## 2014 0.00595 0.52317 0.00333 9.9e-05 0.04661 0.08757 0.28121 0.38459 0.00238
    ## 2015 2.4e-05 0.03905 6.5e-06 3.8e-08 0.00016 0.00057 0.00356 0.00739 0.27769
    ## 2016 0.14844 0.66954 0.09492 0.01726 0.54339 0.76207 0.80957 0.63495 1.1e-05
    ## 2017 0.00534 0.55035 0.00302 8.9e-05 0.04661 0.09030 0.28918 0.40968 0.00138
    ##      2011    2012    2013    2014    2015    2016   
    ## 2003 -       -       -       -       -       -      
    ## 2004 -       -       -       -       -       -      
    ## 2005 -       -       -       -       -       -      
    ## 2006 -       -       -       -       -       -      
    ## 2007 -       -       -       -       -       -      
    ## 2008 -       -       -       -       -       -      
    ## 2009 -       -       -       -       -       -      
    ## 2010 -       -       -       -       -       -      
    ## 2011 -       -       -       -       -       -      
    ## 2012 0.05122 -       -       -       -       -      
    ## 2013 0.03701 0.93017 -       -       -       -      
    ## 2014 0.29833 0.36365 0.32333 -       -       -      
    ## 2015 0.00385 0.40793 0.40968 0.06863 -       -      
    ## 2016 0.75276 0.02135 0.01163 0.16440 0.00141 -      
    ## 2017 0.32577 0.32577 0.27769 0.92623 0.05247 0.17067
    ## 
    ## P value adjustment method: BH
