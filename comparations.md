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
kruskal.test(los ~ regions, data = strokerelos)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  los by regions
    ## Kruskal-Wallis chi-squared = 2206.1, df = 1, p-value < 2.2e-16

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
