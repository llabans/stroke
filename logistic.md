Comparations
================
LMLS
2020-12-24

## data

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
setwd("~/Documentos/R/Stroke/death/")
y2016deaths <- read.csv("yearsdeaths.cvs")
glimpse(y2016deaths)
```

    ## Rows: 6,566
    ## Columns: 9
    ## $ sexo      <chr> "Women", "Women", "Men", "Men", "Women", "Women", "Men", "M…
    ## $ edad      <int> 70, 87, 80, 66, 57, 56, 64, 64, 37, 90, 73, 98, 85, 92, 92,…
    ## $ cid10     <chr> "I64", "I64", "I64", "I64", "I64", "I64", "I60", "I60", "I6…
    ## $ agecat    <chr> "55-74", "75+", "75+", "55-74", "55-74", "55-74", "55-74", …
    ## $ level     <chr> "I, II", "I, II", "III", "III", "III", "III", "III", "III",…
    ## $ regions   <chr> "Sierra", "Sierra", "Sierra", "Sierra", "Sierra", "Sierra",…
    ## $ condicion <chr> "Discharged", "Discharged", "Discharged", "Discharged", "Di…
    ## $ los       <int> 2, 1, 28, 14, 2, 7, 10, 3, 12, 11, 12, 5, 4, 5, 12, 8, 19, …
    ## $ anio      <int> 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017,…

## exposure

``` r
y2016deaths$exposure <- ifelse(y2016deaths$cid10=="I63",0,
                               ifelse(y2016deaths$cid10=="I60",1,
                                      ifelse(y2016deaths$cid10=="I61",2,
                                             ifelse(y2016deaths$cid10=="I64",3,NA))))
y2016deaths$exposure <- factor(y2016deaths$exposure, levels = c(0,1,2,3), labels = c("I63", "I60", "I61","I64"))
sum(is.na(y2016deaths$exposure)) #0
```

    ## [1] 0

## outcome

``` r
y2016deaths$outcome <- ifelse(y2016deaths$condicion=="Discharged", 0, 
                              ifelse(y2016deaths$condicion=="Death", 1, NA)) %>% 
  factor(y2016deaths$outcome, levels = c(0,1), labels = c("Discharged","Death"))
```

## logistic model

``` r
glm.fit <- glm(outcome ~ exposure + sexo + edad,
               data = y2016deaths,
               family = binomial)
```

## strata

``` r
glm.fit.1 <- glm(outcome ~ exposure + sexo + edad +level,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$regions=="Lima/Callao")

glm.fit.2 <- glm(outcome ~ exposure + sexo + edad +level,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$regions=="Resto Costa")

glm.fit.3 <- glm(outcome ~ exposure + sexo + edad +level,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$regions=="Selva")

glm.fit.4 <- glm(outcome ~ exposure + sexo + edad +level,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$regions=="Sierra")

glm.fit.5 <- glm(outcome ~ exposure + sexo + edad +regions,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$level=="I, II")

glm.fit.6 <- glm(outcome ~ exposure + sexo + edad +regions,
               data = y2016deaths,
               family = binomial, subset = y2016deaths$level=="III")
```

## Coeff~OR

``` r
summary(glm.fit)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad, family = binomial, 
    ##     data = y2016deaths)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8725  -0.5266  -0.4483  -0.3729   2.5386  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.756189   0.232665 -16.144  < 2e-16 ***
    ## exposureI60  1.399018   0.136506  10.249  < 2e-16 ***
    ## exposureI61  0.904375   0.122736   7.368 1.73e-13 ***
    ## exposureI64  0.414292   0.112511   3.682 0.000231 ***
    ## sexoWomen    0.196411   0.079564   2.469 0.013565 *  
    ## edad         0.015121   0.002984   5.067 4.05e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4693.9  on 6565  degrees of freedom
    ## Residual deviance: 4545.1  on 6560  degrees of freedom
    ## AIC: 4557.1
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit))
```

    ## (Intercept) exposureI60 exposureI61 exposureI64   sexoWomen        edad 
    ##  0.02337265  4.05122049  2.47038813  1.51329933  1.21702746  1.01523582

``` r
exp(confint(glm.fit))
```

    ## Waiting for profiling to be done...

    ##                  2.5 %     97.5 %
    ## (Intercept) 0.01474398 0.03671004
    ## exposureI60 3.10233290 5.29990044
    ## exposureI61 1.94521515 3.14845240
    ## exposureI64 1.21700336 1.89232339
    ## sexoWomen   1.04155262 1.42291031
    ## edad        1.00934399 1.02122358

\#\#model1

``` r
summary(glm.fit.1)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + level, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$regions == "Lima/Callao")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0885  -0.4811  -0.3881  -0.3197   2.5495  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.06811    0.37939  -8.087 6.12e-16 ***
    ## exposureI60  1.49631    0.18554   8.065 7.34e-16 ***
    ## exposureI61  0.68811    0.17725   3.882 0.000104 ***
    ## exposureI64  0.13869    0.17361   0.799 0.424378    
    ## sexoWomen    0.37372    0.12612   2.963 0.003045 ** 
    ## edad         0.01247    0.00468   2.665 0.007706 ** 
    ## levelIII    -0.77833    0.15753  -4.941 7.78e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2002.5  on 3079  degrees of freedom
    ## Residual deviance: 1895.6  on 3073  degrees of freedom
    ## AIC: 1909.6
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit.1))
```

    ## (Intercept) exposureI60 exposureI61 exposureI64   sexoWomen        edad 
    ##  0.04650882  4.46516642  1.98994441  1.14876723  1.45313045  1.01254956 
    ##    levelIII 
    ##  0.45917471

``` r
exp(confint(glm.fit.1))
```

    ## Waiting for profiling to be done...

    ##                  2.5 %     97.5 %
    ## (Intercept) 0.02191171 0.09701797
    ## exposureI60 3.10784282 6.43830128
    ## exposureI61 1.40631512 2.82088118
    ## exposureI64 0.81799040 1.61742529
    ## sexoWomen   1.13611486 1.86346300
    ## edad        1.00335038 1.02193788
    ## levelIII    0.33855148 0.62832664

\#\#model2

``` r
summary(glm.fit.2)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + level, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$regions == "Resto Costa")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8209  -0.5407  -0.4713  -0.3166   2.6605  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.061311   0.491422  -8.264  < 2e-16 ***
    ## exposureI60  1.488841   0.352374   4.225 2.39e-05 ***
    ## exposureI61  1.507906   0.270070   5.583 2.36e-08 ***
    ## exposureI64  0.921608   0.281522   3.274  0.00106 ** 
    ## sexoWomen    0.101671   0.153344   0.663  0.50731    
    ## edad         0.014231   0.005602   2.540  0.01108 *  
    ## levelIII    -0.074570   0.181259  -0.411  0.68078    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1267.0  on 1794  degrees of freedom
    ## Residual deviance: 1221.6  on 1788  degrees of freedom
    ## AIC: 1235.6
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit.2))
```

    ## (Intercept) exposureI60 exposureI61 exposureI64   sexoWomen        edad 
    ##  0.01722641  4.43195586  4.51726010  2.51332884  1.10701948  1.01433277 
    ##    levelIII 
    ##  0.92814221

``` r
exp(confint(glm.fit.2))
```

    ## Waiting for profiling to be done...

    ##                   2.5 %     97.5 %
    ## (Intercept) 0.006432556 0.04423645
    ## exposureI60 2.213772212 8.88712262
    ## exposureI61 2.714712281 7.86952121
    ## exposureI64 1.476531720 4.47325230
    ## sexoWomen   0.820026137 1.49694004
    ## edad        1.003360301 1.02565758
    ## levelIII    0.649971975 1.32346968

\#\#model3

``` r
summary(glm.fit.3)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + level, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$regions == "Selva")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2181  -0.4502  -0.4044  -0.3908   2.3306  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.500264   0.824933  -3.031 0.002439 ** 
    ## exposureI60  0.529110   0.488843   1.082 0.279088    
    ## exposureI61  0.657892   0.489440   1.344 0.178893    
    ## exposureI64 -0.273555   0.397621  -0.688 0.491465    
    ## sexoWomen   -0.037676   0.280147  -0.134 0.893017    
    ## edad         0.004208   0.010860   0.387 0.698392    
    ## levelIII     1.672433   0.435920   3.837 0.000125 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 411.07  on 579  degrees of freedom
    ## Residual deviance: 380.18  on 573  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 394.18
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit.3))
```

    ## (Intercept) exposureI60 exposureI61 exposureI64   sexoWomen        edad 
    ##   0.0820633   1.6974201   1.9307184   0.7606702   0.9630247   1.0042171 
    ##    levelIII 
    ##   5.3251087

``` r
exp(confint(glm.fit.3))
```

    ## Waiting for profiling to be done...

    ##                  2.5 %     97.5 %
    ## (Intercept) 0.01533718  0.3928546
    ## exposureI60 0.65069355  4.4994162
    ## exposureI61 0.73568364  5.1140594
    ## exposureI64 0.36001247  1.7363666
    ## sexoWomen   0.55274629  1.6648286
    ## edad        0.98332553  1.0262139
    ## levelIII    2.24913599 12.5643336

\#\#model4

``` r
summary(glm.fit.4)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + level, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$regions == "Sierra")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0023  -0.6138  -0.5520  -0.4551   2.3853  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.216433   0.539194  -5.965 2.44e-09 ***
    ## exposureI60  1.014355   0.311933   3.252  0.00115 ** 
    ## exposureI61  0.641726   0.291842   2.199  0.02789 *  
    ## exposureI64 -0.004345   0.245606  -0.018  0.98589    
    ## sexoWomen    0.093928   0.166205   0.565  0.57199    
    ## edad         0.018889   0.006553   2.882  0.00395 ** 
    ## levelIII    -0.338013   0.223875  -1.510  0.13109    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 983.99  on 1109  degrees of freedom
    ## Residual deviance: 957.17  on 1103  degrees of freedom
    ## AIC: 971.17
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
exp(coef(glm.fit.4))
```

    ## (Intercept) exposureI60 exposureI61 exposureI64   sexoWomen        edad 
    ##  0.04009782  2.75758294  1.89975691  0.99566476  1.09848025  1.01906845 
    ##    levelIII 
    ##  0.71318609

``` r
exp(confint(glm.fit.4))
```

    ## Waiting for profiling to be done...

    ##                  2.5 %    97.5 %
    ## (Intercept) 0.01358141 0.1126879
    ## exposureI60 1.49954225 5.1162951
    ## exposureI61 1.07673245 3.3946731
    ## exposureI64 0.62356597 1.6391252
    ## sexoWomen   0.79355754 1.5236787
    ## edad        1.00626728 1.0324802
    ## levelIII    0.45286056 1.0920554

\#\#model5

``` r
summary(glm.fit.5)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + regions, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$level == "I, II")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9969  -0.5697  -0.4851  -0.4063   2.5269  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.239249   0.361577  -8.959  < 2e-16 ***
    ## exposureI60         1.097073   0.236406   4.641 3.47e-06 ***
    ## exposureI61         0.753231   0.202442   3.721 0.000199 ***
    ## exposureI64         0.101723   0.171856   0.592 0.553912    
    ## sexoWomen           0.144382   0.112032   1.289 0.197482    
    ## edad                0.017090   0.004273   4.000 6.34e-05 ***
    ## regionsResto Costa -0.327105   0.167736  -1.950 0.051162 .  
    ## regionsSelva       -0.560717   0.199897  -2.805 0.005031 ** 
    ## regionsSierra       0.036304   0.162405   0.224 0.823117    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2278.2  on 2884  degrees of freedom
    ## Residual deviance: 2203.9  on 2876  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 2221.9
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit.5))
```

    ##        (Intercept)        exposureI60        exposureI61        exposureI64 
    ##         0.03919332         2.99538671         2.12385180         1.10707647 
    ##          sexoWomen               edad regionsResto Costa       regionsSelva 
    ##         1.15532511         1.01723636         0.72100800         0.57079970 
    ##      regionsSierra 
    ##         1.03697075

``` r
exp(confint(glm.fit.5))
```

    ## Waiting for profiling to be done...

    ##                         2.5 %     97.5 %
    ## (Intercept)        0.01908401 0.07879767
    ## exposureI60        1.88313535 4.76663145
    ## exposureI61        1.43327861 3.17430391
    ## exposureI64        0.79703533 1.56541478
    ## sexoWomen          0.92785134 1.43986278
    ## edad               1.00882807 1.02587453
    ## regionsResto Costa 0.52040948 1.00530464
    ## regionsSelva       0.38446791 0.84305342
    ## regionsSierra      0.75691814 1.43185806

\#\#model6

``` r
summary(glm.fit.6)
```

    ## 
    ## Call:
    ## glm(formula = outcome ~ exposure + sexo + edad + regions, family = binomial, 
    ##     data = y2016deaths, subset = y2016deaths$level == "III")
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3421  -0.4955  -0.3878  -0.3239   2.5482  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -3.722895   0.323951 -11.492  < 2e-16 ***
    ## exposureI60         1.449754   0.173116   8.374  < 2e-16 ***
    ## exposureI61         0.938787   0.157055   5.977 2.27e-09 ***
    ## exposureI64         0.363652   0.170518   2.133   0.0330 *  
    ## sexoWomen           0.245344   0.114570   2.141   0.0322 *  
    ## edad                0.010115   0.004271   2.368   0.0179 *  
    ## regionsResto Costa  0.167030   0.137909   1.211   0.2258    
    ## regionsSelva        1.709119   0.384479   4.445 8.78e-06 ***
    ## regionsSierra       0.384566   0.212570   1.809   0.0704 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2397.0  on 3679  degrees of freedom
    ## Residual deviance: 2271.8  on 3671  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 2289.8
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(glm.fit.6))
```

    ##        (Intercept)        exposureI60        exposureI61        exposureI64 
    ##          0.0241639          4.2620666          2.5568782          1.4385733 
    ##          sexoWomen               edad regionsResto Costa       regionsSelva 
    ##          1.2780608          1.0101668          1.1817896          5.5240950 
    ##      regionsSierra 
    ##          1.4689769

``` r
exp(confint(glm.fit.6))
```

    ## Waiting for profiling to be done...

    ##                         2.5 %      97.5 %
    ## (Intercept)        0.01269958  0.04523753
    ## exposureI60        3.04003454  5.99765873
    ## exposureI61        1.88402601  3.49023777
    ## exposureI64        1.02963935  2.01136320
    ## sexoWomen          1.02161260  1.60129541
    ## edad               1.00178443  1.01870625
    ## regionsResto Costa 0.89800677  1.54283467
    ## regionsSelva       2.56592316 11.72838160
    ## regionsSierra      0.95249752  2.19742849
