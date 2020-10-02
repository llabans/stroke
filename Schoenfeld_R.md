\#\#\#<a href="https://stackoverflow.com/questions/22422687/r-survival-package-plotting-log-logsurvival-against-logtime" class="uri">https://stackoverflow.com/questions/22422687/r-survival-package-plotting-log-logsurvival-against-logtime</a>

Data
----

    y2016deaths <- read.csv("death/yearsdeaths.cvs") 

GENERATE EXPOSURE AND OBJETCT SURV
----------------------------------

    y2016deaths$exposure <- ifelse(y2016deaths$cid10=="I63",0,
                                   ifelse(y2016deaths$cid10=="I60",1,
                                          ifelse(y2016deaths$cid10=="I61",2,
                                                 ifelse(y2016deaths$cid10=="I64",3,NA))))
    y2016deaths$exposure <- factor(y2016deaths$exposure, levels = c(0,1,2,3), labels = c("I63", "I60", "I61","I64"))
    sum(is.na(y2016deaths$exposure)) #0

    ## [1] 0

    y2016deaths$survivalobj0 <- with(y2016deaths,Surv(los,condicion=="Death"))
    summary(y2016deaths$survivalobj0)

    ##       time            status      
    ##  Min.   : 1.000   Min.   :0.0000  
    ##  1st Qu.: 4.000   1st Qu.:0.0000  
    ##  Median : 7.000   Median :0.0000  
    ##  Mean   : 9.436   Mean   :0.1153  
    ##  3rd Qu.:12.000   3rd Qu.:0.0000  
    ##  Max.   :59.000   Max.   :1.0000

Cox model
---------

    cox1 <- coxph(survivalobj0 ~ exposure+strata(regions,level),data=y2016deaths)
    summary(cox1)

    ## Call:
    ## coxph(formula = survivalobj0 ~ exposure + strata(regions, level), 
    ##     data = y2016deaths)
    ## 
    ##   n= 6565, number of events= 757 
    ##    (1 observation deleted due to missingness)
    ## 
    ##               coef exp(coef) se(coef)     z Pr(>|z|)    
    ## exposureI60 0.9156    2.4984   0.1255 7.295 2.98e-13 ***
    ## exposureI61 0.6486    1.9128   0.1167 5.558 2.72e-08 ***
    ## exposureI64 0.3573    1.4294   0.1131 3.158  0.00159 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##             exp(coef) exp(-coef) lower .95 upper .95
    ## exposureI60     2.498     0.4003     1.954     3.195
    ## exposureI61     1.913     0.5228     1.522     2.404
    ## exposureI64     1.429     0.6996     1.145     1.784
    ## 
    ## Concordance= 0.603  (se = 0.014 )
    ## Likelihood ratio test= 63.15  on 3 df,   p=1e-13
    ## Wald test            = 62.59  on 3 df,   p=2e-13
    ## Score (logrank) test = 64.83  on 3 df,   p=5e-14

    cox2 <- coxph(survivalobj0 ~ exposure+sexo+ edad+ strata(regions, level),data=y2016deaths)
    summary(cox2)

    ## Call:
    ## coxph(formula = survivalobj0 ~ exposure + sexo + edad + strata(regions, 
    ##     level), data = y2016deaths)
    ## 
    ##   n= 6565, number of events= 757 
    ##    (1 observation deleted due to missingness)
    ## 
    ##                 coef exp(coef) se(coef)     z Pr(>|z|)    
    ## exposureI60 0.964795  2.624249 0.128007 7.537 4.81e-14 ***
    ## exposureI61 0.683784  1.981361 0.116839 5.852 4.85e-09 ***
    ## exposureI64 0.337851  1.401932 0.113019 2.989  0.00280 ** 
    ## sexoWomen   0.195240  1.215603 0.074470 2.622  0.00875 ** 
    ## edad        0.011660  1.011729 0.002794 4.173 3.00e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##             exp(coef) exp(-coef) lower .95 upper .95
    ## exposureI60     2.624     0.3811     2.042     3.373
    ## exposureI61     1.981     0.5047     1.576     2.491
    ## exposureI64     1.402     0.7133     1.123     1.750
    ## sexoWomen       1.216     0.8226     1.051     1.407
    ## edad            1.012     0.9884     1.006     1.017
    ## 
    ## Concordance= 0.621  (se = 0.014 )
    ## Likelihood ratio test= 90.27  on 5 df,   p=<2e-16
    ## Wald test            = 89.32  on 5 df,   p=<2e-16
    ## Score (logrank) test = 91.64  on 5 df,   p=<2e-16

Test for the proportional-hazards (PH) assumption,
--------------------------------------------------

    test.ph1 <- cox.zph(cox1)
    test.ph2 <- cox.zph(cox2)

    test.ph1

    ##          chisq df      p
    ## exposure  13.4  3 0.0038
    ## GLOBAL    13.4  3 0.0038

    test.ph2

    ##          chisq df      p
    ## exposure 14.18  3 0.0027
    ## sexo      2.12  1 0.1454
    ## edad      2.66  1 0.1031
    ## GLOBAL   17.13  5 0.0043

PLOT
----

    ggcoxzph(test.ph1)

![](Schoenfeld_R_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    ggcoxzph(test.ph2)

![](Schoenfeld_R_files/figure-markdown_strict/unnamed-chunk-5-2.png)
