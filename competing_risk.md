CI overall
----------

    setwd("~/Documentos/R/Stroke/death/")
    y2016deaths <- y2016deaths <- read.csv("yearsdeaths.cvs")
    library(survival)
    library(cmprsk)

    CI.overall <- cuminc(ftime=y2016deaths$los, fstatus=y2016deaths$condicion)
    CI.overall

    ## Estimates and Variances:
    ## $est
    ##                      10        20        30        40        50
    ## 1 Death      0.09092294 0.1073713 0.1116357 0.1139202 0.1146817
    ## 1 Discharged 0.60006092 0.8044472 0.8540969 0.8717636 0.8793786
    ## 
    ## $var
    ##                        10           20           30           40           50
    ## 1 Death      1.249284e-05 1.442453e-05 1.490245e-05 1.515360e-05 1.523636e-05
    ## 1 Discharged 3.603488e-05 2.344142e-05 1.858354e-05 1.670773e-05 1.588148e-05

Including Plots of CI.overall
-----------------------------

![](competing_risk_files/figure-markdown_strict/pressure-1.png)

Estimating CI of specific-cause
-------------------------------

    CI.4vs5 <- cuminc(ftime=y2016deaths$los, fstatus=y2016deaths$condicion, group=y2016deaths$cid10)
    CI.4vs5

    ## Tests:
    ##                stat pv df
    ## Death      121.1578  0  3
    ## Discharged 229.7790  0  3
    ## Estimates and Variances:
    ## $est
    ##                        10         20         30        40         50
    ## I60 Death      0.16913947 0.20326409 0.21216617 0.2151335 0.21513353
    ## I61 Death      0.12006197 0.14252517 0.14562355 0.1479473 0.14872192
    ## I63 Death      0.05155807 0.06005666 0.06345609 0.0674221 0.06855524
    ## I64 Death      0.08356841 0.09802539 0.10225670 0.1033145 0.10401975
    ## I60 Discharged 0.36498516 0.62314540 0.71364985 0.7611276 0.77596439
    ## I61 Discharged 0.50038730 0.74128582 0.80867545 0.8311387 0.84353215
    ## I63 Discharged 0.59036827 0.85325779 0.90311615 0.9218130 0.92917847
    ## I64 Discharged 0.70733427 0.84590973 0.87764457 0.8854020 0.88928068
    ## 
    ## $var
    ##                          10           20           30           40           50
    ## I60 Death      2.071046e-04 2.378177e-04 2.451983e-04 2.476831e-04 2.476831e-04
    ## I61 Death      8.141357e-05 9.381012e-05 9.544134e-05 9.666612e-05 9.710799e-05
    ## I63 Death      2.763560e-05 3.185265e-05 3.351034e-05 3.542348e-05 3.597362e-05
    ## I64 Death      2.673731e-05 3.068950e-05 3.179516e-05 3.206953e-05 3.225164e-05
    ## I60 Discharged 3.404120e-04 3.422243e-04 2.981384e-04 2.662844e-04 2.553352e-04
    ## I61 Discharged 1.915380e-04 1.459258e-04 1.179461e-04 1.072878e-04 1.011684e-04
    ## I63 Discharged 1.361412e-04 7.022607e-05 4.914019e-05 4.052034e-05 3.712235e-05
    ## I64 Discharged 7.160836e-05 4.477203e-05 3.693652e-05 3.496938e-05 3.398063e-05

Including Plots
---------------

    plot(CI.4vs5, lty=c(1,1,2,2,3,3,4,4),
         col=c("black", "blue", "orange", "red", "black", "blue", "orange", "red"),
         curvlab=c("Discharge, I60", "Discharge, I61", "Discharge, I63", "Discharge, I64",
           "Death, I60", "Death, I61", "Death, I63", "Death, I64"), xlab="Days",  cex=0.58, cex.axis=0.7, cex.lab=0.7, cex.main=1,cex.sub=1)

![](competing_risk_files/figure-markdown_strict/unnamed-chunk-1-1.png)

Test statistic for RC IV vs V
-----------------------------

    CI.4vs5$Tests

    ##                stat pv df
    ## Death      121.1578  0  3
    ## Discharged 229.7790  0  3
