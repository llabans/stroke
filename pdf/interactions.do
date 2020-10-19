*local dir `c(pwd)'
//ON
*cd "`c(pwd)'"
*cd "D:\42-tiempoevento"

**** Base de datos: Cáncer y muerte ****
use "Z:\home\init5\Documentos\stata\bioestadistica\deaths.dta", clear

codebook

sum los, d

*label define condicion 0 "Discharged" 1 "Death", replace
******************** KAPLAN MEIER ******************** 
*modificar base a SURVIVAL TIME
*SETEAR A SOBREVIDA
stset los, failure(condicion==2)

*listar todos los eventos en tiempo y funcion SOBREVIDA por cada punto en el tiempo
*en el que ocurre evento de interes
sts list, survival
sts list, failure

***************************************************
**************** WEIBULL MODEL ********************
**********A: xLEVEL**********
*AM: 
*Y=B0 + B1.Level + B2.Exposure + B3.confusores
streg i.level i.exposure edad sexo regions, strata(level) d(weibull)

*AM + Interaction: 
*Y = B0 + B1.Level + B2.Exposure + B3.Level.Exposure + B4.confusores
streg i.level i.exposure i.level#i.exposure edad sexo regions, strata(level) d(weibull)

**********B: xREGIONS********
*AM: Y = B0 + B1.regions + B2.Exposure + B3.confusores
streg i.regions i.exposure edad sexo level, strata(regions) d(weibull)

*AM + Interaction: Y = B0 + B1.regions + B2.Exposure + B3.regions.Exposure + B4.confusores
streg i.regions i.exposure i.regions#i.exposure edad sexo level, strata(regions) d(weibull)

********COMPARISONS************
*XLEVEL
streg i.level i.exposure i.level#i.exposure edad sexo regions, strata(level) d(weibull)
estimates store m1
streg i.level i.exposure edad sexo regions, strata(level) d(weibull)
lrtest . m1

streg i.level i.exposure i.level#i.exposure edad sexo i.regions, strata(regions) d(weibull)
estimates store m2
streg i.level i.exposure edad sexo i.regions, strata(regions) d(weibull)
lrtest . m2

*xREGIONS
streg i.regions i.exposure i.regions#i.exposure edad sexo level, strata(regions) d(weibull)
estimates store m3
streg i.regions i.exposure edad sexo level, strata(regions) d(weibull)
lrtest . m3, force

streg i.regions i.exposure i.regions#i.exposure edad sexo level, strata(regions) d(weibull)
estimates store m4
streg i.regions i.exposure edad sexo level, strata(regions) d(weibull)
lrtest . m4, force
