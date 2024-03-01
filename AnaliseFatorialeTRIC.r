library(lavaan)
library(lavaanPlot)
library(mirt)
library(readr)

data <- read_delim("localizacaodoarquivo",  delim = "\t", escape_double = FALSE,  trim_ws = TRUE)

dataC <-data[,161:200]
Theta<-matrix(seq(-5,5, by=.1))

myModelC <- '
   # latent variables
C =~ COrga1 + COrga2 +COrga3 + COrga4 + COrga5 + COrga6 +COrga7 + COrga8 + COrga9+  COrga10+CDili1 + CDili2  +CDili3  +CDili4 + CDili5  +CDili6  +CDili7  +CDili8+  CDili9 + CDili10+ CPerf1  +CPerf2  +CPerf3  +CPerf4 + CPerf5  +CPerf6  +CPerf7  +CPerf8  +CPerf9  +CPerf10+CPrud1  +CPrud2  +CPrud3  +CPrud4+  CPrud5  +CPrud6  +CPrud7  +CPrud8  +CPrud9  +CPrud10
'

fitC <- cfa(model = myModelC,
            data  = dataC, estimator="WLS")
summary (fitC, fit.measures = TRUE)

irtC <- mirt(dataC,1, itemtype = "gpcm", SE = TRUE)
M2(irtC)
itemfit(irtC)
coef(irtC,simplify = TRUE, IRTpars = TRUE)$items

extr_C<-extract.item(irtC,2) 
infoC<-iteminfo(extr_C,Theta, total.info=TRUE) 
plot(Theta,infoC,type='l',main='Test information Curve')



dataC4 <-data[,c(162,168,171:178,180,182,192,193,198)]
myModelC4 <- '
 # latent variables
C4 =~  COrga2 +  COrga8 + +CDili1 + CDili2  +CDili3  +CDili4 + CDili5  +CDili6  +CDili7  +CDili8 + CDili10 +CPerf2  +CPrud2  +CPrud3 +CPrud8  
'

 fitC4 <- cfa(model = myModelC4,
              data  = dataC4, estimator="WLS")
 summary (fitC4, fit.measures = TRUE)
irtC4 <- mirt(dataC4,1, itemtype = "gpcm", SE = TRUE)
M2(irtC4)
itemfit(irtC4)

coef(irtC4,simplify = TRUE, IRTpars = TRUE)$items

#curva de informacao final
extr_C4<-extract.item(irtC4,2) 
infoC4<-iteminfo(extr_C4,Theta, total.info=TRUE) 
plot(Theta,infoC4,type='l',main='Test information Curve')

#area sob a curva
DescTools::AUC(x=Theta, y=infoC4, method= c("trapezoid"))
