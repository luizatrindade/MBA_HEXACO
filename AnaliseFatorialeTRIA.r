library(lavaan)
library(lavaanPlot)
library(mirt)
library(readr)

data <- read_delim("localizacaodoarquivo",  delim = "\t", escape_double = FALSE,  trim_ws = TRUE)
Theta<-matrix(seq(-5,5, by=.1))
dataA <-data[,121:160]

myModelA <- '
   # latent variables
  A =~ AForg1+  AForg2 + AForg3+  AForg4 + AForg5 + AForg6 + AForg7 + AForg8+  AForg9 + AForg10+ AGent1 + AGent2 + AGent3 + AGent4 + AGent5 + AGent6 + AGent7 + AGent8 + AGent9 + AGent10+AFlex1 + AFlex2 + AFlex3 + AFlex4  +AFlex5 + AFlex6 + AFlex7 + AFlex8 + AFlex9+  AFlex10+APati1 + APati2 + APati3 + APati4 + APati5 + APati6 + APati7 + APati8 + APati9 + APati10
'

fitA <- cfa(model = myModelA,
            data  = dataA, estimator="WLS")
summary (fitA, fit.measures = TRUE)

irtA <- mirt(dataA,1, itemtype = "gpcm", SE = TRUE)
M2(irtA)
itemfit(irtA)
coef(irtA,simplify = TRUE, IRTpars = TRUE)$items

extr_A<-extract.item(irtA,2) 
infoA<-iteminfo(extr_A,Theta, total.info=TRUE) 
plot(Theta,infoA,type='l',main='Test information Curve')

#retirar tudo com a (discriminacao) abaixo de 0.34 em modulo 

dataA2 <-data[,c(122:123,125:127,129:131,135:138,140,143,147:160)]

myModelA2 <- '
   # latent variables
  A2 =~ AForg2 + AForg3 + AForg5 + AForg6 + AForg7 + AForg9 + AForg10+ AGent1 + AGent5 + AGent6 + AGent7 + AGent8 + AGent10+ AFlex3  + AFlex7 + AFlex8 + AFlex9+  AFlex10+APati1 + APati2 + APati3 + APati4 + APati5 + APati6 + APati7 + APati8 + APati9 + APati10
'

fitA2 <- cfa(model = myModelA2,
            data  = dataA2, estimator="WLS")
summary (fitA2, fit.measures = TRUE)

irtA2 <- mirt(dataA2,1, itemtype = "gpcm", SE = TRUE)
M2(irtA2)
itemfit(irtA2)
coef(irtA2,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.4 em modulo

dataA3 <-data[,c(123,125:126,135,138,147,151:160)]

myModelA3 <- '
   # latent variables
  A3 =~   AForg3 + AForg5 + AForg6   + AGent5 + AGent8  + AFlex7 +APati1 + APati2 + APati3 + APati4 + APati5 + APati6 + APati7 + APati8 + APati9 + APati10
'

fitA3 <- cfa(model = myModelA3,
             data  = dataA3, estimator="WLS")
summary (fitA3, fit.measures = TRUE)

irtA3 <- mirt(dataA3,1, itemtype = "gpcm", SE = TRUE)
M2(irtA3)
itemfit(irtA3)
coef(irtA3,simplify = TRUE, IRTpars = TRUE)$items

extr_A3<-extract.item(irtA3,2) 
infoA3<-iteminfo(extr_A3,Theta, total.info=TRUE) 
plot(Theta,infoA3,type='l',main='Test information Curve')
