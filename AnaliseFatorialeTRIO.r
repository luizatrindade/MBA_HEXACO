library(lavaan)
library(lavaanPlot)
library(mirt)
library(readr)

data <- read_delim("localizacaodoarquivo",  delim = "\t", escape_double = FALSE,  trim_ws = TRUE)

dataO <-data[,201:240]
Theta<-matrix(seq(-5,5, by=.1))

myModelO <- '
   # latent variables
O =~ OAesA1 +OAesA2 + OAesA3 + OAesA4 + OAesA5 +OAesA6 +OAesA7 +OAesA8+  OAesA9 + OAesA10+ OInqu1 + OInqu2 + OInqu3 + OInqu4+  OInqu5  +OInqu6  +OInqu7  +OInqu8  +OInqu9  + OInqu10+OCrea1  +OCrea2  +OCrea3  +OCrea4 + OCrea5 + OCrea6  +OCrea7  +OCrea8  +OCrea9  +OCrea10+OUnco1  +OUnco2  +OUnco3  +OUnco4  +OUnco5  +OUnco6 + OUnco7 + OUnco8 + OUnco9+  OUnco10
'

fitO <- cfa(model = myModelO,
            data  = dataO, estimator="WLS")
summary (fitO, fit.measures = TRUE)

irtO <- mirt(dataO,1, itemtype = "gpcm", SE = TRUE)
M2(irtO)
itemfit(irtO)
coef(irtO,simplify = TRUE, IRTpars = TRUE)$items

extr_O<-extract.item(irtO,2) 
infoO<-iteminfo(extr_O,Theta, total.info=TRUE) 
plot(Theta,infoO,type='l',main='Test information Curve')

#retirar tudo com a (discriminacao) abaixo de 0.34 em modulo 

dataO2 <-data[,c(201,203,212:214,216,218,221:228,231:233,235,240)]

myModelO2 <- '
   # latent variables
O2 =~ OAesA1 + OAesA3 + OInqu2 + OInqu3 + OInqu4+  +OInqu6 +OInqu8 +OCrea1  +OCrea2  +OCrea3  +OCrea4 + OCrea5 + OCrea6  +OCrea7  +OCrea8 +OUnco1  +OUnco2  +OUnco3  +OUnco5 +OUnco10
'

fitO2 <- cfa(model = myModelO2,
            data  = dataO2, estimator="WLS")
summary (fitO2, fit.measures = TRUE)

irtO2 <- mirt(dataO2,1, itemtype = "gpcm", SE = TRUE)
M2(irtO2)
itemfit(irtO2)
coef(irtO2,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.4 em modulo

dataO3 <-data[,c(203,214,216,218,221:228,232,233)]

myModelO3 <- '
   # latent variables
O3 =~  OAesA3 + OInqu4+  +OInqu6 +OInqu8 +OCrea1  +OCrea2  +OCrea3  +OCrea4 + OCrea5 + OCrea6  +OCrea7  +OCrea8  +OUnco2  +OUnco3  
'

fitO3 <- cfa(model = myModelO3,
             data  = dataO3, estimator="WLS")
summary (fitO3, fit.measures = TRUE)

irtO3 <- mirt(dataO3,1, itemtype = "gpcm", SE = TRUE)
M2(irtO3)
itemfit(irtO3)
coef(irtO3,simplify = TRUE, IRTpars = TRUE)$items

extr_O3<-extract.item(irtO3,2) 
infoO3<-iteminfo(extr_O3,Theta, total.info=TRUE) 
plot(Theta,infoO3,type='l',main='Test information Curve')
