#instalacao e carregamentos e pacotes
library(lavaan)
library(lavaanPlot)
library(mirt)
library(readr)

#carregamento de dados
data <- read_delim("localdoarquivo",  delim = "\t", escape_double = FALSE,  trim_ws = TRUE)

Theta<-matrix(seq(-5,5, by=.1))
dataX <-data[,81:120]

myModelX <- '
   # latent variables
       X =~ XExpr1 + XExpr2 + XExpr3 + XExpr4 + XExpr5 + XExpr6 + XExpr7+  XExpr8 + XExpr9 + XExpr10 + XSocB1 + XSocB2 + XSocB3 + XSocB4 + XSocB5 + XSocB6 + XSocB7 + XSocB8 + XSocB9 + XSocB10 +XSoci1 + XSoci2 + XSoci3  +XSoci4 + XSoci5  +XSoci6  +XSoci7+  XSoci8 + XSoci9  +XSoci10+ XLive1  +XLive2+  XLive3  +XLive4 + XLive5  +XLive6  +XLive7  +XLive8 + XLive9  +XLive10
'

fitX <- cfa(model = myModelX,
            data  = dataX, estimator="WLS")
summary (fitX, fit.measures = TRUE)

irtX <- mirt(dataX,1, itemtype = "gpcm", SE = TRUE)
M2(irtX)
itemfit(irtX)
coef(irtX,simplify = TRUE, IRTpars = TRUE)$items

extr_X<-extract.item(irtX,2) 
infoX<-iteminfo(extr_X,Theta, total.info=TRUE) 
plot(Theta,infoX,type='l',main='Test information Curve')

#retirar tudo com a (discriminacao) abaixo de 0.34 em modulo 

dataX2 <-data[,c(81,83,86:88,90,92:95,97:111,113:116,118)]

myModelX2 <- '
   # latent variables
       X2 =~ XExpr1 + XExpr3 + XExpr6 + XExpr7+  XExpr8  + XExpr10 + XSocB2 + XSocB3 + XSocB4 + XSocB5 + XSocB7 + XSocB8 + XSocB9 + XSocB10 +XSoci1 + XSoci2 + XSoci3  +XSoci4 + XSoci5  +XSoci6  +XSoci7+  XSoci8 + XSoci9  +XSoci10+ XLive1 +  XLive3  +XLive4 + XLive5  +XLive6  +XLive8
'

fitX2 <- cfa(model = myModelX2,
            data  = dataX2, estimator="WLS")
summary (fitX2, fit.measures = TRUE)

irtX2 <- mirt(dataX2,1, itemtype = "gpcm", SE = TRUE)
M2(irtX2)
itemfit(irtX2)
coef(irtX2,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.4 em modulo 

dataX3 <-data[,c(81,83,86:88,92,93,97:108,110,111,113,114,116,118)]

myModelX3 <- '
   # latent variables
       X3 =~ XExpr1 + XExpr3 + XExpr6 + XExpr7+  XExpr8 + XSocB2 + XSocB3+ XSocB7 + XSocB8 + XSocB9 + XSocB10 +XSoci1 + XSoci2 + XSoci3  +XSoci4 + XSoci5  +XSoci6  +XSoci7+  XSoci8 +XSoci10+ XLive1 +  XLive3  +XLive4 +XLive6  +XLive8
'

fitX3 <- cfa(model = myModelX3,
             data  = dataX3, estimator="WLS")
summary (fitX3, fit.measures = TRUE)

irtX3 <- mirt(dataX3,1, itemtype = "gpcm", SE = TRUE)
M2(irtX3)
itemfit(irtX3)
coef(irtX3,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.5 em modulo 

dataX4 <-data[,c(81,83,86:88,92,93,97:99,101:108,110,113,118)]

myModelX4 <- '
   # latent variables
       X4 =~ XExpr1 + XExpr3 + XExpr6 + XExpr7+  XExpr8 + XSocB2 + XSocB3+ XSocB7 + XSocB8 + XSocB9 +XSoci1 + XSoci2 + XSoci3  +XSoci4 + XSoci5  +XSoci6  +XSoci7+  XSoci8 +XSoci10 +  XLive3 +XLive8
'

fitX4 <- cfa(model = myModelX4,
             data  = dataX4, estimator="WLS")
summary (fitX4, fit.measures = TRUE)

irtX4 <- mirt(dataX4,1, itemtype = "gpcm", SE = TRUE)
M2(irtX4)
itemfit(irtX4)
coef(irtX4,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.55 em modulo 

dataX5 <-data[,c(81,83,86:88,92,93,97:99,101:104,106:108,110)]

myModelX5 <- '
   # latent variables
       X5 =~ XExpr1 + XExpr3 + XExpr6 + XExpr7+  XExpr8 + XSocB2 + XSocB3+ XSocB7 + XSocB8 + XSocB9 +XSoci1 + XSoci2 + XSoci3  +XSoci4  +XSoci6  +XSoci7+XSoci10
'

fitX5 <- cfa(model = myModelX5,
             data  = dataX5, estimator="WLS")
summary (fitX5, fit.measures = TRUE)

irtX5 <- mirt(dataX5,1, itemtype = "gpcm", SE = TRUE)
M2(irtX5)
itemfit(irtX5)
coef(irtX5,simplify = TRUE, IRTpars = TRUE)$items


#retirar tudo com a (discriminacao) abaixo de 0.64 em modulo 

dataX6 <-data[,c(81,83,86,88,92,93,97:99,101:104,106:108)]

myModelX6 <- '
   # latent variables
          X6 =~ XExpr1 + XExpr3 + XExpr6 +  XExpr8 + XSocB2 + XSocB3+ XSocB7 + XSocB8 + XSocB9 +XSoci1 + XSoci2 + XSoci3  +XSoci4  +XSoci6  +XSoci7
'

fitX6 <- cfa(model = myModelX6,
             data  = dataX6, estimator="WLS")
summary (fitX6, fit.measures = TRUE)

irtX6 <- mirt(dataX6,1, itemtype = "gpcm", SE = TRUE)
M2(irtX6)
itemfit(irtX6)
coef(irtX6,simplify = TRUE, IRTpars = TRUE)$items

extr_X6<-extract.item(irtX6,2) 
infoX6<-iteminfo(extr_X6,Theta, total.info=TRUE) 
plot(Theta,infoX6,type='l',main='Test information Curve')
