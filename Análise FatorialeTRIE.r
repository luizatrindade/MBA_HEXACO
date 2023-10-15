#Instalação e carregamento de pacotes
install.packages("lavaan")
install.packages("lavaanPlot")
install.packages("mirt")

library(readr)
library(lavaan)
library(lavaanPlot)
library(mirt)


#carregamento de dados-link no readme
data <- read_delim("localdoarquivo")

dataE <-data[,41:80]

myModelE <- '
   # latent variables
        E =~ EFear1 + EFear2 + EFear3 + EFear4 + EFear5 +  EFear6 + EFear7 + EFear8  +EFear9 + EFear10+EAnxi1+  EAnxi2 + EAnxi3 + EAnxi4 + EAnxi5 + EAnxi6 + EAnxi7 + EAnxi8 + EAnxi9 + EAnxi10 + EDepe1 + EDepe2 +  EDepe3 + EDepe4 + EDepe5+  EDepe6 + EDepe7+  EDepe8+  EDepe9  +EDepe10+ESent1 + ESent2 + ESent3 + ESent4 + ESent5 + ESent6 + ESent7 + ESent8+  ESent9+  ESent10
'

fitE <- cfa(model = myModelE,
           data  = dataE, estimator="WLS")
summary (fitE, fit.measures = TRUE)

irtE <- mirt(dataE,1, itemtype = "gpcm", SE = TRUE)
M2(irtE)
itemfit(irtE)
coef(irtE,simplify = TRUE, IRTpars = TRUE)$items
Theta<-matrix(seq(-5,5, by=.1))
extr_E1<-extract.item(irtE,2) 
info1<-iteminfo(extr_E1,Theta, total.info=TRUE) 
plot(Theta,info1,type='l',main='Test information Curve')

#retirar tudo com a (discriminacao) abaixo de 0.34 em modulo 


dataE2 <-data[,c(41: 44,47,50:56,58,59,61:65,68,70,80)]

myModelE2 <- '
   # latent variables
        E2 =~ EFear1 + EFear2 + EFear3 + EFear4  + EFear7  + EFear10  +EAnxi1+  EAnxi2 + EAnxi3 + EAnxi4 + EAnxi5 + EAnxi6  + EAnxi8 + EAnxi9  + EDepe1 + EDepe2 +  EDepe3 + EDepe4 + EDepe5+ EDepe8 +EDepe10 +ESent10
'

fitE2 <- cfa(model = myModelE2,
            data  = dataE2, estimator="WLS")
summary (fitE2, fit.measures = TRUE)

irtE2 <- mirt(dataE2,1, itemtype = "gpcm", SE = TRUE)
M2(irtE2)
itemfit(irtE2)
coef(irtE2,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com a (discriminacao) abaixo de 0.4 em modulo 

dataE3 <-data[,c(41,42, 44,47,51:56,59,61,63:65)]

myModelE3 <- '
   # latent variables
        E3 =~ EFear1 + EFear2  + EFear4  + EFear7  +EAnxi1+  EAnxi2 + EAnxi3 + EAnxi4 + EAnxi5 + EAnxi6  + EAnxi9  + EDepe1 +  EDepe3 + EDepe4 + EDepe5 
'

fitE3 <- cfa(model = myModelE3,
             data  = dataE3, estimator="WLS")
summary (fitE3, fit.measures = TRUE)

irtE3 <- mirt(dataE3,1, itemtype = "gpcm", SE = TRUE)
M2(irtE3)
itemfit(irtE3)
coef(irtE3,simplify = TRUE, IRTpars = TRUE)$items

#Plot curva de informacao
extr_E3<-extract.item(irtE3,2) 
info3<-iteminfo(extr_E3,Theta, total.info=TRUE) 
plot(Theta,info3,type='l',main='Test information Curve')
