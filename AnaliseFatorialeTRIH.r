#Instalação e carregamento de pacotes
install.packages("lavaan")
install.packages("mirt")

library(lavaan)
library(lavaanPlot)
library(mirt)
library(DescTools)


#carregamento de dados-link no readme
data <- read_delim("localdoarquivo")
  
#analise H inicial
dataH <-data[,1:40]
Theta<-matrix(seq(-5,5, by=.1))

myModelH <- '
  # latent variables
     H =~ HSinc1 + HSinc2 + HSinc3 + HSinc4 + HSinc5 + HSinc6 + HSinc7 + HSinc8 + HSinc9 + HSinc10+ HFair1 + HFair2 + HFair3  +HFair4  +HFair5+  HFair6+  HFair7 + HFair8 + HFair9 + HFair10+ HGree1 + HGree2 + HGree3 + HGree4 + HGree5 + HGree6 + HGree7 + HGree8  +HGree9 + HGree10+HMode1 + HMode2 + HMode3 + HMode4 + HMode5 + HMode6 + HMode7+  HMode8 + HMode9 + HMode10
    
 '

 fit <- cfa(model = myModelH,
           data  = dataH, estimator="WLS")
 summary (fit, fit.measures = TRUE)
irtH <- mirt(dataH,1, itemtype = "gpcm", SE = TRUE)
M2(irtH)
itemfit(irtH)
coef(irtH,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com "a" (discriminacao) abaixo de 0.34 em modulo, itens com baixissima discriminacao 

dataH2<-dataH[,c(2:7,11,17:20,24:27,30:31, 35:37,39,40)]

myModelH2 <- '
   # latent variables
    H2 =~  HSinc2 + HSinc3 + HSinc4 + HSinc5 + HSinc6 + HSinc7 + HFair1 +  HFair7 + HFair8 + HFair9 + HFair10+ HGree4 + HGree5 + HGree6 + HGree7 + HGree10+HMode1 + HMode5 + HMode6 + HMode7 + HMode9 + HMode10
   
'

fit2 <- cfa(model = myModelH2,
           data  = dataH2, estimator="WLS")
summary (fit2, fit.measures = TRUE)

irtH2 <- mirt(dataH2,1, itemtype = "gpcm", SE = TRUE)
M2(irtH2)
itemfit(irtH2)
coef(irtH2,simplify = TRUE, IRTpars = TRUE)$items

#retirada de itens com as menores discriminacoes
dataH3<-dataH[,c(2:7,17,19,20,24:26,30, 35:36,39,40)]

myModelH3 <- '
   # latent variables
    H3 =~  HSinc2 + HSinc3 + HSinc4 + HSinc5 + HSinc6 + HSinc7  +  HFair7 + HFair9 + HFair10+ HGree4 + HGree5 + HGree6  + HGree10 + HMode5 + HMode6 + HMode9 + HMode10
   
'

fit3 <- cfa(model = myModelH3,
            data  = dataH3, estimator="WLS")
summary (fit3, fit.measures = TRUE)

irtH3 <- mirt(dataH3,1, itemtype = "gpcm", SE = TRUE)
M2(irtH3)
itemfit(irtH3)
coef(irtH3,simplify = TRUE, IRTpars = TRUE)$items

#retirar tudo com as menores discriminacoes

dataH4<-dataH[,c(2:4,5,6,17,19,20,24:26,30, 35:36,39)]

myModelH4 <- '
   # latent variables
    H4 =~    HSinc2 + HSinc3 + HSinc4 + HSinc5 + HSinc6+  HFair7 + HFair9 + HFair10+ HGree4 + HGree5 + HGree6  + HGree10 + HMode5 + HMode6 + HMode9 
'

fit4 <- cfa(model = myModelH4,
            data  = dataH4, estimator="WLS")
summary (fit4, fit.measures = TRUE)

irtH4 <- mirt(dataH4,1, itemtype = "gpcm", SE = TRUE)
M2(irtH4)
itemfit(irtH4)
coef(irtH4,simplify = TRUE, IRTpars = TRUE)$items

#curva de informacao inicial
extr_H<-extract.item(irtH,2) 
infoH<-iteminfo(extr_H,Theta, total.info=TRUE) 
plot(Theta,infoH,type='l',main='Test information Curve')

#curva de informacao final
extr_H4<-extract.item(irtH4,2) 
infoH4<-iteminfo(extr_H4,Theta, total.info=TRUE) 
plot(Theta,infoH4,type='l',main='Test information Curve')

#cálculo de área sob a curva
DescTools::AUC(x=Theta, y=infoH,method = c("trapezoid"))
DescTools::AUC(x=Theta, y=infoH4,method = c("trapezoid"))

