#Instalação e carregamento de pacotes
install.packages("lavaan")
install.packages("mirt")

library(lavaan)
library(lavaanPlot)
library(mirt)


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

