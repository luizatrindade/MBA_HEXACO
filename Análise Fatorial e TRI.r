#Instalação e carregamento de pacotes
install.packages("lavaan")
install.packages("mirt")

library(lavaan)
library(lavaanPlot)
library(mirt)


#carregamento de dados-link no readme

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

