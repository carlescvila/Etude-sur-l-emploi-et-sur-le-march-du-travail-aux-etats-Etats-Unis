#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------DOSSIER DE TECHNIQUES DE PREVISION ET CONJONCTURE-----------------------
#--------------------------Carles CERDÁ VILA, M1 EKAP 2021-2022------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

# *******************************************************************************************
#Importation de la base de données
library(readxl)
setwd("C:/Users/carle/OneDrive/Escritorio/M1 EKAP/Techniques de prévision et conjoncture/Dossier TPC")
y<-read_xls("PAYNSA.xls")
y<-y[,-1]
yy<-ts(data = y, start = c(1990,01), frequency = 12)
plot(yy)

# *******************************************************************************************
#--------------------------------------------------------------------------------------------
#                                        PARTIE I
#--------------------------------------------------------------------------------------------

#Détection des points atypiques dans la série primitive
library(tsoutliers)
fit<-tso(yy)
plot(fit)
show(fit)

#Série ajustée
adj<-fit$yadj
plot(adj, ylab = "PAYNSA")
write(t(adj), file = "paynsa_outliers.csv", ncolumns = 1, append = FALSE)

#Statistiques descriptives
library(readr)
adj1<-read_csv("paynsa_outliers.csv", col_names = FALSE)
summary(adj1)
library(prettyR)
describe(adj1)
library(moments)
kurtosis(adj1)
skewness(adj1)

library(ggplot2)

ggplot(adj1, aes(X1)) + geom_histogram(fill = "dodgerblue1", bins = 50, color = "grey1") +
  labs(title = "Histogramme de la série corrig?e", x = "PAYNSA", y = "Fréquence") + 
  geom_vline(aes(xintercept = mean(X1)), color = "darkred", linetype = "dashed", size = 1)

ggplot(adj1, aes(X1)) + geom_boxplot(fill = "dodgerblue1", color = "grey1") + theme_minimal() +
  coord_flip() + scale_y_continuous(limits = c(-0.75, 0.75)) + scale_x_continuous(limits = c(105000,165000)) +
  labs(title = "Boite à moustache de la série corrigée", x = "PAYNSA")

#Détection de la saisonalité
library(seastests)

  #Test de Friedman
  ft <- fried(adj)
  show(ft)

  #Test de saisonnalité (TRUE ou FALSE)
  is <- isSeasonal(adj, test="wo")
  show(is)

  #Test de Kruskal-Wallis
  kwt <- kw(adj)
  show(kwt)

  #Test QS modifié
  qst <- qs(adj)
  show(qst)

  #Seasonal dummies
  sd <- seasdum(adj)
  show(sd)

  #Test de Welch
  w <- welch(adj)
  show(w)

  #Test de Webel-Ollech
  wot<-combined_test(adj)
  show(wot)
  
#Type de schéma
  #Test de Buys-Ballot
  library(pastecs)
  bbmat<-buysbal(yy)
  bbmat
  means_bb<-rowMeans(bbmat[1:32,])
  library(matrixStats)
  sd_bb<-rowSds(bbmat[1:32,])
  
  modbb<-lm(sd_bb ~ means_bb)
  summary(modbb)
  
  #Test log-level
  #Regarima
  #X13 method
  library(RJDemetra)
  myregx13 <- regarima_x13(adj, spec ="RG5c")
  summary(myregx13)
  par(mfrow = c(3,2))
  plot(myregx13)
    #Information sur l'spécification du modèle
    myregx13$model
    myregx13$model$effects
    #Résidus
    myregx13$residuals
    #Statistiques sur les résidus du modèle RegARIMA
    myregx13$residuals.stat 
  
  #Seasonal
  #Modèle additif
  library(seasonal)
  seasXadd<-seas(adj, transform.function = "none")
  final(seasXadd)
  par(mfrow = c(1,1))
  plot(seasXadd)
  summary(seasXadd)

  #Modèle multiplicatif
  seasXmul<-seas(adj, transform.function = "log")
  final(seasXmul)
  plot(seasXmul)
  summary(seasXmul)

# *******************************************************************************************
#--------------------------------------------------------------------------------------------
#                                      PARTIE II
#--------------------------------------------------------------------------------------------

#Décomposition
  #Ajustement de la série des valeurs atypiques
  adjseries1 = series(seasXadd, "a19")
  write(t(adjseries1),file="adj_seasonal.csv",ncolumn=1,append=FALSE)

par(mfrow = c(2,2))
plot(adjseries1, main = "Série primitive sans valeurs atypiques", ylab = "PAYNSA")  
plot(seasXadd, main = "Série originale et ajustée", ylab = "PAYNSA")
plot(trend(seasXadd), main ="Tendance de la série", ylab = "PAYNSA")
plot(irregular(seasXadd), main = "Variation résiduelle de la série", ylab = "PAYNSA")

udg(seasXadd, "x13mdl")

#Désaisonnalisation
#Méthode X13-ARIMA-SEATS
myspec <- x13_spec("RSA5c")
mysax13 <- x13(adjseries1, myspec)
summary(mysax13$regarima)
mysax13
par(mfrow = c(2,1))
plot(mysax13$final)


# *******************************************************************************************
#--------------------------------------------------------------------------------------------
#                                     PARTIE III
#--------------------------------------------------------------------------------------------

#Méthodes non-paramétriques, semi-paramétriques et paramétriques

  #X13-ARIMA-SEATS
  series(seasXadd, "forecast.forecasts")
  fore = series(seasXadd, "fct")
  fore[1]

  #STL
  library(forecast)
  fitstl = stlm(yy)
  show(fitstl)
  prevstl <- forecast(yy, 12)
  show(prevstl)

  prevstl = stlf(yy,12)
  show(prevstl)
  plot(prevstl)

  #STS
  library(stats)
  fitsts <- StructTS(yy)
  show(fitsts)
  prevsts <- forecast(fitsts,12)
  show(prevsts)
  
  #BSTS
  library(bsts)
  ss<-AddLocalLinearTrend(list(), yy)
  ss<-AddTrig(ss, yy, period = 12, frequencies = 1:3)
  modelbsts<-bsts(yy, state.specification = ss, niter = 500)
  show(modelbsts)
  pred<-predict(modelbsts, horizon = 12, burn = 100)
  show(pred)
  print(pred$distribution[1,1])
  
  #Bagged model
  fitbag<-baggedModel(yy[,1])
  fitbag
  prevbag<-forecast(fitbag,12)
  show(prevbag)
  
  #Graphiques
  par(mfrow = c(1,1))
  plot(fore, main = "Forecast from X13-ARIMA-SEATS")
  
  par(mfrow = c(2,2))
  plot(prevstl, ylab = "PAYNSA")
  plot(prevsts, ylab = "PAYNSA")
  plot(pred, ylab = "PAYNSA", main = "Forecast from BSTS")
  plot(prevbag, ylab = "PAYNSA")
  
#Méthodes de lissage exponentiel
  
  #Holt-Winters
    # lissage
    m <- HoltWinters(yy, seasonal = 'add') 
    show(m)
    summary(m)
    p<-predict(m, n.ahead = 12, prediction.interval = TRUE)
  fitHW<-forecast(m, h = 12)
  show(fitHW)
  
  #ETS
  fitets <- ets(yy)
  show(fitets)
  prevets <- forecast(fitets,12)
  show(prevets)
  
  #TBATS
  fittbats = tbats(yy)
  prevbats<-forecast(fittbats, 12)
  show(prevbats)
  show(fittbats)

  #ATA
  library(ATAforecasting)
  ata_fit <- ATA(yy, seasonal.test = TRUE, seasonal.model = "x13")
  ata_fit
  ata_fc <- ATA.Forecast(ata_fit, h=12)
  ata_accry <- ATA.Accuracy(ata_fc)
  ata_accry

  #Graphiques
  par(mfrow = c(2,2))
  plot(m, p)
  plot(fitHW, ylab = "PAYNSA")
  plot(prevets, ylab = "PAYNSA")
  plot(prevbats, ylab = "PAYNSA")
  par(mfrow = c(1,1))
  plot(ata_fc$fitted)
  
#Modèle SARIMA
out<-auto.arima(adj)
out1=arima(adj,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12))
show(out1)
p=predict(out1,12)
plot(forecast(out, h=12), ylab = "PAYNSA")
show(p)

#Erreurs de prévision
prednaiv<-snaive(yy, h=12)
show(prednaiv)

mse <- function(error){mean(error^2)}

msestl<-fitstl$model$mse
msex13<-mse(myregx13$residuals)
msests<-mse(prevsts$residuals)
msebsts<-mse(modelbsts$one.step.prediction.errors)
msebagmod<-mse(fitbag$residuals)
fitHW$residuals<-(fitHW$residuals[-c(1:12),])
mseHW<-mse(fitHW$residuals)
mseets<-fitets$mse
msetbats<-mse(residuals(fittbats))
ata_fit$residuals<-(ata_fit$residuals[-1,])
mseata<-mse(ata_fit$residuals)
mseSAR<-mse(out1$residuals)
prednaiv$residuals<-(prednaiv$residuals[-c(1:12),])
msenaiv<-mse(prednaiv$residuals)

#Test de précision
# Test on out-of-sample one-step forecasts
fcnaive=snaive(yy,h=12)
#Naive estimator
dm.test(residuals(ata_fit), residuals(fcnaive), h=12)
dm.test(residuals(fitSAR), residuals(fcnaive), h=12)
dm.test(residuals(fittbats), residuals(fcnaive), h= 12)
dm.test(residuals(fitets), residuals(fcnaive), h=12)
dm.test(residuals(fitHW), residuals(fcnaive), h=12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fcnaive), h = 12)
dm.test(residuals(modelbsts), residuals(fcnaive), h=12)
dm.test(residuals(fitsts), residuals(fcnaive), h=12)
dm.test(residuals(myregx13), residuals(fcnaive), h=12)
dm.test(residuals(fitstl), residuals(fcnaive), h=12)
#X13-ARIMA-SEATS
dm.test(residuals(ata_fit), residuals(myregx13), h=12) # pvalue > 0.05
dm.test(residuals(fitSAR), residuals(myregx13), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(myregx13), h= 12)
dm.test(residuals(fitets), residuals(myregx13), h=12)
dm.test(residuals(fitHW), residuals(myregx13), h=12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(myregx13), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(myregx13), h=12)
dm.test(residuals(fitsts), residuals(myregx13), h=12)
dm.test(residuals(fitstl), residuals(myregx13), h=12)
#SARIMA
dm.test(residuals(ata_fit), residuals(fitSAR), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(fitSAR), h= 12)
dm.test(residuals(fitets), residuals(fitSAR), h=12)
dm.test(residuals(fitHW), residuals(fitSAR), h=12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fitSAR), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitSAR), h=12)
dm.test(residuals(fitsts), residuals(fitSAR), h=12)
dm.test(residuals(fitstl), residuals(fitSAR), h=12)
#STL
dm.test(residuals(ata_fit), residuals(fitstl), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(fitstl), h= 12) # pvalue > 0.05
dm.test(residuals(fitets), residuals(fitstl), h=12) # pvalue > 0.05
dm.test(residuals(fitHW), residuals(fitstl), h=12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fitstl), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitstl), h=12)
dm.test(residuals(fitsts), residuals(fitstl), h=12) # pvalue > 0.05
#ETS
dm.test(residuals(ata_fit), residuals(fitets), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(fitets), h= 12)
dm.test(residuals(fitHW), residuals(fitets), h=12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fitets), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitets), h=12)
dm.test(residuals(fitsts), residuals(fitets), h=12) # pvalue > 0.05
#Holt-Winters
dm.test(residuals(ata_fit), residuals(fitHW), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(fitHW), h= 12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fitHW), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitHW), h=12)
dm.test(residuals(fitsts), residuals(fitHW), h=12) # pvalue > 0.05
#STS
dm.test(residuals(ata_fit), residuals(fitsts), h=12) # pvalue > 0.05
dm.test(residuals(fittbats), residuals(fitsts), h= 12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(fitsts), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitsts), h=12)
#ATA
dm.test(residuals(fittbats), residuals(ata_fit), h= 12) # pvalue > 0.05
dm.test(residuals(fitbag), residuals(ata_fit), h = 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(ata_fit), h=12)
#Bagged model
dm.test(residuals(fittbats), residuals(fitbag), h= 12) # pvalue > 0.05
dm.test(residuals(modelbsts), residuals(fitbag), h=12)
#TBATS
dm.test(residuals(modelbsts), residuals(fittbats), h=12)
