
setwd("D:/Documents/ensae cours/S2/Serie temporelles/projet")

#Requirements
library(zoo)
library(tseries)
library(readr)
library(xtable)


#Chargement des données
data <- read.csv("D:/Documents/ensae cours/S2/Serie temporelles/projet/valeurs_mensuelles.csv",sep=";",header = TRUE)

data<-na.omit(data)
data <- data[2:nrow(data),] #le readcsv a mis une ligne de caracteres au debut du coup je l'enleve
colnames(data) <- c("Date","valeur","code")
data <- data[,c("Date","valeur")]

test <- data[1:12,]
train <- data[13:nrow(data),]

#Stats desc
summary(data$valeur)

#Question 2

serie_indice = stats::ts(train$valeur, start=c(1990,1), end=c(2019,01), frequency=12)
plot.ts(serie_indice)

#La série n'a pas l'air d'être stationnaire à l'oeil.
#vérifions d'abord par un acf (non formel) puis par
# un test de Dickey Fuller: 

#acf
acf(serie_indice, lag.max=349)
#La lente décroissance des autocorrelations (significativement différentes de 0) indique
#de la non stationnarité.


#DF
adf.test(serie_indice) #on ne rejette pas la non stationnarité

#PP test
pp.test(serie_indice) #ne rejette pas non plus la stationnarité.


#tranformation pour rendre stationnaire.
Dt<-diff(serie_indice) #diff à l'ordre 1
plot(Dt, ylab="Série différenciée")



acf(Dt, lag.max = 50)
pacf(Dt,lag.max = 50)
adf.test(Dt) #ça c'est ok pour k=0,1,2,3,4,5,6

#PP test
pp.test(Dt)


#Question 3
plot.ts(Dt,ylab="Série Différenciée")




#Partie 2 
#Quesion 4 : 
monthplot(serie_indice)


#Identification du modèle 

#arima (1,0,0) soit AR(1)
arima(serie_indice,c(1,0,0))
#le coeff ar1 est proche de 1 ce qui confirme la non stationnaité comme on le savait déjà

#arima (1,1,0)
arima(serie_indice,c(1,1,0))

#SARIMA_12 (1,1,0)(1,1,0)
arima(serie_indice, c(1,1,0), seasonal=list(order=c(1,1,0),period=12))


#SARIMA_12 (1,1,1)(1,1,0)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,0),period=12))

#SARIMA_12 (1,1,1)(0,1,1)
model <- arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12))
#le mieux :)

#SARIMA_12 (1,1,1)(1,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,1),period=12))

#SARIMA_12 (1,1,1)(2,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(2,1,1),period=12))





#Etude des résidus
resi <- residuals(model)

plot(density(resi)) #pas mal

ks.test(resi,"pnorm",0,1)

acf(resi) #fantastique
pacf(resi) #fantastique

ret=c(1:12)
Box.test.2(model,nlag=ret,type="Ljung-Box",fitdf=3)
