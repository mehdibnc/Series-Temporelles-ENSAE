
setwd("C:\\Users\\benoit\\Desktop\\ENS\\ENSAE")

#Requirements
library(lmtest)
library(zoo)
library(tseries)
library(readr)
library(xtable)


#Chargement des donn�es
data <- read_delim("C:/Users/benoit/Desktop/ENS/ENSAE/2A/S2/S�ries temp/Projets/valeurs_mensuelles.csv",";", escape_double = FALSE)

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

#La s�rie n'a pas l'air d'�tre stationnaire � l'oeil.
#v�rifions d'abord par un acf (non formel) puis par
# un test de Dickey Fuller: 

#acf
acf(serie_indice, lag.max=349)
#La lente d�croissance des autocorrelations (significativement diff�rentes de 0) indique
#de la non stationnarit�.


#DF
adf.test(serie_indice) #on ne rejette pas la non stationnarit�

#PP test
pp.test(serie_indice) #ne rejette pas non plus la stationnarit�.


#tranformation pour rendre stationnaire.
Dt<-diff(serie_indice) #diff � l'ordre 1
plot(Dt, ylab="S�rie diff�renci�e")



acf(Dt, lag.max = 50, main = "")
pacf(Dt,lag.max = 50, main = "")
adf.test(Dt) #�a c'est ok pour k=0,1,2,3,4,5,6

#PP test
pp.test(Dt)


#Question 3
plot.ts(Dt,ylab="S�rie Diff�renci�e")




#Partie 2 
#Quesion 4 : 
monthplot(serie_indice, xlab = "Mois", ylab = "Valeurs")


#Identification du mod�le 

#arima (1,0,0) soit AR(1)
arima(serie_indice,c(1,0,0))
#le coeff ar1 est proche de 1 ce qui confirme la non stationnait� comme on le savait d�j�
bic=AIC(arima(serie_indice,c(1,0,0)),k = log(length(serie_indice)))
bic
#arima (1,1,0)
arima(serie_indice,c(1,1,0))
bic=AIC(arima(serie_indice,c(1,1,0)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,0)(1,1,0)
arima(serie_indice, c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
bic=AIC(arima(serie_indice, c(1,1,0), seasonal=list(order=c(1,1,0),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(1,1,0)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,0),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,0),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(1,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,1),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,1),period=12)),k = log(length(serie_indice)))
bic


#SARIMA_12 (1,1,1)(0,1,1)
model <- arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12))
#le mieux :)
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12)),k = log(length(serie_indice)))
bic



#SARIMA_12 (1,1,1)(2,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(2,1,1),period=12)),k = log(length(serie_indice)))
bic




#Etude des r�sidus
resi <- residuals(model)

plot(density(resi)) #pas mal

ks.test(resi,"pnorm",0,1)

acf(resi) #fantastique
pacf(resi) #fantastique

ret=c(1:12)
Box.test.2(model,nlag=ret,type="Ljung-Box",fitdf=3)

resi <- residuals(model)
densit�_residus = density(resi)
densit�_th�orique = dnorm(densit�_residus$x, mean=mean(resi),sd=sd(resi))
hist(resi, breaks = 100,main = "", xlab = "R�sidus de l'estimation
     par SARIMA (1,1,1)(0,1,1)", ylab = "Probabilit�",freq=FALSE)
lines(densit�_residus,xaxt="n",yaxt="n",ylab="",xlab="",main="",col = "blue",lwd = 2)
lines(densit�_residus$x,densit�_th�orique,xaxt="n",yaxt="n",ylab="",xlab="",main="",col =
        "blue",lwd=2,lty = 2)

#significativit� ?
coeftest(model) #Tous � 1%


#Portmanteau
serial.test(series, lags.pt = 12, type = "PT.asymptotic")







