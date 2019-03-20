
setwd("../projet")

#Requirements
library(readxl)
library(zoo)
library(tseries)


#Chargement des données
data <- read_excel("../series_longues_ipi_201712 (1).xls")

#Nettoyage et formats
st <- data[,c("NAF rev. 2","(C2)")]
colnames(st) <- c("Date","valeur")
st$Date <- lapply(st$Date,as.character)

date <- function(x){
  return(paste(paste(substr(x, 1, 4),paste("-",substr(x, 5, 6),sep=""),sep=""),"-01",sep=""))
}

st$Date <- lapply(st$Date,date)
st[['Date']] <- strptime(st[['Date']], format='%Y-%m-%d')


#plot de la série temp
plot(st$valeur ~ as.Date(st$Date),type="l",xlab="Temps",ylab = "S_t")


#Question 2
#La série n'a pas l'air d'être stationnaire à l'oeil.
#vérifions d'abord par un acf (non formel) puis par
# un test de Dickey Fuller: 

#acf
acf(st$valeur, lag.max=334) #oulala !! c'est pas très bon 

#DF
adf.test(st$valeur,k=6) #on ne rejette pas la non stationnarité


#tranformation pour rendre stationnaire.
Dt<-diff(st$valeur) #diff à l'ordre 1
plot(Dt ~ as.Date(st$Date)[2:length(st$Date)],type="l",xlab="Temps",ylab = "D_t",col="orange") #top


acf(Dt) #bizarre 
adf.test(Dt,k=0) #ça c'est ok pour k=0,1,2,3,4,5,6

#Question 3
