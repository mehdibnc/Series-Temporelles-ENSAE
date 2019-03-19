setwd("../Serie temporelles/projet")

#Requirements
library(readxl)
library(zoo)


#Chargement des données
data <- read_excel("D:/Documents/ensae cours/S2/Serie temporelles/projet/series_longues_ipi_201712 (1).xls")

#Nettoyage et formats
st <- data[,c("NAF rev. 2","(C2)")]
colnames(st) <- c("Date","valeur")
st$Date <- lapply(st$Date,as.character)

date <- function(x){
  return(paste(paste(substr(x, 1, 4),paste("-",substr(x, 5, 6),sep=""),sep=""),"-01",sep=""))
}

st$Date <- lapply(st$Date,date)
st$Datef <- lapply(st$Date,as.Date)


#plot de la série temp
plot.ts(st$valeur)
