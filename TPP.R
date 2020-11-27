setwd("~/Documents/R/Datos")

library(MASS)
library(GGally)

frecuencias<-read.csv("Vessel_X.txt",header = FALSE)
compuestos<-read.csv("Vessel_Y.txt",header = TRUE)

#vessels<-cbind(compuestos,frecuencias)
# Quiero los compuestos en funcion de las frecuencias:
