setwd("~/Documents/AE/R/Guia 2")
library(ggplot2)
library(GGally)
library(readxl)

cemento<-read_excel("cemento.xls")
cemento<-cemento[,2:7] #quito la columna "obs"
#ggpairs(cemento)

reg<-lm(y~x1+x2+x3+x4+x5,data=cemento)
summary(reg)

#las variables mas significativas parecen ser(orden descendente):X3,X4,X2,X1,X5

#voy a calcularlo de forma manual:
x<-model.matrix(reg) #matriz de disenio
X<-x[,2:6] #si quisiera eliminar el intercept
X<-x
n<-nrow(X)
p<-ncol(X)

y<-cemento$y
beta_sombrero<-solve(t(X)%*%X)%*%t(X)%*%y
y_sombrero<-X%*%beta_sombrero
residuos<-y-y_sombrero
s2<-t(residuos)%*%residuos/(n-p) #esta es la forma correcta de hacerlo
s2<-s2[1] #necesito pasar de matriz(1x1) a numero
A<-solve(t(X)%*%X)
covarianza<-A*s2
beta_se<-sqrt(abs(covarianza))
beta_se

#de forma automatica con lm():
reg2<-lm(y~x1+x2+x3+x4+x5-1,data=cemento)
summary(reg2)


reg3<-lm(y~x2+x3+x4-1,data=cemento)
summary(reg3)


reg4<-lm(y~x2+x3+x4,data=cemento)
summary(reg4)
