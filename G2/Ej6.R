#setwd("~/Documents/AE/R/Guia 2")
library(ggplot2)
library(GGally)

peaks<-read.table("peak.txt")
peaks<-peaks[2:11]
#a)

ggpairs(peaks)

#X1:area de la cuenca(1),X2:area impermeable(2),X3:pendiente promedio terreno(3)
#X4:maxima longitud afluentes(4),X5:indice absorcion(5),X6:capacidad deposito suelo(6)
#X7:velocidad infiltracion(7),X8:cantidad de lluvia(8),X9:tiempo de lluvia>0.25 pulgada/hora(9)

Y<-peaks$V11 #Caudal de agua
X1<-peaks$V2
X2<-peaks$V3
X3<-peaks$V4
X4<-peaks$V5
X5<-peaks$V6
X6<-peaks$V7
X7<-peaks$V8
X8<-peaks$V9
X9<-peaks$V10
Y<-peaks$V11

n<-length(X1)
X1_raya<-mean(X1)
sd_X1<-sd(X1)
X2_raya<-mean(X2)
sd_X2<-sd(X2)

#Correlacion muestral: t(X1-X1_raya)%*%(X2-X2_raya)/(sd_X1*sd_X2*(n-1))
#cor(X1,X2)
#Covarianza muestral: t(X1-X1_raya)%*%(X2-X2_raya)/((n-1))
#cov(X1,X2)

#Las variables con mayor correlacion con Y son: X4(0.866),X1(0.781),X7 y X4 (0.667)

#b)
peaks<-log(peaks)
ggpairs(peaks)

#ahora las variables mas correlacionadas son: X4,X1,X2,X5 (en ese orden)
