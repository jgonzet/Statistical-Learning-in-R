#setwd("~/Documents/AE/R/Guia 1")
gradua2<-read.table("graduados.txt")
x<-gradua2$V1

x_raya<-mean(x) #media podada 0.1
s<-sd(x)
var_x<-var(x)
iqr<-IQR(x) #distancia intercuantil

mad(x) #median absolute deviation: desvio a la mediana

boxplot(x) #c) se ven los cuantiles, min, max, mediana.
qqnorm(x) #d) podemos comparar los datos con los cuantiles de una normal
