library(readxl)
library(zoo)
library(lmtest)

#Ejercicio para practicar box-cox

library(EnvStats)
library(MASS)

#box cox

aceite<-read.table("oil.txt", header=TRUE)


#reg<-lm(aceite$WW~aceite$PP+aceite$FF)
reg<-lm(WW~PP+FF,data=aceite)
summary(reg)

rest<-rstandard(reg)

qqnorm(rest)
abline(0,1)

plot(reg$fitted.values,rest, pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

bc<-boxcox(aceite$WW~aceite$PP+aceite$FF)
lamda<-bc$x[which.max(bc$y)]
y2<-(aceite$WW^lamda-1)/lamda

reg2<-lm(y2~aceite$PP+aceite$FF)
summary(reg2)


rest2<-rstandard(reg2)
rest<-rstudent(reg2)

qqnorm(rest2)
abline(0,1)

plot(reg2$fitted.values,rest, pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)



#y si elijo el 0?

y3<-log(aceite$WW)
reg3<-lm(y3~aceite$PP+aceite$FF)
summary(reg2)


rest3<-rstandard(reg3)

qqnorm(rest3)
abline(0,1)

plot(reg3$fitted.values,rest3, pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)



