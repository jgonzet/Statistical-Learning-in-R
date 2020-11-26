library(leaps)
library(readxl)
library(GGally)

#Producción de biomasa en el estuario Cape Fear. 3 tipos de vegetación en 3 regiones. 
#Se analizan variables del sustrato

bio<-read_excel("Biomasa.xls")
attach(bio)
x<-cbind(K,SODIO,PH,SAL,ZN)

ggpairs(bio)
cor(bio)
reg<-lm(BIO~K+SODIO+PH+SAL+ZN)
summary(reg)

#pruebo con seleccion de variables

forw<-regsubsets(BIO~x,data = bio, method = "exhaustive")

summary(forw)

par(mfrow=c(2,2))
plot(summary(forw)$rss,pch=20,xlab="Modelo", ylab= "RSS")
plot(summary(forw)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(1:5,summary(forw)$cp,pch=20,ylim=c(0,8),xlab="Modelo", ylab= "CP")
abline(0,1)
plot(summary(forw)$bic,pch=20,xlab="Modelo", ylab= "BIC")
par(mfrow=c(1,1))

#el modelo 2 es el mejor

# es el que tiene sodio y ph


####################################



reg2<-lm(BIO~SODIO+PH)
summary(reg2)

#Probamos con la función LEAPS para ver todos los casos posibles

#CP
cp<-leaps(x,BIO,method = "Cp")
plot(cp$size,cp$Cp,ylim = c(0,7), pch=20)
abline(0,1, col="firebrick")
identify(cp$size,cp$Cp,n=1)
#el mejor parece el 17, tiene 4 variables, considerando el criterio Cp

cp$which[17+5,]

#si considero el criterio de R2 ajustados
r2a<-leaps(x,BIO,method = "adjr2")
plot(r2a$size, r2a$adjr2)
which.max(r2a$adjr2)
r2a$which[26,]



