library(mlbench)
library(GGally)
library(car)
data("BostonHousing")
attach(BostonHousing) #invoca variables

X<-cbind(nox,rm,lstat,medv) #selecciono las variables que quiero

#a)Vemos la correlacion entre variables
cor(X)#correlacion
ggpairs(as.data.frame(X))

#Calculams la regresion en las 3 variables
reg<-lm(medv~nox+rm+lstat,data=BostonHousing)
summary(reg)$fstatistic
?summary.lm
vif(reg)

#b)Calculamos si la regresion es significativa:
x<-model.matrix(reg)
n<-nrow(X)
p<-ncol(X)
beta<-reg$coefficients
c<-cbind(c(0,0,0),c(1,0,0),c(0,1,0),c(0,0,1)) #combinacion de betas a testear en H0
s2<-(t(reg$residuals)%*%reg$residuals)/(n-p) #desvio residuos
q<-qr(c)$rank
f<-(t(c%*%beta) %*%solve(c%*%solve(t(x)%*%x)%*%t(c))%*%(c%*%beta))/(q*s2)

#d)Matriz de correlacion de los betas: (Xt*X)*Sigma^2
y<-medv
y_sombrero<-x%*%beta
residuos<-y-y_sombrero

s2<-t(residuos)%*%residuos/(n-p) #esta es la forma correcta de hacerlo
s2<-s2[1] #necesito pasar de matriz(1x1) a numero
A<-solve(t(x)%*%x)
covarianza<-A*s2 #Matriz de covarianza de betas
beta_se<-sqrt(abs(covarianza)) #error standar betas
beta_se

