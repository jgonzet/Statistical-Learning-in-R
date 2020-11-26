library(ggplot2)
library(GGally)

data("mtcars")

autos<-mtcars[,c(1:3,6,8)]

#analizo relación
ggpairs(autos)

#a) Sólo considero X2
#x: wt, y: mpg

g2<-ggplot(autos, aes(x=wt, y=mpg)) + 
  geom_point()+
  xlab("Peso del motor")+
  ylab("Rendimiento")
g2

#Buscamos calcular el intervalo de confianza para la respuesta.
#realizamos el ajuste
reg<-lm(mpg~wt,data=autos)
LI<-summary(reg)$coef[2,1]-qt(0.975,32-2)*summary(reg)$coef[2,2]
LS<-summary(reg)$coef[2,1]+qt(0.975,32-2)*summary(reg)$coef[2,2]


#Una forma de graficarlo sin hacer las cuentas, con ggplot2
g2+ geom_smooth(method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor", 
       y="Consumo", x="Peso del motor")


# Calculamos los intervalos usando la fórmula

# a) Intervalos de CONFIANZA
n<-nrow(autos)

X<-model.matrix(reg)

p<-ncol(X) #cant de parametros a estimar

res<-reg$residuals 

s2<-t(res)%*%res/(n-p)

#s2<-summary(reg)$sigma^2

A<-solve(t(X)%*%X)

#Podemos hacer la cuenta a mano...

IC<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  IC[i,]<-c(reg$fitted.values[i]-qt(0.975,n-p)*sqrt(s2[1,1]*t(X[i,])%*%A%*%X[i,]),
            reg$fitted.values[i]+qt(0.975,n-p)*sqrt(s2[1,1]*t(X[i,])%*%A%*%X[i,])) 
}


# O dejamos que lo haga R

int<-predict(reg,interval = "confidence", level = 0.95) 


# b) Intervalos de PREDICCION

ICP<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  ICP[i,]<-c(reg$fitted.values[i]-qt(0.975,n-p)*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,])),
             reg$fitted.values[i]+qt(0.975,n-p)*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,]))) 
}


# o con R

intP<-predict(reg,interval = "prediction", level = 0.95)

# Creamos un nuevo data frame con toda la información

intervalos<-data.frame(cbind(autos,IC,ICP))

g<-ggplot(intervalos)+
  geom_point(aes(x=wt,y=mpg))+
  geom_line(aes(x=wt,y=X1), col="skyblue")+
  geom_line(aes(x=wt,y=X2), col="skyblue")+
  geom_line(aes(x=wt,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X2.1), col="chocolate",lty=4)+
  geom_smooth(aes(x=wt,y=mpg),method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor - Intervalos", 
       y="Consumo", x="Peso del motor")+
  theme_light()
g


# Bandas de confianza

BC<-matrix(0,nrow = n,ncol=2)
for(i in 1:n)
{
  BC[i,]<-c(reg$fitted.values[i]-sqrt(p*qf(0.975,p,n-p))*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,])),
            reg$fitted.values[i]+sqrt(p*qf(0.975,p,n-p))*sqrt(s2[1,1]*(1+t(X[i,])%*%A%*%X[i,]))) 
}

intervalos<-data.frame(cbind(autos,IC,ICP,BC))
g<-ggplot(intervalos)+
  geom_point(aes(x=wt,y=mpg))+
  geom_line(aes(x=wt,y=X1), col="skyblue")+
  geom_line(aes(x=wt,y=X2), col="skyblue")+
  geom_line(aes(x=wt,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X2.1), col="chocolate",lty=4)+
  geom_line(aes(x=wt,y=X1.2), col="chartreuse4",lwd=1.5)+
  geom_line(aes(x=wt,y=X2.2), col="chartreuse4",lwd=1.5)+
  geom_smooth(aes(x=wt,y=mpg),method="lm", col="firebrick",se=FALSE)+
  labs(title="Consumo Vs peso del motor - Intervalos", 
       y="Consumo", x="Peso del motor")+
  theme_light()
g




####################################
# Algunas cosas más
# Agregamos una variable categórica: 

# ahora vamos a estimar los parámetros.
#si el modelo solo tiene a X2:

reg<-lm(mpg~wt,data=autos)

summary(reg)

reg2<-lm(mpg~wt+vs,data=autos)
summary(reg2)


c<-c(0,0,1)
X<-model.matrix(reg2)
beta<-reg2$coefficients
s2<-(t(reg2$residuals)%*%reg2$residuals)/(32-3)
sqrt(s2) #lo que aparece en el summary

f<-(t(c)%*%beta)%*%solve(t(c)%*%solve(t(X)%*%X)%*%c)%*%t(c)%*%beta/s2
sqrt(f) #lo que aparece en el summary

#en el grafico original separando por cilindros (que es una variable categórica)
autos$cyl<-factor(autos$cyl, labels = c("4","6","8"))
autos$vs<-factor(autos$vs, labels = c("V-Shaped","Straight"))

ggplot(autos, aes(x=wt, y=mpg)) + 
  geom_point()+
  xlab("Peso del motor")+
  ylab("Rendimiento")+
  geom_point(aes(shape=cyl, col=cyl), size=3)


