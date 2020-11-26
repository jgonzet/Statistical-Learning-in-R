#setwd("~/Documents/AE/R/Guia 2")
library(ggplot2)
library(GGally)

vapor<-read.table("vapor.txt")
reg<-lm(V3~V2,data=vapor)

x<-model.matrix(reg)
n<-nrow(x)
p<-ncol(x)
A<-solve(t(x)%*%x)
y_sombrero<-reg$fitted.values
s2<-t(reg$residuals)%*%reg$residuals/(n-p)

t_crit<-qt(0.975,n-p)

#Intervalos de confianza:
IC<-matrix(0,nrow=nncol=p)
for(i in 1:n){
  IC[i,]<-c(y_sombrero[i]-t_crit*sqrt(s2*t(x[i,])%*%A%*%x[i,]),
            y_sombrero[i]+t_crit*sqrt(s2*t(x[i,])%*%A%*%x[i,])) 
}


#Intervalos de prediccion:
ICP<-matrix(0,nrow=n,ncol=p)
for(i in 1:n)
{
  ICP[i,]<-c(y_sombrero[i]-t_crit*sqrt(s2*(1+t(x[i,])%*%A%*%x[i,])),
             y_sombrero[i]+t_crit*sqrt(s2*(1+t(x[i,])%*%A%*%x[i,]))) 
}

#Graficamos:

intervalos<-data.frame(cbind(vapor,IC,ICP))

ggplot(intervalos)+
  geom_point(aes(x=V2,y=V3))+
  geom_line(aes(x=V2,y=X1), col="skyblue")+
  geom_line(aes(x=V2,y=X2), col="skyblue")+
  geom_line(aes(x=V2,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=V2,y=X2.1), col="chocolate",lty=4)+
  geom_smooth(aes(x=V2,y=V3),method="lm", col="green",se=FALSE)+
  labs(title="Vapor Vs Lo que sea X - Intervalos", 
       y="Vapor", x="Lo que sea X")+
  theme_light()


#Bandas de confianza:
f_crit<-qf(0.975,p,n-p)
BC<-matrix(0,nrow = n,ncol=p)
for(i in 1:n)
{
  BC[i,]<-c(y_sombrero[i]-sqrt(p*f_crit)*sqrt(s2*(1+t(x[i,])%*%A%*%x[i,])),
            y_sombrero[i]+sqrt(p*f_crit)*sqrt(s2*(1+t(x[i,])%*%A%*%x[i,]))) 
}

intervalos<-data.frame(cbind(vapor,IC,ICP,BC))
g<-ggplot(intervalos)+
  geom_point(aes(x=V2,y=V3))+
  geom_line(aes(x=V2,y=X1), col="skyblue")+
  geom_line(aes(x=V2,y=X2), col="skyblue")+
  geom_line(aes(x=V2,y=X1.1), col="chocolate",lty=4)+
  geom_line(aes(x=V2,y=X2.1), col="chocolate",lty=4)+
  geom_line(aes(x=V2,y=X1.2), col="chartreuse4",lwd=1.5)+
  geom_line(aes(x=V2,y=X2.2), col="chartreuse4",lwd=1.5)+
  geom_smooth(aes(x=V2,y=V3),method="lm", col="firebrick",se=FALSE)+
  labs(title="Vapor Vs X - Intervalos", 
       y="Vapor", x="X")+
  theme_light()
g
