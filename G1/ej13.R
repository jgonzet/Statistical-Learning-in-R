#setwd("~/Documents/AE/R/Guia 1")
library(ggplot2)
abalones<-read.table("abalone.txt",sep=",")
#summary(abalones)

#Features: sexo(1),longitud(2),diametro(3),altura(4),
#          peso completo(5),peso carne(6),peso viscera(7),
#          peso caparazon(8),anillos(9)

long<-abalones$V2
diam<-abalones$V3

ajuste<-lm(diam~long)
#summary(ajuste)

beta_sombrero<-ajuste$coefficients
y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*long

ggplot(data=abalones)+ 
  geom_point(aes(x=V2, y=V3))+
  geom_line(aes(x=V2,y=y_sombrero),col="green")+
  labs(title="Regresion lineal",
       x = "Longitud", 
       y = "Diametro")+
  theme_light()

#pairs(abalones)

#inciso b):
y<-abalones$V5
x1<-abalones$V6
x2<-abalones$V7
x3<-abalones$V8
#reg<-lm(y~x1+x2+x3)
reg<-lm(V5~V6+V7+V8,data=abalones)


summary(reg)
beta_sombre<-reg$coefficients
y_sombre<-beta_sombre[1]+beta_sombre[2]*x1+beta_sombre[3]*x2+beta_sombre[1]*x3

#residuos<-y-y_sombre
#plot(residuos)

#=============================================================================================

#inciso c)
diam<-abalones$V3
diam2<-diam*diam
peso<-abalones$V5

#ajuste1<-lm(peso~diam+diam2)
ajuste2<-lm(V5~V3+diam2,data=abalones)

summary(ajuste2)

beta_sombre<-ajuste2$coefficients
y_sombre<-beta_sombre[1]+beta_sombre[2]*diam+beta_sombre[3]*diam2

ggplot(data=abalones)+ 
  geom_point(aes(x=V3,y=V5))+
  geom_line(aes(x=V3,y=y_sombre),col="green")+
  labs(title="Regresion lineal",
       x = "Longitud", 
       y = "Diametro")+
  theme_light()


#incido c) cubico

diam<-abalones$V3
diam3<-diam^3
peso<-abalones$V5

#ajuste1<-lm(peso~diam+diam2)
ajuste2<-lm(V5~diam3,data=abalones)

summary(ajuste2)

beta_sombre<-ajuste2$coefficients
y_sombre<-beta_sombre[1]+beta_sombre[2]*diam3

ggplot(data=abalones)+ 
  geom_point(aes(x=V3,y=V5))+
  geom_line(aes(x=V3,y=y_sombre),col="green")+
  labs(title="Regresion lineal",
       x = "Longitud", 
       y = "Diametro")+
  theme_light()

