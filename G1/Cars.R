library(ggplot2)

#cargo los datos
datos<-cars


#defino las variables
x<-datos$speed #velocidad del auto
y<-datos$dist #distancia requerida de frenado


summary(x) # me da todos los parametros del boxplot

#grafico bonitos
ggplot(data=datos)+ 
  geom_point(aes(x=speed, y=dist))+
  labs(title="",
       x="Velocidad", 
       y = "Distancia de frenado")+
  theme_light()

x_raya<-mean(x) #promedio de los elementos de x
y_raya<-mean(y)

s_x<-sd(x) #devio estandar muestral
s_y<-sd(y) #devio estandar muestral

#estimar beta0 y beta1

X<-matrix(c(rep(1,50),x),byrow = FALSE,ncol=2)

unos<-rep(1,50)

X<-cbind(unos,x) #une columnas

# Operaciones con matrices:
  # solve(X) # te da la inversa
  # t(X) #transpuesta de X
  # X%*%X #producto entre matrices 

beta_sombrero<-solve(t(X)%*%X)%*%t(X)%*%y

beta_sombrero[1] #elemento 1 del vector

#X[1,3] # el elemento de la fila 1 y columna 3 de la matriz X

plot(x,y,pch=20)
#grafico mi recta predicha
y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*x
lines(x,y_sombrero,col="chocolate" )

points(x[10],y[10],col="red")

#otra forma de hacer la recta
abline(beta_sombrero[1],beta_sombrero[2],col="Forestgreen")


datos2<-data.frame(cbind(datos,y_sombrero))

ggplot(data=datos2)+ 
  geom_point(aes(x=speed, y=dist))+
  geom_line(aes(x=speed,y=y_sombrero),col="Firebrick")+
  labs(title="Regresion lineal",
       x = "Velocidad", 
       y = "Distancia de frenado")+
  theme_light()

S2<-t(y-y_sombrero)%*%(y-y_sombrero)/(50-2) #varianza de los residuos
sqrt(S2) #desvio residuos


# ahora la versión linda
help(lm)
ajuste<-lm(y~x)

names(ajuste)
summary(ajuste)$sigma
summary(ajuste)$desv

names(summary(ajuste))
beta_0<-ajuste$coefficients[1]
