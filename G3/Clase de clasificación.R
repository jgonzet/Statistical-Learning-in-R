library(ISLR)
library(glmnet)
library(ggplot2)
library(faraway)
library(MASS)
library(GGally)
library(class)

# EJEMPLO--------

datos<-Default

#quiero ver si la variable balance me sirve para predecir

gg<-ggplot(data=datos, aes(x=balance,y=default, col=default))+
  geom_point()+
  scale_color_manual(values=c("skyblue", "chocolate"))+
  theme_light()

gg


#si lo quiero ver en función de las dos variables...

gg2<-ggplot(data=datos, aes(x=balance,y=income,col=default))+
  geom_point()+
  scale_color_manual(values=c("skyblue", "chocolate"))+
  theme_light()
gg2


#con plot
#colores<-c("skyblue", "chocolate")
#plot(datos$balance,datos$income,col=colores[datos$default], 
 #    pch=20, xlab="Balance", ylab="Income")
#legend(
 # x ="topright",
  #legend = levels(datos$default),
#  col = colores,
#  pch = 19, # same as pch=20, just smaller
#  cex = .7 # scale the legend to look attractively sized
#)

gg3<-ggplot(data=datos, aes(y=balance,fill=default))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue", "chocolate"))+
  theme_light()

gg3

gg4<-ggplot(data=datos, aes(y=income,fill=default))+
  geom_boxplot()+
  scale_fill_manual(values=c("skyblue", "chocolate"))
gg


#-------------------------
# Vecinos mas cercanos
#-------------------------

vecinos.k<-function(x,k,x0)
{
  distancia<-c()
  for(i in 1:length(x))
  {
    distancia[i]<-sqrt(sum(x[i]-x0)^2)
  }
  minimos<-sort(distancia,index.return=TRUE)$ix
  return(minimos[1:k])
}

help(sort)

clasificador<-function(x,y,k,x0)
{
  y_elegidos<-y[vecinos.k(x,k,x0)]
  prediccion<-ifelse(mean(y_elegidos=="No")>0.5,"No","Si")
  return(prediccion)
}

#si no quiero depender de saber que hay adentro de y, o hay mas clases...

clasificador2<-function(x,y,k,x0)
{
  prediccion<-which.max(table(y[vecinos.k(x,k,x0)]))
  return(names(prediccion))
}


clasificador2(datos$balance,datos$default,5,1500)
clasificador(datos$balance,datos$default,5,1700)
clasificador2(datos$balance,datos$default,5,2000)


# y si no con R...
knn(train=datos$balance,cl=datos$default,test=1500,k=5)
knn(train=datos$balance,cl=datos$default,test=1700,k=5)
knn(train=datos$balance,cl=datos$default,test=2000,k=5)

#-------------------------
# Regresión logística
#-------------------------
datos<-Default
levels(datos$default)<-c(0,1)
datos$default<-as.numeric(as.character(datos$default))

colores<-c("skyblue", "chocolate")
plot(datos$balance,datos$default,pch=20, 
     col=colores[as.factor(datos$default)],
     ylim = c(-0.5,1.5))

reg<-lm(default~balance,data=datos)
abline(reg) #no sirve para este caso


logistica<-glm(default~balance,data=datos,family =binomial)
summary(logistica)

colores<-c("skyblue", "chocolate")
orden<-order(datos$balance)
plot(datos$balance,datos$default,pch=20, col=colores[datos$default])
lines(datos$balance[orden],logistica$fitted.values[orden], col="darkblue",lwd=2)
abline(h=0.5)


#-----------------------------
# Evaluación del clasificador
#-----------------------------

set.seed(27)
train<-sample(1:10000,8000)  # esto saca uniforme y sin repetir, ya que lo uso para generar posiciones
datos.E<-datos[train,]       # las train
datos.T<-datos[-train,]      # todas menos las train

# quiero clasificar los puntos de datos.T con la muestra de datos.E y luego contar la cantidad de puntos mal clasificados

prediccion<-c()
for(i in 1:nrow(datos.T)) #clasifico usando balance
{
  prediccion[i]<-clasificador(datos.E$balance,datos.E$default,5,datos.T$balance[i])
}

tabla<-table(prediccion,datos.T$default)
mal_clasB<-(tabla[2,1]+tabla[1,2])/sum(tabla)


prediccion<-c()
for(i in 1:nrow(datos.T)) #clasifico usando income
{
  prediccion[i]<-clasificador(datos.E$income,datos.E$default,5,datos.T$income[i])
}

tabla<-table(prediccion,datos.T$default)
mal_clasI<-(tabla[2,1]+tabla[1,2])/sum(tabla)


#-------------------------
# LDA
#-------------------------

library(MASS)

ajuste_lda<-lda(default~balance+income,data=datos)
  par(mar = rep(4, 4))
plot(ajuste_lda)
predichos<-predict(ajuste_lda)
table(predichos$class,datos$default)
