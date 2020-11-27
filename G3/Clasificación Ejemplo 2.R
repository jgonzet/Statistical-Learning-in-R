# Regresion Logistic, LDA, QDA, y KNN

# The Stock Market Data
# Variables:
# % de ganancia de las acciones entre 2001 y 2005
# Lag1 ... Lag 5: % de ganancia de los 5 dias anteriores
# Volumen: numero de acciones que se movieron el dia anterior
# Today: % de la ganancia de ese dia
# Direction: Si el mercado subio o bajo ese dia
# Año

library(GGally)
library(MASS)
library(ISLR)
names(Smarket) #Vemos que variables tiene

dim(Smarket)
summary(Smarket)

ggpairs(Smarket) # Todas las variables una en contra de la otra

# Correlaciones.La columna 9 tiene variable cualitativa. No se observa correlacion entre la ganancia de hoy y los dias previos
cor(Smarket[,-9])
attach(Smarket) #para que me reconozca las variables

plot(Volume)
plot(Volume~Year)

# Queremos predecir la variable Direccion, en funcion de las variables Lag1 a Lag5 y volumen
# usaremos para esto regresion logistica, con el comando glm. Significa generalized linear models. 
# dentro de ella, regresion logistica es la correspondiente a la familia binomial

# Regresión Logistica

ajuste_rl<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket,family=binomial) #Binomial=Logistic Regresion
summary(ajuste_rl) #Observar que todos los pv son...

probs<-predict(ajuste_rl,type="response") # response es para que me de las probabilidades

probs[1:10]

contrasts(Direction)
#me dice cual de los dos me tomo como 1 entonces se cual es la probabilidad que me calcula con predict

#para predecir si es up or down, tengo que decidir cual es el corte k
#si elijo k=0.5, si p>0.5 le digo que es up, si no tengo que buscar el mejor k


pred<-rep("Down", 1250)
pred[probs>0.5]<-"Up"

#tabla de confusóion
table(pred,Direction)

#es buena la prediccion?
(507+145)/1250
#predice bien el 52.16% de las veces, es ligeramente mejor que elegir al azar...
mean(pred==Direction) #asi lo hace tambien


#Validación cruzada
train<-(Year<2005)
Smarket.2005<-Smarket[!train,]
dim(Smarket.2005)
Direction.2005<-Direction[!train]
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train) #subset train es para que lo haga
                                                                                                  #solo con esas coordenadas
glm.probs<-predict(glm.fit,Smarket.2005,type="response")

glm.pred<-rep("Down",252)
glm.pred[glm.probs>.5]<-"Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) #En realidad el error era peor de lo que pensaba!!!


#claramente como predictor no es bueno, prefiero tirar una moneda y listo
#por ahi si uno remueve algunas variables predictoras que no parecen tener nada de relacion mejora
#porque estos predictores aumentan la varianza sin disminuir el sesgo
#dejamos entonces las que tienen el menor p-valor


glm.fit<-glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")
glm.pred<-rep("Down",252)
glm.pred[glm.probs>.5]<-"Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) #Mejoro

#si queremos predecir un nuevo valor
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")



# Lo mismo pero con LDA
library(MASS)

lda.fit<-lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,subset=train)
lda.fit

par(mar = rep(4, 4))
plot(lda.fit) #Notar que ambas se parecen muuucho
lda.pred<-predict(lda.fit, Smarket.2005) #Hago la prediccion en los datos que no estaban en el training
names(lda.pred)
lda.pred$class
lda.pred$posterior
dim(lda.pred$posterior)
lda.class<-lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)


# QDA
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-NN

library(class)
train.X<-cbind(Lag1,Lag2)[train,] #Con todo esto extraigo solo lo que preciso
test.X<-cbind(Lag1,Lag2)[!train,]
train.Direction<-Direction[train]
set.seed(27)
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252 #Es tan bueno como tirar una moneda
knn.pred<-knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
