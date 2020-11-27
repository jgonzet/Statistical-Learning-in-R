#
##Ejercicio 1

library(ISLR)
library(glmnet)
library(ggplot2)
library(glmnet)
library(faraway)
library(MASS)
library(GGally)



datos<-read.csv("kyphosis.csv")

gg<-ggplot(data=datos, aes(y=Age,fill=Kyphosis))+
  geom_boxplot()
gg

ajuste<-glm(Kyphosis~Age, data=datos, family = binomial)
summary(ajuste)

ajuste_todas<-glm(Kyphosis~Age+Number+Start, data=datos, family = binomial)
summary(ajuste_todas)
#Se observa que si tenemos en cuenta todas las variables, todas resultan significativas con un nivel de 0.1, pero solo Start resulta significativa con un nivel de 0.05.

#c Ajustamos ahora a un modelo de regresión logística como en el ítem a) pero agregando un término cuadrático.


Age2<-datos$Age^2
datos2<-cbind(datos,Age2)

ajuste2<-glm(Kyphosis~Age+Age2, data=datos2, family = binomial)
summary(ajuste2)


#Vemos que de esta manera, la variable Age sí resulta significativa.

ajuste3<-glm(Kyphosis~., data=datos2, family = binomial)
summary(ajuste3)

Number2<-datos$Number^2
datos3<-cbind(datos2,Number2)

ajuste4<-glm(Kyphosis~Number2+Age*Number, data=datos3, family = binomial)
summary(ajuste4) 





##Ejercicio 2


datos<-read.csv("binary.csv")
ggpairs(datos)

#Como las variables *rank* y *admit* son variables categóricas, podremos analizar mejor los datos si las transformamos a factores

datos$rank<-factor(datos$rank)
datos$admit<-factor(datos$admit)

ggpairs(datos) #se entiende un poco mejor

ajuste<-glm(admit~., data=datos, family=binomial)
summary(ajuste)


#Cuando la variable *GRE* aumenta en una unidad, el log odds de la admisión aumenta en $\hat{\beta}_{gre}$. Observando la salida del ajuste, este valor es 0.00226. 

#En cuanto a  la interpretación de las estimaciones de los coeficientes relacionados con la variable rank, si observamos la matriz de diseño, o la salida del ajuste, podemos ver que el modelo toma como nivel basal al caso en que el rango es 1, es decir, si agregamos variables dummies el modelo sería el siguiente:
#  $log odds=\beta_0+\beta_1 GRE+\beta_2 GPA +\beta_3 D_2+\beta_4 D_3+\beta_5 D_4$
  
 # Donde las variables $D_i$ valen 1 cuando la variable *rank* vale i. Entonces, cuando la variable *rank* vale 1, todas las dummies valen 0, y el modelo será 
#$log odds=\beta_0+\beta_1 GRE+\beta_2 GPA$
  
#  Por lo tanto, $\beta_3$ será el cambio en el log odds cuando paso de rank 1 a rank 2. Lo mismo para los otros coeficientes referidos a la variable *rank*.


#Buscamos $\hat{\pi}$ para los distintos niveles de la variable *rank* cuando las otras variables toman como valor la media muestral.

m_gre<-mean(datos$gre)
m_gpa<-mean(datos$gpa)

beta_sombrero<-ajuste$coefficients

r1<-c(1,m_gre,m_gpa,0,0,0)
r2<-c(1,m_gre,m_gpa,1,0,0)
r3<-c(1,m_gre,m_gpa,0,1,0)
r4<-c(1,m_gre,m_gpa,0,0,1)

pi1<-1/(1+exp(-t(beta_sombrero)%*%r1))
pi2<-1/(1+exp(-t(beta_sombrero)%*%r2))
pi3<-1/(1+exp(-t(beta_sombrero)%*%r3))
pi4<-1/(1+exp(-t(beta_sombrero)%*%r4))
c(pi1,pi2,pi3,pi4)


#El valor predicho para cada una de estas observaciones será 1 si $\hat{\pi}_i$ es mayor a 0.5, o 0 en caso contrario.

pred1<-1*(pi1>0.5)
pred2<-1*(pi2>0.5)
pred3<-1*(pi3>0.5)
pred4<-1*(pi4>0.5)
c(pred1,pred2,pred3,pred4)


# Buscamos las estimaciones de los cocientes de los odds cuando la variable $x_i$ aumenta en una unidad y el resto permanece constante, para cada i de 1 a 5.


codd1<-exp(ajuste$coef[1])
codd2<-exp(ajuste$coef[2])
codd3<-exp(ajuste$coef[3])
codd4<-exp(ajuste$coef[4])
codd5<-exp(ajuste$coef[5])
c(codd1,codd2,codd3,codd4,codd5)

IC<-matrix(0,ncol=2,nrow=5)

for(i in 1:5)
{
  IC[i,]<-exp(c(summary(ajuste)$coef[i,1]-qnorm(0.975)*summary(ajuste)$coef[i,2],
                summary(ajuste)$coef[i,1]+qnorm(0.975)*summary(ajuste)$coef[i,2])) 
}
colnames(IC)<-c("LI","LS")
rownames(IC)<-c("exp{beta 1}", "exp{beta 2}", "exp{beta 3}",
                "exp{beta 4}", "exp{beta 5}")

IC


anova(ajuste, test= "Chisq")

mod1<-glm(admit ~ 1,  data = datos, family = "binomial")
mod2<-glm(admit ~ gre,  data = datos, family = "binomial")
mod3<-glm(admit ~ gre+gpa,  data = datos, family = "binomial")
mod4<-glm(admit ~ gre+gpa+rank,  data = datos, family = "binomial")
anova(mod1,mod2,test="LRT")
anova(mod2, mod3, test="LRT")
anova(mod3, mod4, test="LRT")

#con Chisq

anova(mod1,mod2,test="Chisq")
anova(mod2, mod3, test="Chisq")
anova(mod3, mod4, test="Chisq")
#f
mod5<-glm(admit ~ gre+gpa,  data = datos, family = "binomial")
mod6<-glm(admit ~ gre+rank,  data = datos, family = "binomial")
mod7<-glm(admit ~ gpa+rank,  data = datos, family = "binomial")
anova(mod5,ajuste,test="Chisq")
anova(mod6,ajuste,test="Chisq")
anova(mod7,ajuste,test="Chisq")





#3 
library(MASS)
library(class)
library(klaR)
library(nnet)

iris<-read.table("iris.data", header = FALSE,sep=",")


#Usando los paquetes de R

attach(iris)
LDA<-lda(V5~V1+V2+V3+V4)
plot(LDA)
summary(LDA)


#Si quiero ver lo que hace proyectando las varialbes en las primeras 2 coordenadas disc.
LDA
z1<-t(LDA$scaling)%*%t(X1)
z2<-t(LDA$scaling)%*%t(X2)
z3<-t(LDA$scaling)%*%t(X3)

plot(z1[1,],z1[2,],pch = 22,xlim = c(-12,10), ylim = c(4,10))
points(z2[1,],z2[2,],pch = 16,col="red")
points(z3[1,],z3[2,],pch = 18,col="blue")

#con las funciones de R
pred1<-predict(LDA,datos)
aparente1<-mean(iris[,5]!=pred1$class)

#con CV
c<-1:150
pred<-c()
for(i in 1:150)
{
  train<-c[-i]
  LD<-lda(V5~V1+V2+V3+V4, subset=train)
  pred[i]<-predict(LD,data.frame(iris[i,1:4]))$class
}
#por alguna razon ahora me lo transformo en numeros
clase<-c(rep(1,50),rep(2,50),rep(3,50))
mean(pred!=clase) 

#cuadrática

QD<-qda(V5~V1+V2+V3+V4)
pred<-predict(QD,iris[,1:4])
table(pred$class,iris[,5])
aparente<-mean(iris[,5]!=pred$class)

##### Ahora haciendo todas las cuentas vistas en teoría
# para ver bien como funciona

X1<-iris[1:50,1:4]#setosa
X2<-iris[51:100,1:4]#versicolor
X3<-iris[101:150,1:4]#virginica

n1<-nrow(X1)
n2<-nrow(X2)
n3<-nrow(X3)

s1<-cov(X1)
s2<-cov(X2)
s3<-cov(X3)

q1<-s1*(n1-1)
q2<-s2*(n2-1)
q3<-s3*(n3-1)

q<-q1+q2+q3
n<-n1+n2+n3

xraya1<-apply(X1,2,mean)
xraya2<-apply(X2,2,mean)
xraya3<-apply(X3,2,mean)

U<-q1+q2+q3
S<-U/(n-3)

#hago la clasificacion armando la formula lineal
clasificacion_lineal<-function(datos)
{
  t<-c()
  for(i in 1:nrow(datos))
  {
    x<-datos[i,]
    d12<-t(xraya1-xraya2)%*%solve(S)%*%(x-(xraya1+xraya2)/2)
    d13<-t(xraya1-xraya3)%*%solve(S)%*%(x-(xraya1+xraya3)/2)
    d23<-t(xraya2-xraya3)%*%solve(S)%*%(x-(xraya3+xraya2)/2)
    d21<--d12
    d31<--d13
    d32<--d23
    if(d12>0 && d13>0)
    {
      t[i]<-"Iris-setosa"
    }
    else 
    {
      if(d21>0 && d23>0)
      {
        t[i]<-"Iris-versicolor"
      }
      else
      {
        t[i]<-"Iris-virginica"
      }
    }
  }
  return(t)
}

datos<-rbind(X1,X2,X3)
pred<-clasificacion_lineal(as.matrix(datos))
aparente<-mean(iris[,5]!=pred)

#ahora usando CV

pred<-c()
for(i in 1:150)
{
  datos<-iris[-i,]
  X1<-datos[c(datos[,5]=="Iris-setosa"),1:4]
  X2<-datos[c(datos[,5]=="Iris-versicolor"),1:4]
  X3<-datos[c(datos[,5]=="Iris-virginica"),1:4]
  n1<-nrow(X1)
  n2<-nrow(X2)
  n3<-nrow(X3)
  s1<-cov(X1)
  s2<-cov(X2)
  s3<-cov(X3)
  n<-n1+n2+n3
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  xraya3<-apply(X3,2,mean)
  S<-(s1*(n1-1)+s2*(n2-1)+s3*(n3-1))/(n-3)
  x<-iris[i,1:4]
  pred[i]<-clasificacion_lineal((as.matrix(x)))
}

mean(pred!=iris[,5])


#ahora por cuadratica

clasificacion_cuadratica<-function(datos)
{
  pred<-c()
  for(i in 1:nrow(datos))
  {
    x<-datos[i,]
    d12<-log(det(s2)/det(s1))-(t(xraya1)%*%solve(s1)%*%xraya1-t(xraya2)%*%solve(s2)%*%xraya2)-t(x)%*%(solve(s1)-solve(s2))%*%x+2*t(x)%*%(solve(s1)%*%xraya1-solve(s2)%*%xraya2)
    d13<-log(det(s3)/det(s1))-(t(xraya1)%*%solve(s1)%*%xraya1-t(xraya3)%*%solve(s3)%*%xraya3)-t(x)%*%(solve(s1)-solve(s3))%*%x+2*t(x)%*%(solve(s1)%*%xraya1-solve(s3)%*%xraya3)
    d23<-log(det(s3)/det(s2))-(t(xraya2)%*%solve(s2)%*%xraya2-t(xraya3)%*%solve(s3)%*%xraya3)-t(x)%*%(solve(s2)-solve(s3))%*%x+2*t(x)%*%(solve(s2)%*%xraya2-solve(s3)%*%xraya3)
    d21<--d12

    if(d12>0 && d13>0)
    {
      pred[i]<-"Iris-setosa"
    }
    else 
    {
      if(d21>0 && d23>0)
      {
        pred[i]<-"Iris-versicolor"
      }
      else
      {
        pred[i]<-"Iris-virginica"
      }
      
    }
  }
  return(pred)
}

datos<-as.matrix(iris[,1:4])
pred<-clasificacion_cuadratica(datos)
aparente<-mean(iris[,5]!=pred) #otra vez lo mismo

#X2: Ancho del pétalo
#X4: Ancho del cépalo
#Me quedo solo con esas dos

iris<-read.table("iris.data", header = FALSE,sep=",")
X1<-iris[1:50,c(2,4)]#setosa
X2<-iris[51:100,c(2,4)]#versicolor
X3<-iris[101:150,c(2,4)]#virginica

plot(X1,col="red", xlim=c(2,5),ylim=c(0,3))
points(X2,col="blue")
points(X3,col="green")

n1<-nrow(X1)
n2<-nrow(X2)
n3<-nrow(X3)

s1<-cov(X1)
s2<-cov(X2)
s3<-cov(X3)

q1<-s1*(n1-1)
q2<-s2*(n2-1)
q3<-s3*(n3-1)

q<-q1+q2+q3
n<-n1+n2+n3

xraya1<-apply(X1,2,mean)
xraya2<-apply(X2,2,mean)
xraya3<-apply(X3,2,mean)
U<-q1+q2+q3
xraya<-(xraya1*n1+xraya2*n2+xraya3*n3)/n

#aparece una nueva observacion, la quiero clasificar
x<-c(3.5,1.75)
points(x,col="chocolate",pch=12)

clasificacion_cuadratica(t(as.matrix(x)))

#con R
library(MASS)
attach(iris)
QD<-qda(V5~V2+V4)
predecir<-predict(QD, newdata = data.frame(V2=x[1],V4=x[2])) 
predecir
#lo clasifica igual que mi funcion :)


####################################################################################################
#COMO DIBUJO LAS REGIONES EN MI PLOT???
# draw discrimination line
np <- 300
nd.x <- seq(from = min(iris[,2]), to = max(iris[,2]), length.out = np)
nd.y <- seq(from = min(iris[,4]), to = max(iris[,4]), length.out = np)
nd <- expand.grid(x = nd.x, y = nd.y)

QD<-qda(V5~V2+V4)

#para toda la grilla anterior predice la data
prd <- as.numeric(predict(QD, newdata=data.frame(V2=nd[,1],V4=nd[,2]))$class)
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), add = TRUE, drawlabels = FALSE)
####################################################################################################

clasificacion_lineal(t(as.matrix(x)))

#con R
LD<-lda(V5~V2+V4)
predecir<-predict(LD, newdata = data.frame(V2=x[1],V4=x[2])) #lo clasifica igual que mi funcion :)
predecir
prd2 <- as.numeric(predict(LD, newdata=data.frame(V2=nd[,1],V4=nd[,2]))$class)
contour(x = nd.x, y = nd.y, z = matrix(prd2, nrow = np, ncol = np),col="red", add = TRUE, drawlabels = FALSE)


#otra forma para grafico de las reglas lineales, salen de despejar de la regla de 
#clasificación de bayes para vectores X en R2

v<-t(xraya1-xraya2)%*%solve(S)
c<-t(xraya1-xraya2)%*%solve(S)%*%((xraya1+xraya2)/2)
abline(c/v[2],v[1]/(-v[2]),col="blue")

v<-t(xraya2-xraya3)%*%solve(S)
c<-t(xraya2-xraya3)%*%solve(S)%*%((xraya2+xraya3)/2)
abline(c/v[2],v[1]/(-v[2]),col="blue")


#para hacer el grafico con el paquete klaR

library(klaR)
help("partimat")
partimat(V5~V2+V4,method="lda")
partimat(V5~V2+V4,method="qda")



#4
finanza<-read.table("finanzas.txt", header = TRUE)
finanzas<-finanza[,-1]
par(mfrow=c(2,2))
attach(finanzas)
plot(x1,x2,col=Grupo)
plot(x1,x3,col=Grupo)
plot(x1,x4,col=Grupo) #estan re mezcladas!
par(mfrow=c(1,1))

#no parecen normales las dos primeras, por ahi si la tercera. 
#Graciela:la normalidad no se ve ahi, solo se puede decir
#si estan mas o no correlacionadas
#para ver si son o no normales hay que hacer graficos de cuantiles
# o algun test de normal bivariada

# Se puede hacer un hacer shapiro mv

X1<-finanzas[1:21,1:2]
X2<-finanzas[22:46,1:2]

xraya1<-apply(X1,2,mean)
xraya2<-apply(X2,2,mean)

s1<-cov(X1)
s2<-cov(X2)

#armo la regla de clasificacion cuadratica con prob iguales

clasificacion_cuadratica<-function(datos)
{
  pred<-c()
  for(i in 1:nrow(datos))
  {
    x<-datos[i,]
    d12<-log(det(s2)/det(s1))-(t(xraya1)%*%solve(s1)%*%xraya1-t(xraya2)%*%solve(s2)%*%xraya2)-t(x)%*%(solve(s1)-solve(s2))%*%x+2*t(x)%*%(solve(s1)%*%xraya1-solve(s2)%*%xraya2)
      if(d12>0)
    {
      pred[i]<-1
    }
    else 
    {
      pred[i]<-2
    }
  }
  return(pred)
}
pred<-clasificacion_cuadratica(as.matrix(finanzas[,1:2]))

e_app<-mean(pred!=finanzas[,5])

#GRAFICARLO
plot(x1,x2,col=Grupo)
finanzas
QD<-qda(Grupo~x1+x2)
####################################################################################################
#COMO DIBUJO LAS REGIONES EN MI PLOT???
# limite lineal
np <- 50
nd.x <- seq(from = min(finanzas[,1]), to = max(finanzas[,1]), length.out = np)
nd.y <- seq(from = min(finanzas[,2]), to = max(finanzas[,2]), length.out = np)
nd <- expand.grid(x = nd.x, y = nd.y)

d12<-c()
for(i in 1:nrow(nd)){
  x<-as.matrix(nd[i,])
  d12[i]<-log(det(s2)/det(s1))-(t(xraya1)%*%solve(s1)%*%xraya1-t(xraya2)%*%solve(s2)%*%xraya2)-(x)%*%(solve(s1)-solve(s2))%*%t(x)+2*(x)%*%(solve(s1)%*%xraya1-solve(s2)%*%xraya2)
}

#para toda la grilla anterior predice la data
contour(x = nd.x, y = nd.y, z = matrix(d12, nrow = np, ncol = np), levels=0,col = "red",add = TRUE)
####################################################################################################

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),1:2]
  X2<-datos[c(datos[,5]==2),1:2]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,1:2]
  pred[i]<-clasificacion_cuadratica((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

#cambiando las probabilidades a priori
clasificacion_cuadratica2<-function(datos)
{
  pred<-c()
  for(i in 1:nrow(datos))
  {
    x<-datos[i,]
    d12<-log(det(s2)/det(s1))-(t(xraya1)%*%solve(s1)%*%xraya1-t(xraya2)%*%solve(s2)%*%xraya2)-t(x)%*%(solve(s1)-solve(s2))%*%x+2*t(x)%*%(solve(s1)%*%xraya1-solve(s2)%*%xraya2)
    if(d12>2.944)
    {
      pred[i]<-1
    }
    else 
    {
      pred[i]<-2
    }
  }
  return(pred)
}
pred<-clasificacion_cuadratica2(as.matrix(finanzas[,1:2]))

e_app<-mean(pred!=finanzas[,5])

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),1:2]
  X2<-datos[c(datos[,5]==2),1:2]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,1:2]
  pred[i]<-clasificacion_cuadratica2((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

#superpongo en el grafico la regla nueva y veo que pasa
contour(x = nd.x, y = nd.y, z = matrix(d12, nrow = np, ncol = np), levels=2.9,col = "blue",add = TRUE)
####################################################################################################


#ahora lineal. Uso funciones de R
LD<-lda(Grupo~x1+x2)
pred<-predict(LD,finanzas[,1:2])
aparente1<-mean(Grupo!=pred$class) 


#######################
#cuad con x1 y x3
X1<-finanzas[1:21,c(1,3)]
X2<-finanzas[22:46,c(1,3)]
xraya1<-apply(X1,2,mean)
xraya2<-apply(X2,2,mean)
s1<-cov(X1)
s2<-cov(X2)
#con la regla de clasificacion cuadratica con prob iguales

pred<-clasificacion_cuadratica(as.matrix(finanzas[,c(1,3)]))
e_app<-mean(pred!=finanzas[,5])

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),c(1,3)]
  X2<-datos[c(datos[,5]==2),c(1,3)]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,c(1,3)]
  pred[i]<-clasificacion_cuadratica((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

#cambiando las probabilidades a priori

pred<-clasificacion_cuadratica2(as.matrix(finanzas[,c(1,3)]))
e_app<-mean(pred!=finanzas[,5])

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),c(1,3)]
  X2<-datos[c(datos[,5]==2),c(1,3)]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,c(1,3)]
  pred[i]<-clasificacion_cuadratica2((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

###########
#con x1 y x4
X1<-finanzas[1:21,c(1,4)]
X2<-finanzas[22:46,c(1,4)]
xraya1<-apply(X1,2,mean)
xraya2<-apply(X2,2,mean)
s1<-cov(X1)
s2<-cov(X2)
#con la regla de clasificacion cuadratica con prob iguales

pred<-clasificacion_cuadratica(as.matrix(finanzas[,c(1,4)]))
e_app<-mean(pred!=finanzas[,5])

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),c(1,3)]
  X2<-datos[c(datos[,5]==2),c(1,3)]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,c(1,4)]
  pred[i]<-clasificacion_cuadratica((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

#cambiando las probabilidades a priori

pred<-clasificacion_cuadratica2(as.matrix(finanzas[,c(1,4)]))
e_app<-mean(pred!=finanzas[,5])

#usando cv
pred<-c()
for(i in 1:46)
{
  datos<-finanzas[-i,]
  X1<-datos[c(datos[,5]==1),c(1,3)]
  X2<-datos[c(datos[,5]==2),c(1,3)]
  n1<-nrow(X1)
  n2<-nrow(X2)
  s1<-cov(X1)
  s2<-cov(X2)
  xraya1<-apply(X1,2,mean)
  xraya2<-apply(X2,2,mean)
  x<-finanzas[i,c(1,4)]
  pred[i]<-clasificacion_cuadratica2((as.matrix(x)))
}

e_cv<-mean(pred!=finanzas[,5])

#######
#por ultimo: usando las 4 variables
