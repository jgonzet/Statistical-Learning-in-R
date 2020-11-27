library(ISLR)
library(glmnet)
library(ggplot2)
library(glmnet)
library(faraway)
library(MASS)
library(GGally)

datos<-Pima.tr
#a) analisis preliminar: correlacion entre variables
ggpairs(datos)

#b) ajuste de regresion logistica:
reg<-glm(type~.,data=datos,family=binomial)
summary(reg)

#c intervalo de confianza para los estimadores de Betas:
IC<-matrix(0,ncol=3,nrow=8)

for(i in 1:8)
{
  IC[i,]<-c(summary(reg)$coef[i,1]-qnorm(0.975)*summary(reg)$coef[i,2],summary(reg)$coef[i,1],
            summary(reg)$coef[i,1]+qnorm(0.975)*summary(reg)$coef[i,2]) 
}

colnames(IC)<-c("LI","beta","LS")
rownames(IC)<-c("beta 0", "beta 1", "beta 2", "beta 3",
                "beta 4", "beta 5", "beta 6", "beta 7")

IC

#d) Tabla de confucius:
prediccion<-predict(reg, type="response")
predichos = rep("No",length(prediccion))
predichos[prediccion>0.5]="Yes"
confusion<-table(predichos,datos$type)
confusion

aciertos<-(confusion[1,1]+confusion[2,2])/length(predichos)

mean(predichos==datos$type)

#e: tabla de confucius para muestras de test: lo voy a hacer manualmente.
test<-Pima.te

#prediccion<-predict(reg,newdata=test, type="response")

exponente<-reg$coefficients[1]+reg$coefficients[2:8]%*%t(test[,1:7])
prediction<-1/(1+exp(-exponente))
treshold<-0.5
predichos = rep("No",length(prediction))
predichos[prediction>treshold]="Yes"

observados<-test$type

confusion<-table(predichos,observados)
confusion

aciertos<-100*(confusion[1,1]+confusion[2,2])/length(predichos)
aciertos
mean(predichos==test$type)

#f: predecimos a Poppers
x<-c(2,100,70,20,26,0.24,30)
expo<-reg$coefficients%*%x
y_pred<-1/(1+exp(-expo))

#si lo quiero hacer con predict, necesito crear una newdata con formato compatible:
xx<-data.frame(2,100,70,20,26,0.24,30,"Yes")
colnames(xx)<-c("npreg","glu","bp","skin","bmi","ped","age")

yy_pred<-predict(reg,newdata=xx,type="response")



#g
