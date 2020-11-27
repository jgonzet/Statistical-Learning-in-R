setwd("~/Documents/R/Datos")
library(MASS)

abalones<-read.table("abalone.txt",sep=",")
colnames(abalones)<-c("edad","longitud","diametro","altura","peso","carne","viscera","caparazon","anillos")

#Junto F y M en una sola categoria A(adulto):
levels(abalones$edad) #me fijo como estan ordenadas
levels(abalones$edad)<-c("A","I","A")

#separo los datos en training y testing:
porcentaje<-0.75
largo<-dim(abalones)[1]
largo_tr<-round(largo*porcentaje)
largo_te<-largo-largo_tr

training<-abalones[1:largo_tr,]
testing<-abalones[(largo_tr+1):largo,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

lda_completo<-qda(edad~.,data=training)

predichos<-predict(lda_completo,newdata=testing,type="response")
confusion<-table(predichos$class,testing[,1])
accuracy<-(confusion[1,1]+confusion[2,2])/largo_te
accuracy

