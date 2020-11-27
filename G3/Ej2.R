#setwd("~/Documents/R/Datos")
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


# Ajusto los 4 modelos con los datos de entrenamiento:
reg1<-glm(edad~longitud,data=training,family='binomial')
reg2<-glm(edad~peso,data=training,family='binomial')
reg3<-glm(edad~anillos,data=training,family='binomial')
reg4<-glm(edad~longitud+peso+anillos,data=training,family='binomial')
reg5<-glm(edad~viscera+anillos+peso,data=training,family='binomial')

# Clasifico las muestras de prueba:
treshold = 0.5
observados<-testing$edad

prediccion1<-predict(reg1,newdata=testing, type="response")
prediccion2<-predict(reg2,newdata=testing, type="response")
prediccion3<-predict(reg3,newdata=testing, type="response")
prediccion4<-predict(reg4,newdata=testing, type="response")
prediccion5<-predict(reg5,newdata=testing, type="response")

# Segun el treshold, clasifico:
predichos1 = rep("A",largo_te)
predichos2 = rep("A",largo_te)
predichos3 = rep("A",largo_te)
predichos4 = rep("A",largo_te)
predichos5 = rep("A",largo_te)
predichos1[prediccion1>treshold]="I"
predichos2[prediccion2>treshold]="I"
predichos3[prediccion3>treshold]="I"
predichos4[prediccion4>treshold]="I"
predichos5[prediccion5>treshold]="I"


confusion1<-table(predichos1,observados)
confusion2<-table(predichos2,observados)
confusion3<-table(predichos3,observados)
confusion4<-table(predichos4,observados)
confusion5<-table(predichos5,observados)

aciertos1<-100*(confusion1[1,1]+confusion1[2,2])/largo_te
aciertos2<-100*(confusion2[1,1]+confusion2[2,2])/largo_te
aciertos3<-100*(confusion3[1,1]+confusion3[2,2])/largo_te
aciertos4<-100*(confusion4[1,1]+confusion4[2,2])/largo_te
aciertos5<-100*(confusion5[1,1]+confusion5[2,2])/largo_te

# PRECISION, RECALL, ACCURACCY
nrow<-4
ncol<-3
resumen<-matrix(0,nrow=nrow,ncol=ncol)

#precision:
resumen[1,1]<-confusion1[2,2]/(confusion1[2,2]+confusion1[1,2])
resumen[2,1]<-confusion2[2,2]/(confusion2[2,2]+confusion2[1,2])
resumen[3,1]<-confusion3[2,2]/(confusion3[2,2]+confusion3[1,2])
resumen[4,1]<-confusion4[2,2]/(confusion4[2,2]+confusion4[1,2])

#recall
resumen[1,2]<-confusion1[2,2]/(confusion1[2,2]+confusion1[2,1])
resumen[2,2]<-confusion2[2,2]/(confusion2[2,2]+confusion2[2,1])
resumen[3,2]<-confusion3[2,2]/(confusion3[2,2]+confusion3[2,1])
resumen[4,2]<-confusion4[2,2]/(confusion4[2,2]+confusion4[2,1])

#accy
resumen[1,3]<-(confusion1[1,1]+confusion1[2,2])/largo_te
resumen[2,3]<-(confusion2[1,1]+confusion2[2,2])/largo_te
resumen[3,3]<-(confusion3[1,1]+confusion3[2,2])/largo_te
resumen[4,3]<-(confusion4[1,1]+confusion4[2,2])/largo_te

colnames(resumen)<-c("Precision","Recall","Accuracy")
rownames(resumen)<-c("Longitud","Peso total","Anillos","Conjunta")

#resumen<-round(resumen,2)
resumen
  
