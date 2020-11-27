#setwd("~/Documents/R/Datos")
abalones<-read.table("abalone.txt",sep=",")
colnames(abalones)<-c("edad","longitud","diametro","altura","peso","carne","viscera","caparazon","anillos")

#Junto F y M en una sola categoria A(adulto):
levels(abalones$edad) #me fijo como estan ordenadas
levels(abalones$edad)<-c("A","I","A")

#separo los datos en training y testing:
porcentaje<-0.7
largo<-dim(abalones)[1]
largo_tr<-round(largo*porcentaje)
largo_te<-largo-largo_tr

training<-abalones[1:largo_tr,]
testing<-abalones[(largo_tr+1):largo,]

ajuste<-glm(edad~longitud+peso+anillos,data=training,family='binomial')
prediction<-predict(ajuste,newdata=testing,family='binomial')

tresholds<-seq(0,1,0.0001)
TPR<-c()
FPR<-c()
distancia<-c()
i<-1
for(t in (tresholds)){
  predichos = rep("A",largo_te)
  predichos[prediction>t]="I"
  confusion<-table(predichos,testing$edad)
  TPR[i]<-confusion[2,2]/(confusion[2,2]+confusion[2,1])
  FPR[i]<-confusion[1,2]/(confusion[1,1]+confusion[1,2])
  distancia[i]<-min(sqrt((TPR[i]-1)^2+FPR[i]^2),sqrt(TPR[i]^2+(FPR[i]-1)^2))
  i<-i+1
}

plot(FPR,TPR)

pos<-which.min(distancia)
theta_optimo<-tresholds[pos]
points(FPR[pos],TPR[pos],pch=1,col="red")
predichos=rep("A",largo_te)
predichos[prediction>theta_optimo]="I"
confusion<-table(predichos,testing$edad)
confusion
FPR[pos]
TPR[pos]
