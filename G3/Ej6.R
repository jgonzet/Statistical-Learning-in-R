setwd("~/Documents/R/Datos")

library(MASS)
library(GGally)



iris<-read.csv("iris.data",sep=",")
colnames(iris)

predictor_lda<-lda(class~.,data=iris)

x<-t(c(7.4,2.8,6.1,1.9))


colnames(x)<-colnames(iris)[1:4]
x<-as.data.frame(x)
predict(predictor_lda,newdata=x,type="response")
