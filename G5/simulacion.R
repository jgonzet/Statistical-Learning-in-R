#simulacion de datos casi colineales
set.seed(456978)
B<-1000
betaest<-matrix(rep(0,5*B),nrow = B,ncol = 5)
significativos<-matrix(rep(0,5*B),nrow = B,ncol = 5)
ECpred<-rep(0,B)

betaest2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
significativos2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
ECpred2<-rep(0,B)

#Es decir, la respuesta Y depende de dos covariables independientes, 
#X1, X2 y las otras dos X3, X4 son casi colineales con X1. 
#Repetimos el experimento B = 1000 veces. 
#Cada vez ajustamos los dos modelos lineales por LS

for(j in 1:B){
  x1<-runif(100,0,5)
  x2<-runif(100,0,5)
  x3<-2*x1+0.1*runif(100,min=-1,max=1)
  x4<--x1+0.1*runif(100,min=-1,max=1)
  yy<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  ajus<-lm(yy ~ x1+x2+x3+x4)
  betaest[j,]<-coef(ajus)
  significativos[j,]<-summary(ajus)$coefficients[,4] #guarda los pvalores del ajuste lineal
  ajus2<-lm(yy ~ x1+x2)
  betaest2[j,]<-coef(ajus2)
  significativos2[j,]<-summary(ajus2)$coefficients[,4]
  
  #prediccion
  
  x1n<-runif(100,0,5)
  x2n<-runif(100,0,5)
  x3n<-2*x1+0.1*runif(100,min=-1,max=1)
  x4n<--x1+0.1*runif(100,min=-1,max=1)
  yyn<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  new <- data.frame(x1 =x1n,x2 =x2n,x3 =x3n,x4 =x4n)
  new2 <- data.frame(x1 =x1n,x2 =x2n)
  yp1<-predict(ajus, new)
  ECpred[j]<-sum((yyn-yp1)^2)/length(yyn)
  yp2<-predict(ajus2, new2)
  ECpred2[j]<-sum((yyn-yp2)^2)/length(yyn)
  
  
  }

#ahora le calculo las varianzas a los estimadores

apply(betaest,2,sd)
apply(betaest2,2,sd)


#Para cada una de las repeticiones, contamos cu ́ales de los coeficientes
#estimados
#resulta significativamente distinto de cero, a nivel 0.05. Obtuvimos

cuales<-rep(0,5)
for(i in 1:5){
cuales[i]<-sum(significativos[,i]<0.05)
}
cuales
cuales2<-rep(0,3)
for(i in 1:3){
  cuales2[i]<-sum(significativos2[,i]<0.05)
}
cuales2



#Otra manera de evaluar la simulacio ́n es calculando el error
#cuadr ́atico medio de predicci ́on en un
#a muestra de testeo independiente de la utilizada en el ajuste.

aa<-range(c(density(ECpred)$y,density(ECpred2)$y))

plot(density(ECpred),ylim=c(0,aa[2]), xlab="", lwd=2, 
     main="Histograma suavizado de los EMC de testeo")
points(density(ECpred2),col="red",type="l",lwd=2)
legend("topright",c("modelo 4 covar","modelo 2 covar"),
       col=c("black", "red"),lwd=2)

boxplot(ECpred,ECpred2,names=c("4 covar","2 covar")
        , main="Error cuadratico medio en 
        muestra de testeo")

cov(cbind(x1,x2,x3,x4))
cor(cbind(x1,x2,x3,x4))

par(mfrow=c(1,2))
boxplot(betaest[,2],betaest2[,2],names=c("OLS 4 covar","OLS 2 covar)                                       2 covar")
        , main=expression(paste("Boxplot de ",hat(beta[1]))))
abline(h=6,col="red",lty=2)

boxplot(betaest[,3],betaest2[,3],names=c("OLS 4 covar","OLS 2 covar")
        , main=expression(paste("Boxplot de ",hat(beta[2]))))
abline(h=-5,col="red",lty=2)


ajus2<-lm(yy ~ x1+x2)
summary(ajus2)

######################################################################
## simulacion con estimadores ridge
######################################################################

library(glmnet)
set.seed(456978)
B<-1000
betaest<-matrix(rep(0,5*B),nrow = B,ncol = 5)
significativos<-matrix(rep(0,5*B),nrow = B,ncol = 5)
ECpred<-rep(0,B)

betaest2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
significativos2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
ECpred2<-rep(0,B)

betaest.rid<-matrix(rep(0,5*B),nrow = B,ncol = 5)
ECpred.rid<-rep(0,B)
for(j in 1:B){
  x1<-runif(100,0,5)
  x2<-runif(100,0,5)
  x3<-2*x1+0.1*runif(100,min=-1,max=1)
  x4<--x1+0.1*runif(100,min=-1,max=1)
  yy<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  ajus<-lm(yy ~ x1+x2+x3+x4)
  betaest[j,]<-coef(ajus)
  significativos[j,]<-summary(ajus)$coefficients[,4] #guarda los pvalores del ajuste lineal
  ajus2<-lm(yy ~ x1+x2)
  betaest2[j,]<-coef(ajus2)
  significativos2[j,]<-summary(ajus2)$coefficients[,4]

  xx<-model.matrix(ajus)
  #ajuste.ridge = glmnet(xx,yy,alpha=0)  
  #rid.crossval<-cv.glmnet(xx, yy,nfolds = 10, alpha=0)
  rid.lam.1.1<-glmnet(xx, yy,alpha=0,lambda = 1.1)  #se parece al rid.crossval$lambda.1se
  betaest.rid[j,]<-coef(rid.lam.1.1)[c(1,3:6),1]
  
  #prediccion
  
  x1n<-runif(100,0,5)
  x2n<-runif(100,0,5)
  x3n<-2*x1+0.1*runif(100,min=-1,max=1)
  x4n<--x1+0.1*runif(100,min=-1,max=1)
  yyn<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  new <- data.frame(x1 =x1n,x2 =x2n,x3 =x3n,x4 =x4n)
  new2 <- data.frame(x1 =x1n,x2 =x2n)
  yp1<-predict(ajus, new)
  ECpred[j]<-sum((yyn-yp1)^2)/length(yyn)
  yp2<-predict(ajus2, new2)
  ECpred2[j]<-sum((yyn-yp2)^2)/length(yyn)
  
  ypridge<-predict(rid.lam.1.1, as.matrix(cbind(rep(1,100),new)))
  ECpred.rid[j]<-sum((yyn-ypridge)^2)/length(yyn)
}

#Resultados
xtable(cbind(0:4,c(apply(betaest,2,sd)),c(apply(betaest2,2,sd),0,0),c(apply(betaest.rid,2,sd))))


boxplot(betaest[,2],betaest2[,2],betaest.rid[,2], 
names=c("OLS 4 covar","OLS 2 covar","ridge 4 cov")
        , main=expression(paste("Boxplot de ",hat(beta[1]))))
abline(h=6,col="red",lty=2)

boxplot(betaest[,3],betaest2[,3],betaest.rid[,3],
        names=c("OLS 4 covar","OLS 2 covar","ridge 4 cov")
        , main=expression(paste("Boxplot de ",hat(beta[2]))))
abline(h=-5,col="red",lty=2)

boxplot(betaest[,4],betaest.rid[,4],
        names=c("OLS 4 covar","ridge 4 cov")
        , main=expression(paste("Boxplot de ",hat(beta[3]))))
abline(h=1,col="red",lty=2)

boxplot(betaest[,5],betaest.rid[,5],
        names=c("OLS 4 covar","ridge 4 cov")
        , main=expression(paste("Boxplot de ",hat(beta[4]))))
abline(h=4,col="red",lty=2)

boxplot((betaest[,2]+2*betaest[,4]-betaest[,5]),
        (betaest.rid[,2]+2*betaest.rid[,4]-betaest.rid[,5]),
        names=c("OLS 4 covar","ridge 4 cov")
        , main=expression(paste("Boxplot de ",hat(beta[1])," + 2",hat(beta[4])," - ",hat(beta[5]))))
abline(h=6,col="red",lty=2)

sd((betaest[,2]+2*betaest[,4]-betaest[,5]))
sd(betaest.rid[,2]+2*betaest.rid[,4]-betaest.rid[,5])


par(mfrow=c(1,1))
aa<-range(c(density(ECpred)$y,density(ECpred2)$y),density(ECpred.rid)$y)

plot(density(ECpred),ylim=c(0,aa[2]), xlab="", lwd=2, 
     main="Histograma suavizado de los EMC de testeo")
points(density(ECpred2),col="red",type="l",lwd=2)
points(density(ECpred.rid),col="blue",type="l",lwd=2)
legend("topright",c("OLS modelo 4 covar","OLS modelo 2 covar","ridge 4 covar"),
       col=c("black", "red","blue"),lwd=2)

boxplot(ECpred,ECpred2,ECpred.rid, names=c("OLS 4 covar","OLS 2 covar","ridge 4 covar")
        , main="Error cuadratico medio en 
        muestra de testeo")




##########################################################33
as.matrix(new)

XX<-model.matrix(ajus)
ajuste.ridge = glmnet(XX,yy,alpha=0)  
rid.crossval<-cv.glmnet(XX, yy,nfolds = 10, alpha=0)
rid.1se<-glmnet(XX, yy,alpha=0,lambda = rid.crossval$lambda.1se)
coef(rid.1se)
coef(ajus)

#el lasso selecciona bien!
las.crossval<-cv.glmnet(XX, yy,nfolds = 10, alpha=1)
las.1se<-glmnet(XX, yy,alpha=1,lambda = las.crossval$lambda.1se)
coef(las.1se)
coef(ajus)
plot(las.crossval)
las.min<-glmnet(XX, yy,alpha=1,lambda = las.crossval$lambda.min)
coef(las.min)


######################################################################
## simulacion con estimadores ridge y LASSO
######################################################################

library(glmnet)
set.seed(456978)
B<-1000
betaest<-matrix(rep(0,5*B),nrow = B,ncol = 5)
significativos<-matrix(rep(0,5*B),nrow = B,ncol = 5)
ECpred<-rep(0,B)

betaest2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
significativos2<-matrix(rep(0,3*B),nrow = B,ncol = 3)
ECpred2<-rep(0,B)

betaest.rid<-matrix(rep(0,5*B),nrow = B,ncol = 5)
betaest.las<-matrix(rep(0,5*B),nrow = B,ncol = 5)
ECpred.rid<-rep(0,B)
ECpred.las<-rep(0,B)
for(j in 1:B){
  x1<-runif(100,0,5)
  x2<-runif(100,0,5)
  x3<-2*x1+0.1*runif(100,min=-1,max=1)
  x4<--x1+0.1*runif(100,min=-1,max=1)
  yy<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  ajus<-lm(yy ~ x1+x2+x3+x4)
  betaest[j,]<-coef(ajus)
  significativos[j,]<-summary(ajus)$coefficients[,4] #guarda los pvalores del ajuste lineal
  ajus2<-lm(yy ~ x1+x2)
  betaest2[j,]<-coef(ajus2)
  significativos2[j,]<-summary(ajus2)$coefficients[,4]
  
  xx<-model.matrix(ajus)
  #ajuste.ridge = glmnet(xx,yy,alpha=0)  
  #rid.crossval<-cv.glmnet(xx, yy,nfolds = 10, alpha=0)
  rid.lam.1.1<-glmnet(xx, yy,alpha=0,lambda = 1.1)  #se parece al rid.crossval$lambda.1se
  betaest.rid[j,]<-coef(rid.lam.1.1)[c(1,3:6),1]
  
  lasso.lam.0.2<-glmnet(xx, yy,alpha=1,lambda = 0.201)  #se parece al las.crossval$lambda.1se
  betaest.las[j,]<-coef(lasso.lam.0.2)[c(1,3:6),1]
  
  
  #prediccion
  
  x1n<-runif(100,0,5)
  x2n<-runif(100,0,5)
  x3n<-2*x1+0.1*runif(100,min=-1,max=1)
  x4n<--x1+0.1*runif(100,min=-1,max=1)
  yyn<-8*x1-5*x2+x3+4*x4+5+rnorm(100,mean=0,sd=1)
  
  new <- data.frame(x1 =x1n,x2 =x2n,x3 =x3n,x4 =x4n)
  new2 <- data.frame(x1 =x1n,x2 =x2n)
  yp1<-predict(ajus, new)
  ECpred[j]<-sum((yyn-yp1)^2)/length(yyn)
  yp2<-predict(ajus2, new2)
  ECpred2[j]<-sum((yyn-yp2)^2)/length(yyn)
  
  ypridge<-predict(rid.lam.1.1, as.matrix(cbind(rep(1,100),new)))
  ECpred.rid[j]<-sum((yyn-ypridge)^2)/length(yyn)
  
  yplasso<-predict(lasso.lam.0.2, as.matrix(cbind(rep(1,100),new)))
  ECpred.las[j]<-sum((yyn-yplasso)^2)/length(yyn)
}

#Resultados
xtable(cbind(0:4,c(apply(betaest,2,sd)),c(apply(betaest2,2,sd),0,0),c(apply(betaest.rid,2,sd)),
       c(apply(betaest.las,2,sd))))


boxplot(betaest[,2],betaest2[,2],betaest.rid[,2],betaest.las[,2],
        names=c("OLS 4co","OLS 2co","ridge 4co","lasso 4co")
        , main=expression(paste("Boxplot de ",hat(beta[1]))))
abline(h=6,col="red",lty=2)

boxplot(betaest[,3],betaest2[,3],betaest.rid[,3],betaest.las[,3],
        names=c("OLS 4co","OLS 2co","ridge 4co","lasso 4co")
        , main=expression(paste("Boxplot de ",hat(beta[2]))))
abline(h=-5,col="red",lty=2)


boxplot(betaest[,4],betaest.rid[,4],betaest.las[,4],
        names=c("OLS 4co","ridge 4co","lasso 4co")
        , main=expression(paste("Boxplot de ",hat(beta[3]))))
abline(h=1,col="red",lty=2)

boxplot(betaest[,4],betaest.rid[,4],betaest.las[,4],
        names=c("OLS 4 covar","ridge 4 cov","lasso 4co")
        , main=expression(paste("Boxplot de ",hat(beta[3]))))
abline(h=1,col="red",lty=2)

boxplot(betaest[,5],betaest.rid[,5],betaest.las[,5],
        names=c("OLS 4 covar","ridge 4 cov","lasso 4co")
        , main=expression(paste("Boxplot de ",hat(beta[4]))))
abline(h=4,col="red",lty=2)


sd((betaest[,2]+2*betaest[,4]-betaest[,5]))
sd(betaest.rid[,2]+2*betaest.rid[,4]-betaest.rid[,5])


par(mfrow=c(1,1))
aa<-range(c(density(ECpred)$y,density(ECpred2)$y),density(ECpred.rid)$y,density(ECpred.las)$y)

plot(density(ECpred),ylim=c(0,aa[2]), xlab="", lwd=2, 
     main="Histograma suavizado de los EMC de testeo")
points(density(ECpred2),col="red",type="l",lwd=2)
points(density(ECpred.rid),col="blue",type="l",lwd=2)
points(density(ECpred.las),col="magenta",type="l",lwd=2)
legend("topright",c("OLS modelo 4 covar","OLS modelo 2 covar","ridge 4 covar","lasso 4 covar"),
       col=c("black", "red","blue","magenta"),lwd=6, bty="n")

boxplot(ECpred,ECpred2,ECpred.rid,ECpred.las, names=c("OLS 4co","OLS 2co","ridge 4 covar","lasso4")
        , main="Error cuadratico medio en 
        muestra de testeo")





