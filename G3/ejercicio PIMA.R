library(ISLR)
library(glmnet)
library(ggplot2)
library(glmnet)
library(faraway)
library(MASS)
library(GGally)

datos<-Pima.tr

#a
ggpairs(datos)

#parecería que npreg y glu discriminan mas el tipo, por ahí age...

#b
ajuste<-glm(type~., data=datos, family=binomial)
summary(ajuste)

#c intervalo de confianza...
IC<-matrix(0,ncol=2,nrow=8)

for(i in 1:8)
{
  IC[i,]<-c(summary(ajuste)$coef[i,1]-qnorm(0.975)*summary(ajuste)$coef[i,2],
            summary(ajuste)$coef[i,1]+qnorm(0.975)*summary(ajuste)$coef[i,2]) 
}
colnames(IC)<-c("LI","LS")
rownames(IC)<-c("beta 0", "beta 1", "beta 2", "beta 3",
                "beta 4", "beta 5", "beta 6", "beta 7")

IC

#d

prediccion<-predict(ajuste, type="response")

predichos=rep("No",length(prediccion))
predichos[prediccion>0.5]="Yes"
confusion<-table(predichos,datos$type)
confusion

aciertos<-(confusion[1,1]+confusion[2,2])/length(predichos)

mean(predichos==datos$type)

#e
test<-Pima.te

prediccion<-predict(ajuste,newdata=test, type="response")
predichos=rep("No",length(prediccion))
predichos[prediccion>0.5]="Yes"

observados<-test$type

confusion<-table(predichos,observados)
confusion

aciertos<-(confusion[1,1]+confusion[2,2])/length(predichos)
aciertos*100


mean(predichos==test$type)

#f

nuevadata<-c( 2 ,100,  70, 20, 26,  0.24 , 30)
ND<-rbind(test[1:7],nuevadata)[333,]
probabilidad_diabetes<-predict(ajuste,newdata=ND, type="response")

#g
sigma_beta_sombrero<-summary(ajuste)$coef[,2]

#hago la cuenta a ver si da lo mismo
X<-model.matrix(ajuste)
pi<-predict(ajuste,type="response")
n<-nrow(datos)
W_sombrero<-diag(pi*(1-pi),ncol=n,nrow=n)

W<-diag(ajuste$weights)#es lo mismo que con mi cuenta

sigma_betas<-sqrt(diag(solve(t(X)%*%W%*%X)))
summary(ajuste)
odd<-exp(salida(ajuste)$coef[3,1]*10)
ic<-c(exp(summary(ajuste)$coef[3,1]-qnorm(0.975)*exp(summary(ajuste)$coef[3,2])),
      exp(summary(ajuste)$coef[3,1]+qnorm(0.975)*exp(summary(ajuste)$coef[3,2])))

#h
intervalo_probabilidad<-function(salida,alfa,x0) #importante respetar el orden en x0
{
  d<-as.matrix(c(1,x0))
  psi<-t(d)%*%(summary(salida)$coef[,1])
  X<-model.matrix(salida)
  W<-diag(salida$weights)
  var<-solve(t(X)%*%W%*%X)
  e<-qnorm(1-alfa/2)*sqrt(t(d)%*%var%*%(d))
  ic<-c(1/(1+exp(-psi+e)),1/(1+exp(-psi-e)))
  return(ic)
}

#i
intervalo_probabilidad(ajuste,0.05,nuevadata)


#OTRA FORMA
# intervalo de confianza usando el método delta

intervalo_probabilidad_delta<-function(salida,alfa,x0, pi)
{
  d<-as.matrix(c(1,x0))
  psi<-t(d)%*%(summary(salida)$coef[,1])
  X<-model.matrix(salida)
  W<-diag(salida$weights)
  var<-solve(t(X)%*%W%*%X)
  e<-qnorm(1-alfa/2)*sqrt(t(d)%*%var%*%(d))*pi*(1-pi)
  ic<-c(pi-e,pi+e)
  return(ic)
}

intervalo_probabilidad_delta(ajuste,0.05,nuevadata,probabilidad_diabetes)

#Con un paquete

confint(ajuste) #da los intervalos para los betas

preds <- predict(ajuste, newdata=ND, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- ajuste$family$linkinv(fit)
upr2 <- ajuste$family$linkinv(upr)
lwr2 <- ajuste$family$linkinv(lwr)


