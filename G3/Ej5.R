library(ISLR)
library(MASS)
univariateLDA<-function(X,Y,k)
{
  #ajuste_lda<-lda(Y~X)
  u<-c()
  var<-c()
  priori<-c()
  for(i in 1:k){
    u[i]= mean(X[Y==i])
    var[i]=sum((X[Y==i]-u[i])^2)/(length(X[Y==i])-k)
    priori[i]=length(X[Y==i])/length(X)
  }
  
  predecir<-function(dato){
    likelihood<-c()
    for(i in 1:k){
      likelihood[i]<-dato*u[i]/var[i]-u[i]^2/(2*var[i])+log(priori[i])
    }
    return(which.max(likelihood))
  }
}

z<-rnorm(1000,1,2)
y<-rep(1,length(z))
y[z<0]=0
x=0.5+5*z+rnorm(1000)

reg<-glm(y~x,family="binomial")




Y<-as.factor(y[1:700]-1)
X<-as.numeric(x[1:700])


levels(Y)<-c('A','B')


ajuste_rl<-glm(Y~X,family='binomial')






summary(ajuste_rl)
predichos<-predict(ajuste_rl,newdata=as.numeric(x[701:1000]),type="response")

predictor<-univariateLDA(x[1:700],y[1:700],2)
predichos<-c()
for(i in 1:300){
  predichos[i]<-predictor(x[(700+i)])}

confusion_LDA<-table(predichos,y[701:1000])

