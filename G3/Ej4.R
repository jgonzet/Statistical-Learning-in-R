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


datos<-Default
X<-datos$balance
Y<-datos$default

levels(Y)
levels(Y)<-c(1,2)
clasificador<-univariateLDA(X,Y,2)
clasificador(10000)  


