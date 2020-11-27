#####################################
#LDA
library(Flury)
library(MASS)

data("microtus")

#armo mi matriz de datos de interes
X<-cbind(microtus$M2Left,microtus$M3Left,microtus$Foramen, 
        microtus$Pbone,microtus$Length,microtus$Height, microtus$Rostrum)

#separo en las dos especies y en los Sin Identificar
MM<-X[1:43,]
MS<-X[44:89,]
SI<-X[90:288,]

y1<-rep(1,nrow(MM))
y2<-rep(2,nrow(MS))
y<-c(y1,y2)
x<-rbind(MM,MS) #pone una abajo de la otra

dim(x)
length(y)
L<-lda(y~x)
plot(L)

r<-t(L$scaling)%*%t(MM)
s<-t(L$scaling)%*%t(MS)
plot(r,rep(2,43),col="blue")
points(s,rep(2,46),col="red")
abline(v=(mean(r)+mean(s))/2, col="green", lwd=2)