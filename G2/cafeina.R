setwd("~/Documents/AE/R/Guia 2")
library(readxl)
cafe<-read_excel("cafeina.xls")
n<-nrow(cafe)

X<-cbind(c(rep(1,10),rep(0,20)),c(rep(0,10),rep(1,10),rep(0,10)),c(rep(0,20),rep(1,10)))
p<-ncol(X)

C<-rbind(c(1,-1,0),c(0,1,-1))
q<-nrow(C)

beta_s<-c(mean(cafe$y[1:10]),mean(cafe$y[11:20]),mean(cafe$y[21:30]))
s_2<-t(cafe$y-X%*%beta_s)%*%(cafe$y-X%*%beta_s)/(n-p)

f<-t(C%*%beta_s)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%(C%*%beta_s)/q/s_2
v<-qf(0.95,q,n-p) #para comparar con f
pv<-100*(1-pf(f,q,n-p)) #p valor
pv


