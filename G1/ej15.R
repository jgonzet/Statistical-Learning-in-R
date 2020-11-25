library(ggplot2)

n<-100
x1<-runif(n,min=0,max=1)
x2<-runif(n,min=0,max=1)
epsilon<-rnorm(n)
y<-2+2*x1+0.3*x2+epsilon

ajuste<-lm(y~x1+x2)
beta_sombrero<-ajuste$coefficients

y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*x1+beta_sombrero[3]*x2

plot(x1,x2)
cor(x1,x2)

#=================================================================
#b)

n<-100
x1<-runif(n,min=0,max=1)
x2<-0.5*x1+rnorm(100)/10

epsilon<-rnorm(n)

y<-2+2*x1+0.3*x2+epsilon

ajuste<-lm(y~x1+x2)
beta_sombrero<-ajuste$coefficients

y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*x1+beta_sombrero[3]*x2

plot(x1,x2)
cor(x1,x2)

