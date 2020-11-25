#x<-rnorm(100000)
#x<-rbinom(1000,10,0.5) #cuenta los exitos en 10 observaciones con prob.0.4 de exito
#x<-rchisq(1000,50) #suma de k=50 normales estandar al cuadrado
#x<-rf(1000,90,40) #F(size,dof 1,dof 2)
x<-rgamma(1000,0.7)#size,shape

mean(x)
var(x)

#realizar histograma, boxplot y Q-Qnorm de x
h<-hist(x,freq=F,labels=F)

#boxplot: whisker and box
bp<-boxplot(x,main="Normal Standar",range=0,outline=F,varwidth = T)

#QQnrom: quantile-quantile plot utilizando una dist. normal
qqnorm(x)
