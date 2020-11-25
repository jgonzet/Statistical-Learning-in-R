library(ggplot2)

n=10000
x<-rnorm(n)
x2<-x^2
desvio=2
epsilon<-rnorm(n,sd=desvio)

y<-(-1)+0.5*x+epsilon
sd(y)


y_reg<-y-epsilon

ajuste1<-lm(y~x)
ajuste2<-lm(y~x+x2)


beta_sombrero1<-ajuste1$coefficients
y_sombrero1<-beta_sombrero1[1]+beta_sombrero1[2]*x

beta_sombrero2<-ajuste2$coefficients
y_sombrero2<-beta_sombrero2[1]+beta_sombrero2[2]*x+beta_sombrero2[3]*x2

    #otra forma de hacer la recta

  ggplot()+ 
    geom_point(aes(x=x,y=y))+
    geom_line(aes(x=x,y=y_sombrero1),col="green")+
    geom_line(aes(x=x,y=y_sombrero2),col="red")+
    geom_line(aes(x=x,y=y_sombrero2),col="yellow")+
    labs(title="Regresion lineal",
         x = "Longitud", 
         y = "Diametro")+
    theme_light()