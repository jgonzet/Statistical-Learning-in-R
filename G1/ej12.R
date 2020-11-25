#setwd("~/Documents/AE/R/Guia 1")
library(ggplot2)

resolver<-function(x,y){
  X<-matrix(c(rep(1,50),x),byrow = FALSE,ncol=2)
  solve(t(X)%*%X)%*%t(X)%*%y
}

girasoles<-read.table("girasol.txt",sep=" ")
#summary(girasoles)

inversion <- girasoles$V1
rinde <- girasoles$V2

beta_sombrero<-resolver(inversion,rinde)
y_sombrero<-beta_sombrero[1]+beta_sombrero[2]*inversion

ggplot(data=girasoles)+
  geom_point(aes(x=inversion, y=rinde))+
  geom_line(aes(x=inversion,y=y_sombrero),col="Firebrick")+
  labs(title="",
       x = "Inversion", 
       y = "Rinde")+
  theme_light()

#al haber tantos outliers, la recta me queda muy abajo de lo que deberia.