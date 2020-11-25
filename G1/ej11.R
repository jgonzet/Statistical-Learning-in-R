resolver<-function(x,y){
  X<-matrix(c(rep(1,50),x),byrow = FALSE,ncol=2)
  solve(t(X)%*%X)%*%t(X)%*%y
}

#cargo los datos
datos<-cars

#defino las variables
x<-datos$speed #velocidad del auto
y<-datos$dist #distancia requerida de frenado

beta_sombrero<-resolver(x,y)

