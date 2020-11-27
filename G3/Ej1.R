setwd("~/Documents/R/Datos")
productos<-read.table("producto.txt")
colnames(productos)<-c("Resultado" ,"Precio", "Marketing")

colores<-c("black", "red")
plot(productos$Precio,productos$Marketing,col=colores[productos$Resultado],pch=20,xlab="Precio",ylab="Presupuesto",main="Ejercicio 1")

media_precio<-mean(productos$Precio)
media_marketing<-mean(productos$Marketing)
media<-c(media_precio,media_marketing)
sd_precio<-sd(productos$Precio)
sd_marketing<-sd(productos$Marketing)
sd<-c(sd_precio,sd_marketing)


productos$Precio<-(productos$Precio-media_precio)/sd_precio
productos$Marketing<-(productos$Marketing-media_marketing)/sd_marketing


vecinos.k<-function(x,k,x0) #esta funcion me devuelve la posicion de los K vecinos mas cercanos en X
{
  distancia<-c()
  for(i in 1:dim(x)[1])
  {
    distancia[i]<-sqrt(sum((x[i,]-x0)^2))
  }
  minimos<-sort(distancia,index.return=TRUE)$ix
  return(minimos[1:k])
}

clasificador<-function(x,k,x0)
{
  xx<-x[,2:3]
  y<-x[,1]
  y_elegidos<-y[vecinos.k(xx,k,x0)]
  prediccion<-ifelse(mean(y_elegidos=="exito")>0.5,"exito","fracaso")
  return(prediccion)
}

clasificador2<-function(x,y,k,x0)
{
  prediccion<-which.max(table(y[vecinos.k(x,k,x0)]))
  return(names(prediccion))
}

x0<-c(5,5)
clasificador(productos,10,x0)
clasificador2(productos[,2:3],productos[,1],10,x0)
