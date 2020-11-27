###################################################################
#Research and development expenditure (% of GDP)
#fuente data.worldbank.org
#https://data.worldbank.org/indicator/gb.xpd.rsdv.gd.zs?end=2016&start=1996&year_high_desc=false
##################################################################
#En economia, suelen aparecer variables explicativas muy correlacionadas
#El gasto interno bruto en investigación
# y desarrollo se define como el gasto total (corriente y capital) 
#en I + D realizado por todas las empresas residentes en un pais, 
#sus institutos de investigacio ́n, laboratorios universitarios y gubernamentales,
#etc. Incluye la I + D financiada desde el extranjero, 
#pero excluye los fondos nacionales para la I + D realizada fuera de la 
#economia nacional. Este indicador se mide como porcentaje del PBI.
#Fuentes: data.worldbank.org y data.oecd.org

# Queremos explicar el % de gasto en I+D de EEUU
# usando como covariables los % de gasto en I + D de otros paıses: Argentina,
# Alemania, China,japon, Francia, Reino Unido, Finlandia, entre 1996-2016.

rdpercentGDP <- read.csv("rdpercentGDP",sep = "")
View(rdpercentGDP)
range(rdpercentGDP[,-1])
attach(rdpercentGDP)

plot(1996:2016,france,type="l",col="blue",ylim=c(0,3.8),xlab="years",ylab="R&D",
       main="Research and development expenditure (% of GDP)",lwd=2)
points(1996:2016,usa,type="l",lwd=2)
points(1996:2016,argen,type="l",col="lightblue",lwd=2)
points(1996:2016,ger,type="l",col="orange",lwd=2)
points(1996:2016,china,type="l",col="red",lwd=2)
points(1996:2016,japan,type="l",col="green",lwd=2)
points(1996:2016,finl,type="l",col="magenta",lwd=2)
points(1996:2016,uk,type="l",col="grey",lwd=2)

text(2015.2,japan[21]+0.3,labels="Japan",col="green")
text(2015.2,france[21]+0.15,labels="France",col="blue")
text(2015,ger[21]+0.1,labels="Germany",col="orange")
text(2015.8,usa[21]-0.1,labels="USA",col="black")
text(2015.2,china[21]-0.2,labels="China",col="red")
text(2014.8,argen[21]+0.2,labels="Argentina",col="lightblue")
text(2015.5,uk[21]-0.2,labels="UK",col="grey")
text(2012,finl[17]+0.4,labels="Finland",col="magenta")

# ¿Estan correlacionadas las variables?

round(cor(rdpercentGDP[2:9]),3)


ajustels<-lm(usa~.,data = rdpercentGDP[,-1] )
XX<-model.matrix(ajustels)   #matriz de disenio
autoval<-eigen(t(XX)%*%XX)$values

summary(ajustels) #no dan significativas...pero estaban re correlacionadas


#numero de condicion
autoval[1]/autoval[8] # Enorme

#ahora con Ridge
library(glmnet)
XX<-model.matrix(ajustels)    #matriz de disenio

# poner alpha = 0 para que haga el ajuste ridge
ajuste.ridge = glmnet(XX,usa,alpha=0)   

plot(ajuste.ridge,label = T,xvar="lambda",lwd=2,main="Ridge Regression")


#podemos fijar los valores para lambda, parametro de suavizado
grid = 10^seq(10,-2,length=100)
ajuste.ridge.con.grilla = glmnet(XX,usa,lambda=grid,alpha=0)   

plot(ajuste.ridge,label = T,xvar="lambda")

plot(ajuste.ridge.con.grilla,label = T,xvar="lambda")
abline(h=0)

rid.ols<-glmnet(XX, usa,alpha=0,lambda = 0)
coef.glmnet(rid.ols)
coef(ajustels)
#coinciden (practicamente)

#podriamos ir probando con varios lambda a mano y buscar el que mejor ajuste...

rid.l08<-glmnet(XX, usa,alpha=0,lambda = 0.08453212)
rid.l1<-glmnet(XX, usa,alpha=0,lambda = 1)
rid.l2<-glmnet(XX, usa,alpha=0,lambda = 2)
rid.l4<-glmnet(XX, usa,alpha=0,lambda = 4)

cbind(coef(rid.l08),coef(rid.l1),coef(rid.l2),coef(rid.l4))


# seleccionamos el valor de lambda para ridge por crossval
# con K=5. Obtenemos el minimo y el minimo con el criterio
# de un desvio estandar

rid.crossval<-cv.glmnet(XX, usa,nfolds = 5, alpha=0)
rid.crossval
names(rid.crossval)
rid.crossval$lambda.min
rid.crossval$lambda.1se

plot(rid.crossval)

rid.1se<-glmnet(XX, usa,alpha=0,lambda = rid.crossval$lambda.1se)
rid.min<-glmnet(XX, usa,alpha=0,lambda = rid.crossval$lambda.min)


#puedo calcular el ecm para después elegir el mejor ajuste de todos
c_ridg<-coef(rid.1se)
ecm_ridg<-mean((usa-t(XX)%*%matrix(c_ridg[-2],nrow=8,ncol=1))^2)


## LASSO

ajus_lasso<-glmnet(XX,usa,alpha=1)
names(ajus_lasso)
summary(ajus_lasso)
plot(ajus_lasso,xvar="lambda",label=TRUE)
abline(0,0,lty=3)


# seleccionamos el valor de lambda para lasso por crossval
# con K=5. Obtenemos el minimo y el minimo con el criterio
# de un desvio estandar

las.crossval<-cv.glmnet(XX, usa,nfolds = 5, alpha=1)

las.crossval$lambda.min
las.crossval$lambda.1se

plot(las.crossval)

las.1se<-glmnet(XX, usa,alpha=1,lambda = las.crossval$lambda.1se)
coef(las.1se)

las.min<-glmnet(XX, usa,alpha=1,lambda = las.crossval$lambda.min)
coef(las.min)

# poner alpha = 1 para que haga el ajuste lasso
ajuste.lasso = glmnet(XX,usa,alpha=1)   
ajuste.lasso.1de = glmnet(XX,usa,alpha=1,lambda = las.crossval$lambda.1se)   
coef(ajuste.lasso.1de)



