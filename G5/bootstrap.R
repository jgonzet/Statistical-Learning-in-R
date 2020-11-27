#Bootstrap - Toy example

set.seed(27)
ene<- 50
x<- rexp(ene)
mediana<-median(x)
mediana


boot=10000
median.boot<- rep(NA,boot)
set.seed(999)
for(i in 1:boot) 
  {
    ind.boot<-sample(1:ene, replace = TRUE)
    median.boot[i]<-median(x[ind.boot]) 
  }

head(median.boot)
hist(median.boot)


median.prom.boot<- mean(median.boot)
var.median.boot<- mean((median.boot-median.prom.boot)^2)
alfa= 0.05

limites.median_LI<- mediana-qnorm(1-alfa/2)*sqrt(var.median.boot)
limites.median_LS<- mediana+qnorm(1-alfa/2)*sqrt(var.median.boot)
c(limites.median_LI,limites.median_LS)


# Bootstrap percentil

head(median.boot)

alfa= 0.05
IC.median.per_LI<- quantile(median.boot, probs = alfa/2, na.rm = T)
IC.median.per_LS<- quantile(median.boot, probs = 1-alfa/2, na.rm = T)
c(IC.median.per_LI,IC.median.per_LS)

