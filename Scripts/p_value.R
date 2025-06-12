X<-sort(c(6.0, 2.3, 4.8, 5.6, 4.5, 3.4, 3.3, 1.9, 4.8, 4.5))
Z<- (X-mean(X))/sd(X) #sd desviacion estadndar #SE ESTANDARIZA
F0<- pnorm(Z) #funcion de distribucion acumulada teorica
Fn<-seq(1,10)/10# FDA EMPIRICA
Dmas<- max(Fn-F0)
Dmenos<- max(F0-Fn+1/10)

X<-runif(100, min=1, max=5) #veo si siguen una districubion normal de media 4.1 y desviacion estandar 1.82
Y<-rnorm(100, mean=4.1, sd=sqrt(1.82))
ks.test(X, Y) #X: VALORES OBSERVADOS, Y:  VALORES TEORICOS, me arroja el p_value
#alfa=0.05 siempre, si acepto entonces sigue la distribucion 
ks.test(X, Y)$p.value #me arroja el p_value
#si el p_value es mayor a 0.05, entonces acepto la hipotesis nula de que los datos siguen una distribucion normal


X<-rexp(100, rate=2) 
Y<-rnorm(100, mean=4.1, sd=sqrt(1.82))
hist(X)
hist(Y)
ks.test(X, Y)$p.value #me arroja el p_value

