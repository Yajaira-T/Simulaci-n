#Metodo de aceptacion y rechazo
#f(x)= 20x(1-x)^3 0<x<1

x<-seq(0,1, by=0.01)
fx<- function(x){
  return(20*x*(1-x)^3)
}
gx<- function(x){
  return(rep(1, length(x)))
}
cgx<-function(x){
  return((135/64)*rep(1, length(x)))
}
plot(x, fx(x), type="l", ylim=c(0,3))
par(new=TRUE)#unir las graficas
plot(x, gx(x), type="l", ylim=c(0,3), col="red")
par(new=TRUE)
plot(x, cgx(x), type="l", ylim=c(0,3), col="blue")

#Simulacion
#g es uniforme en 0 y 1
fun<-function(){
  c<-135/64
  U1<- runif(1) #apartir de U1 obtenemos el valor de Y=UI
  U2<-runif(1)
  if(U2<=fx(U1)/(c*gx(U1))){
    return(U1)
  }else{
    NA
  }
  #para comentar selecciono y luego control shift c 
 # cat("Valor Y:", U1, "\n ", "f(Y) ", fx(U1), "\n ", 
 #     "c.g(Y)", cgx(U1), "\n ", "Razon : ", fx(U1)/cgx(U1), "\n ",
 #     "Valor U2:", U2) 
}

res<- sapply(1:10000, function(k){fun()})
res<-res[!is.na(res)]
#porcentaje de aceptacion
length(res)/10000
#media(1/c)
64/135



#Ejemplo 
x<-seq(0,10, by=0.01)
fx<- function(x){
  return(x*exp(-0.5*x^2))
}
gx<- function(x){ #funcion de densidad exponencial
  lambda<-1/2
  return(lambda*exp(-lambda*x))
}
#busqueda de la constante c 
h<- function(x){
  ifelse(gx(x)==0, -Inf, fx(x)/gx(x))
}
optimize(h, interval = c(0,4), maximum = TRUE)
cgx<-function(x){
  return(2.140004*gx(x))
}
plot(x, fx(x), type="l", ylim=c(0,1))
par(new=TRUE)#unir las graficas
plot(x, gx(x), type="l", ylim=c(0,1), col="red")
par(new=TRUE)
plot(x, cgx(x), type="l", ylim=c(0,1), col="blue")

#Simulacion
fun<-function(){
  c<-2.140004
  U1<- runif(1) 
  lambda<-1/2
  Y<-(-1/lambda)*log(U1)# Y sigue una distribucion exponencial(1/2)
  U2<-runif(1)
  if(U2<=fx(Y)/(c*gx(Y))){
    return(Y)
  }else{
    NA
  }
}
fun()

res<- sapply(1:100000, function(k){fun()})
res<-res[!is.na(res)]
summary(res)
hist(res, freq = FALSE, breaks="FD")
curve(fx, add = TRUE, lw=2)
#porcentaje de aceptacion
length(res)/100000
#media(1/c)
1/2.140004

#con lambda 2/5
x<-seq(0,10, by=0.01)
fx<- function(x){
  return(x*exp(-0.5*x^2))
}
gx<- function(x){ #funcion de densidad exponencial
  lambda<-2/5
  return(lambda*exp(-lambda*x))
}
#busqueda de la constante c 
h<- function(x){
  ifelse(gx(x)==0, -Inf, fx(x)/gx(x))
}
optimize(h, interval = c(0,4), maximum = TRUE)
cgx<-function(x){
  return(2.360661*gx(x))
}
plot(x, fx(x), type="l", ylim=c(0,1))
par(new=TRUE)#unir las graficas
plot(x, gx(x), type="l", ylim=c(0,1), col="red")
par(new=TRUE)
plot(x, cgx(x), type="l", ylim=c(0,1), col="blue")

#Simulacion
fun<-function(){
  c<-2.360661
  U1<- runif(1) 
  lambda<-2/5
  Y<-(-1/lambda)*log(U1)# Y sigue una distribucion exponencial(1/2)
  U2<-runif(1)
  if(U2<=fx(Y)/(c*gx(Y))){
    return(Y)
  }else{
    NA
  }
}
fun()

res<- sapply(1:100000, function(k){fun()})
res<-res[!is.na(res)]
summary(res)
hist(res, freq = FALSE, breaks="FD")
curve(fx, add = TRUE, lw=2)
#porcentaje de aceptacion
length(res)/100000
#media(1/c)
1/2.360661


