#generacion de v.a discretas

x_ran <- function(){
  p <- c(0.20, 0.15, 0.25, 0.40)
  pcum<-cumsum(p)
  U<- runif(1) #1 numero aleatroio entre 0 y 1
  if(U<pcum[1]){
    return(1)
  } else if (U<pcum[2]){
    return (2)
  }else if (U<pcum[3]){
    return (3)
  }else{
    return(4)
  }
}

res<-sapply(1: 100000, function(k){x_ran()})
res
table(res)/length(res) #frecuencia relativa:con qué frecuencia ocurre un valor en relación con el total de observaciones.

x_var<- function(p, x, n=1){
  return(as.numeric(as.character(cut(runif(n), breaks= c(0, cumsum(p)), labels=x))))
 
}
table(x_var(p=c(0.40, 0.25, 0.20, 0.15), x=c(4,3,2,1), n=1000000))#Usa table() para contar la frecuencia absoluta: # de veces que ocurre un valor 
#system.time(...) mide cuánto tiempo tarda en ejecutarse
system.time(sapply(1:1000000, function(k){x_ran()}))
system.time(x_var(p=c(0.10, 0.15, 0.25, 0.10, 0.40), x=c(-2, 2, 6, 10, 15), n=1000000))


#generacion de variables aleatorias Poisson
fun_poisson <- function(lambda){
  U<-runif(1)
  pk<- exp(-lambda)
  k<-0
  Fk<-0
  repeat{
    Fk<- Fk+pk
    if(U<Fk){
      return(k)
      break
    }
    k<-k+1
    pk<-(lambda/k)*pk
  
  }
}
res<-sapply(1:100000, function(k){fun_poisson(12)})
hist(res)
mean(res)

#Generacion de variables aleatorias binomial
fun_binomial<- function(n,p){
  U<- runif(1)
  pk <- (1-p)^n
  k<- Fk<- 0
  repeat{
    Fk<- Fk+pk
    if(U<Fk){
      return (k)
      break
    }
   
    pk<- ((n-k)/(k+1))*(p/(1-p))*pk
    k<- k+1
  }
}
table(sapply(1:100000, function(k){fun_binomial(16, 0.7)}))
mean(sapply(1:100000, function(k){fun_binomial(16, 0.7)}))
# implementar una funcion para simular una v.a. que provengan de una distribucion binomial negativa 

fun_binomial_negativa <- function(r, p){
  U <- runif(1)
  pk <- p^r
  k <- r
  Fk <- 0
  repeat{
    Fk <- Fk + pk
    if(U < Fk){
      return(k)
      break
    }
    pk <- (k*(1-p)/(k-r+1))*pk
    k <- k+1
  }
}

table(sapply(1:100000, function(k){fun_binomial_negativa(16, 0.7)}))
mean(sapply(1:100000, function(k){fun_binomial_negativa(16, 0.7)}))

#Metodo de aceptacion y rechazo
 Y<- c(1:10)
 p<- c(0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09,0.09, 0.10, 0.10)
 p<- c(0.20, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09,0.09, 0.10, 0.01)
 sum(p)
fun_ar<- function(Y, p){
  q<-1/length(Y)
  c<- max(p/q)
  U1<- runif(1)
  Yr<- floor(length(Y)*U1)+1  #floor() redondea hacia abajo al entero más cercano 
  #Aceptacion y rechazo
  U2<- runif(1)
  #cat("Valor de Y:", Yr, "Razon:", p[Yr]/(c*q), "Aleatorio:", U2, "\n")
  if(U2<p[Yr]/(c*q)){
    Xr<- Yr # Aceptado
    return(c(Yr, Xr))
  } else {
    return(c(Yr, NA_integer_)) # Rechazado
  }
} 
#q facil de simular, q=0.10 cte para todos pues es tiene una distribucion uniforme discerta 
fun_ar(Y, p)

library(tidyverse)
res<-as.data.frame(t(sapply(1:100000, function(k){fun_ar(Y, p)})))
res %>% group_by(V1) %>% summarise(Conteo=n(), Rechazados=sum(is.na(V2))) %>%
  mutate(Porcentaje= Rechazados/Conteo, Teórico= 1-(p/(2*1/10)))
#1.2 0 2 es la probbailidad ,mas alta
table(res[! is.na(res)])/length(res[!is.na(res)])

length(res[is.na(res)])/length(res)# num de casos que se rechaza

#variables aleatorias exponenciales
#F(x)=1-exp(-lambda x)=u

fun_exp <- function (lambda){
  U<- runif(1)
  return((-1/lambda)*log(U))
}
res<-sapply(1:10000, function(k){fun_exp(lambda=5)})
hist(res, freq= FALSE, col="#389dff", breaks= "FD", main= "F.Exponencial")
# funcion de densidad
fx<- function(x){
  lambda<- 5
  return(lambda*exp(-lambda*x))
}
curve(fx, lwd=2, add=TRUE) #graficar una funcion en una curva, add es agregar la curva al grafico existente
 #Funcion de Distribucion de laplace
fx<- function(x){
  lambda<- 3
  return((lambda/2)*exp(-lambda*abs(x)))
}
Fx<- function(x){
  lambda<-3
  if(x<0){
    return(0.5*exp(lambda*x))
  }else{
    return(1-0.5*exp(-lambda*x))
  }
}
x<- seq(-10, 10, by =0.1)
plot(x, sapply(x, function(k){Fx(k)}), type= "l")

fun_laplace<- function(lambda){
  U<-runif(1)
  if(U<0.5){
    return((1/lambda)*log(2*U))
  }else{
    return((-1/lambda)*log(2*(1-U)))
  }
}
res<-sapply(1:120000, function(k){fun_laplace(3)})
hist(res, freq= FALSE, breaks= "FD", col="green", border= "green")
curve(fx, add= TRUE, lwd=3)
#El tiempo de reparacion de unas maquinas de escribir tiene una distribucion
#aproximadamente exponencial con media 22 minutos
#a) Estime la probabilidad de que el tiempo de reparacion sea menor 
#que diez minutos
lambda <- 1/22
res1 <- sapply(1:10000, function(k){fun_exp(lambda)})
length(res1[res1 < 10])/length(res1)

# Prob. Teórica
1 - exp(-10/22)

# b) El costo de reparacion es de 2000 dolares por cada media hora o fraccion
# Cual es la probabilidad de que una reparacion cueste 4000 dolares?

library(data.table)
T1 <- data.table(N = 1:10000, X = sapply(1:10000, function(k){fun_exp(lambda)}))
T1[, CostoReparacion := 2000*ceiling(X/30)]
T1[, list(Conteo = .N), by = CostoReparacion]

# Probabilidad estimada
1894/10000

# Prob. Teorica
# P(30 < X <= 60) = F(60) - F(30)
(1-exp(-60/22)) - (1-exp(-30/22))

# c) Para efectuar una programacion, ¿Cuanto tiempo se debe asignar a cada
# reparacion para que la probabilidad de que cualquier tiempo de reparacion
# mayor que el tiempo asignado sea solo del 10%?

