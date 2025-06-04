#Ejercicio 1 
#Juego 1 
Juego1<-function(){
  num<- sample(3:18, 1, FALSE)
  lanzamiento<-sample(1:6, 3, replace=TRUE)
  resul<-sum(lanzamiento)
  
  if(resul==num){
    return(100)
   
  }
  else{
    return(-8)  }
 
}
ganancia_esperada<-mean(sapply(1:10000, function(x)Juego1()))
cat("Ganancia esperada del Juego 1:", ganancia_esperada, "dólares\n")

#Juego 2 
Juego2<- function(){
  num<-sample(1:6, 1)
  lanzar<-sample(1:6, 4, replace=TRUE)
  conteo<-sum(lanzar==num)
  if(conteo==0){
    return(-25)
  }else{
    return(10*conteo)
  }
  
}

ganancia_esperada2<-mean(sapply(1:10000, function(x)Juego2()))
cat("Ganancia esperada del Juego 2:", ganancia_esperada2, "dólares\n")
#Ejercicio 2 
# 1. Probabilidad de que a todas las personas que asistan se les asigne una mesa

# Primero, definimos la distribución del número de asistentes por reserva
asistentes <- c(2, 3, 4, 5, 6, 7, 8)
prob_asistentes <- c(0.24, 0.17, 0.29, 0.12, 0.09, 0.05, 0.04)

# Número de reservas aceptadas
n_reservas <- 35

# Probabilidad de asistencia
p_asiste <- 0.8

# Capacidad del restaurante (28 mesas de 6 personas)
capacidad <- 28 * 6

# Función para simular el número total de asistentes
simular_asistentes <- function() {
  # Determinar cuántas reservas asisten (binomial)
  reservas_que_asisten <- rbinom(1, n_reservas, p_asiste)
  
  # Para cada reserva que asiste, muestreamos el número de asistentes
  if (reservas_que_asisten > 0) {
    total_asistentes <- sum(sample(asistentes, reservas_que_asisten, replace = TRUE, prob = prob_asistentes))
  } else {
    total_asistentes <- 0
  }
  
  return(total_asistentes)
}

# Simulamos muchas veces
n_simulaciones <- 100000
resultados <- replicate(n_simulaciones, simular_asistentes())

# Probabilidad de que todos quepan
prob_todos_caben <- mean(resultados <= capacidad)
cat("1. Probabilidad de que todos quepan:", prob_todos_caben, "\n")

# 2. Probabilidad de que se requiera unir dos mesas (necesitan más de 6 personas)
# Esto ocurre cuando alguna reserva tiene 7 u 8 personas
prob_union_mesas <- sum(prob_asistentes[asistentes >= 7])
cat("2. Probabilidad de requerir unir dos mesas:", prob_union_mesas, "\n")

# 3. Probabilidad de que la cuenta supere los 100 dólares para una reserva
# Primero, necesitamos el número esperado de asistentes por reserva
media_asistentes <- sum(asistentes * prob_asistentes)

# Para una reserva, el consumo total es suma de consumos individuales (uniforme 15-25)
# Aproximamos con distribución normal
media_consumo <- (15 + 25)/2
var_consumo <- ((25 - 15)^2)/12
media_total <- media_asistentes * media_consumo
var_total <- media_asistentes * var_consumo

# Calculamos P(Total > 100)
z <- (100 - media_total) / sqrt(var_total)
prob_superar_100 <- 1 - pnorm(z)
cat("3. Probabilidad de que la cuenta supere 100 dólares:", prob_superar_100, "\n")

# 4. Estimación de la propina total en un día
# Simulamos un día completo
propina_total <- function() {
  # Número total de asistentes en el día
  total_asistentes_dia <- simular_asistentes()
  
  if (total_asistentes_dia == 0) return(0)
  
  # 20% dejan propina
  n_propinas <- rbinom(1, total_asistentes_dia, 0.2)
  
  if (n_propinas == 0) return(0)
  
  # Cada propina es exponencial con media 2
  propinas <- rexp(n_propinas, rate = 1/2)
  
  return(sum(propinas))
}

# Simulamos muchos días
propinas_diarias <- replicate(n_simulaciones, propina_total())

# Estimamos la propina media esperada
propina_media <- mean(propinas_diarias)
cat("4. Propina total estimada en un día:", propina_media, "\n")
# Ejercicio 2 Kev

set.seed(12345)

x <- c(2,3,4,5,6,7,8)

p <- c(0.24, 0.17, 0.29, 0.12, 0.09, 0.05, 0.04)

pt <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)

# 1. probabilidad de que a todas las personas que asistan al restaurante
# se les asigne una mesa

fun1 <- function(x, p){
  U <- runif(1)
  k <- 1
  pk <- p[k]
  Fk <- 0
  repeat{
    Fk <- Fk + pk
    if(U < Fk){
      return(x[k])
      break
    }
    k <- k+1
    pk <- p[k]
  }
}
fun1(x,p)

library(data.table)

T1 <- data.table(N = 1:35, X = sapply(1:35, function(k){fun1(x, p)}))
T1
T1[, MesasReservadas := ceiling(X/6)] # redondear un número hacia arriba al entero más próximo. 
T1
T1[, Asistencia := sample(c(0,1), prob = c(0.2, 0.8), size = 35, replace = TRUE)]
T1
T1[, MesasRequeridas := sapply(1:35, function(k){
  if(T1$Asistencia[k] == 1){
    return(T1$MesasReservadas[k])
  } else{
    return(0)
  }
})]
T1
res <- sum(T1$MesasRequeridas)
res

# la probabilidad se calcula de la siguiente forma:

prob <- function(res){
  if(28/res < 1){
    return(28/res)
  } else{
    return(1)
  }
}
prob(res)

# Probabilidad teorica

p %*% pt

#Chat
simulacion1 <- function() {
  T1 <- data.table(N = 1:35, X = sapply(1:35, function(k){fun1(x, p)}))
  T1[, MesasReservadas := ceiling(X/6)]
  T1[, Asistencia := sample(c(0,1), prob = c(0.2, 0.8), size = 35, replace = TRUE)]
  T1[, MesasRequeridas := ifelse(Asistencia == 1, MesasReservadas, 0)]
  return(sum(T1$MesasRequeridas) <= 28)
}

mean(replicate(10000, simulacion1()))


# 2. probabilidad de que se requieran unir dos mesas para atender la reserva

#Chat
prob_unir_mesas <- mean(T1$X[T1$Asistencia == 1] > 6)
prob_unir_mesas
simulacion2 <- function() {
  T1 <- data.table(N = 1:35, X = sapply(1:35, function(k){fun1(x, p)}))
  T1[, Asistencia := sample(c(0,1), prob = c(0.2, 0.8), size = 35, replace = TRUE)]
  return(any(T1$X[T1$Asistencia == 1] > 6))
}
mean(replicate(10000, simulacion2()))

#k
res <- table(T1$MesasRequeridas[T1$MesasRequeridas != 0])
res
res <- res/35
res
# la probabilidad se calcula de la siguiente forma:

as.numeric(res["2"])
