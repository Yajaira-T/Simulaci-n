#**************EJERCICIO 1 **********
#PARTE 1 
#Primera forma
x_var<- function(p, x, n=1){
  return(as.numeric(as.character(cut(runif(n), breaks= c(0, cumsum(p)), labels=x))))
  
}
res<-x_var(p=c(0.13, 0.21, 0.33, 0.17, 0.09, 0.04, 0.02, 0.01), x=c(1,2,3,4,5,6,7,8), n=43)
sum(res)
#segunda forma

set.seed(123)#para que se mantengan los resultados
sectores_por_dia <- sample(1:8, size = 43, replace = TRUE, prob = c(0.13, 0.21, 0.33, 0.17, 0.09, 0.04, 0.02, 0.01))
total_sectores <- sum(sectores_por_dia)
cat("Número total de sectores que requerirán ayuda en 43 días:", total_sectores, "\n")
cat("Distribución de sectores por día:\n")
print(table(sectores_por_dia))
#PARTE 2 
n_vehiculos <- 5
prob_averia <- 0.09
total_dias <- 43

#Simulamos vehículos operativos cada día
vehiculos_operativos <- rbinom(total_dias, n_vehiculos, 1 - prob_averia)
sectores_no_atendidos <- pmax(sectores_por_dia - vehiculos_operativos, 0)
total_no_atendidos <- sum(sectores_no_atendidos)
cat("\nNúmero total de sectores no atendidos en 43 días:", total_no_atendidos, "\n")
cat("Distribución de sectores no atendidos por día:\n")
print(table(sectores_no_atendidos))

#*****EJERCICIO 2 *****
X<-runif(1000, -1, 1)
Y<-runif(1000, -1, 1)

suma_cuadrados<- function (X, Y){
  conteo_pares=0
  for (i in 1:1000){
    if(X[i]^2+Y[i]^2<=1){
      conteo_pares<-conteo_pares+1
    }
    else {
      conteo_pares<-conteo_pares+0
    }
      
  }
  return(conteo_pares)
}
suma_cuadrados(X,Y)
suma_cuadrados(X,Y)/length(X)
pi_aprox<-suma_cuadrados(X,Y)/length(X)*4
pi_aprox
#***EJERCICIO 3 ***


#Funcion 1:exp(x^{2}+exp(x))
a <- -1
b <- 2
f <- function(x){
  return(exp(x^2 + exp(x)))
}
res <- a + (b - a) * runif(1000000)
res
mean(sapply(res, function(x) (b - a) * f(x)))
teorico <- tryCatch(integrate(f, lower = a, upper = b)$value, error = function(e) NA)
teorico

#Funcion 2: exp(-x^{2})

# Función original
g <- function(x){
  exp(-x^2)
}

# Parte positiva: x en [0, ∞)
h_pos <- function(y) {
  x <- (1 / y) - 1
  return(g(x) / y^2)
}

# Parte negativa: x en (-∞, 0]
h_neg <- function(y) {
  x <- -((1 / y) - 1)
  return(g(x) / y^2)
}

# Número de muestras
n <- 1000000
# Generar y en (0,1) pero evitar extremos(evitar divisiones por cero o valores infinitos)
y_vals <- runif(n, min = 1e-6, max = 1 - 1e-6)

# Evaluar ambas partes
I_pos <- mean(sapply(y_vals, h_pos))
I_neg <- mean(sapply(y_vals, h_neg))

# Estimación final
I_total <- I_pos + I_neg

cat("Estimación de la integral:", I_total, "\n")
cat("Valor real exacto:         ", sqrt(pi), "\n")


#***EJERCICIO 4 ***
lambda_por_minuto <- 0.1
minutos_en_hora <- 60

# Calculamos lambda para una hora
lambda_hora <- lambda_por_minuto * minutos_en_hora

# Calculamos P(X ≤ 2) usando la distribución de Poisson
prob_max_2_mensajes <- ppois(2, lambda = lambda_hora)

# Resultado
cat("Probabilidad de como máximo 2 mensajes en una hora:", prob_max_2_mensajes, "\n")

#Usando la funcion del profe
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
res<-sapply(1:10000, function(k){fun_poisson(lambda_hora)})
sum(res<=2)/10000
mean(res<=2)
#Parte 2 
# Probabilidad deseada
prob_deseada <- 0.8

# Resolvemos para t en P(X=0) = exp(-λt) = 0.8
t <- -log(prob_deseada)/lambda_por_minuto

# Resultado
cat("Tiempo necesario (minutos) para P(ningún mensaje) = 0.8:", t, "\n")
cat("Equivalente en horas:", t/60, "\n")
#otra forma
# Búsqueda iterativa del tiempo t
buscar_t <- function(prob_objetivo = 0.8, n = 10000){
  t <- 0
  paso <- 0.1  # precisión del tiempo
  
  repeat {
    lambda <- 0.1 * t
    resultados <- replicate(n, fun_poisson(lambda))
    prob_cero <- mean(resultados == 0)
    
    if (prob_cero <= prob_objetivo) {
      break
    }
    
    t <- t + paso
  }
  
  return(t)
}

# Ejecutar búsqueda
tiempo_aprox <- buscar_t()
cat("Tiempo estimado para que P(X=0) ≈ 0.8:", tiempo_aprox, "minutos\n")
