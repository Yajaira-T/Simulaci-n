#EJERCICIO 1 
#a)Crear una función del método de los cuadrados medios

 cuadrados_medios <- function(numero, N) {
   resultados <- numeric(N)
   semilla <- numero
   
   for (i in 1:N) {

     cuadrado <- semilla^2
     
     # Convertir a cadena y rellenar con ceros a la izquierda si es necesario
     cuadrado_str <- sprintf("%08d", cuadrado)
     
     # Extrae las cifras centrales
     longitud <- nchar(cuadrado_str)
     inicio <- (longitud - 4) / 2 + 1
     fin <- inicio + 3
     numero_pseudoaleatorio <- as.integer(substr(cuadrado_str, inicio, fin))
     
     resultados[i] <- numero_pseudoaleatorio
     semilla <- numero_pseudoaleatorio
   }
   
   return(resultados/10^{longitud/2})
 }
 
 #Ejemplo con una semilla que sea numero primo
 cuadrados_medios(9973, 10)



# b) Aproximación de integral impropia empleando el metodo de Monte Carlo
approx_integral <- function(N = 1e5) {
  lambda <- 1  
  x <- rexp(N, rate = lambda) + pi  
  fx <- 3 * x / (1 + x^2)^2
  gx <- dexp(x - pi, rate = lambda) 
  estimate <- mean(fx / gx)
  return(estimate)
}

approx_integral()


# c) Transformada inversa
inv_transform <- function(N) {
  u <- runif(N)
  x <- (-1 + sqrt(1 + 8 * u)) / 2
  return(x)
}

trans_vals <- inv_transform(10)
cat("\nc) Variables generadas transformada inversa:\n")
print(trans_vals)


# d) Aceptacion y rechazo
f <- function(x) (2 / sqrt(pi)) * sqrt(x) * exp(-x)

# g(x): familia de propuestas (exponencial)
g <- function(x, lambda) lambda * exp(-lambda * x)

# M(lambda): cota superior de f(x)/g(x)
M_lambda <- function(lambda) {
  x_vals <- seq(0.01, 10, length.out = 1000)
  ratios <- f(x_vals) / g(x_vals, lambda)
  max(ratios)
}

# Buscar lambda óptimo 
lambdas <- seq(0.1, 2, by = 0.01)
M_vals <- sapply(lambdas, M_lambda)

lambda_opt <- lambdas[which.min(M_vals)]
M_opt <- min(M_vals)

cat("λ :", lambda_opt, "\n")
cat("Valor mínimo M", M_opt, "\n")

aceptacion_rechazo <- function(N, lambda) {
  f <- function(x) (2 / sqrt(pi)) * sqrt(x) * exp(-x)
  g <- function(x) lambda * exp(-lambda * x)
  
  x_vals <- seq(0.001, 10, length.out = 1000)
  M <- max(f(x_vals) / g(x_vals))  
  
  muestras <- numeric(0)
  while (length(muestras) < N) {
    y <- rexp(N, rate = lambda)
    u <- runif(N)
    aceptados <- y[u <= f(y) / (M * g(y))]
    muestras <- c(muestras, aceptados)
  }
  return(muestras[1:N])
}


set.seed(127)
muestra_opt <- aceptacion_rechazo(10, lambda_opt)
print("Valores generados con aceptación-rechazo")
print(muestra_opt)



#EJERCICIO 2 
set.seed(127)
n_asegurados <- 2800
lambda_mensual <- 28 
meses <- 12
gasto_por_siniestro <- 180

# Distribuciones
montos <- c(500, 1000, 2500, 5000)
prob_montos <- c(0.35, 0.30, 0.25, 0.10)
primas <- c(200, 400, 600, 800, 1000)
prob_primas <- c(0.04, 0.42, 0.29, 0.18, 0.07)

# 1. Simular primas para todos los asegurados
primas_asegurados <- sample(primas, n_asegurados, replace = TRUE, prob = prob_primas)

# 2. Simular siniestros anuales (total para todos los asegurados)
total_siniestros_anual <- sum(rpois(meses, lambda_mensual))

# 3. Asignar siniestros aleatoriamente a asegurados
ids_asegurados_con_siniestro <- sample(n_asegurados, total_siniestros_anual, replace = TRUE)

# 4. Calcular montos de siniestros y gastos
montos_siniestros <- sample(montos, total_siniestros_anual, replace = TRUE, prob = prob_montos)
gastos_siniestros <- montos_siniestros + gasto_por_siniestro

# 5. Calcular gastos por asegurado
gastos_asegurados <- tapply(gastos_siniestros, ids_asegurados_con_siniestro, sum)
gastos_totales <- numeric(n_asegurados)
gastos_totales[as.numeric(names(gastos_asegurados))] <- gastos_asegurados

# 6. Calcular resultados
resultados <- primas_asegurados - gastos_totales
genero_perdida <- resultados < 0

# Resultados
# 1. Resultado económico anual
resultado_total <- sum(resultados)
cat("1. Resultado económico anual:", resultado_total, "USD\n")

# 2. Proporción de asegurados que generan pérdida
prop_perdida <- mean(genero_perdida)
cat("2. Proporción de asegurados que generan pérdida:", round(prop_perdida * 100, 2), "%\n")

# 3. Impacto al duplicar lambda
lambda_mensual_nuevo <- 56
total_siniestros_nuevo <- sum(rpois(meses, lambda_mensual_nuevo))
ids_siniestros_nuevo <- sample(n_asegurados, total_siniestros_nuevo, replace = TRUE)
montos_nuevos <- sample(montos, total_siniestros_nuevo, replace = TRUE, prob = prob_montos)
gastos_nuevos <- tapply(montos_nuevos + gasto_por_siniestro, ids_siniestros_nuevo, sum)
gastos_totales_nuevo <- numeric(n_asegurados)
gastos_totales_nuevo[as.numeric(names(gastos_nuevos))] <- gastos_nuevos
resultados_nuevos <- primas_asegurados - gastos_totales_nuevo

cat("3. Impacto de duplicar la tasa de siniestralidad:\n")
cat("   Resultado original:", resultado_total, "USD\n")
cat("   Resultado nuevo:", sum(resultados_nuevos), "USD\n")
cat("   Diferencia:", sum(resultados_nuevos) - resultado_total, "USD\n")

