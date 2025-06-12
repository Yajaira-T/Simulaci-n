
par(mar = c(4, 4, 2, 1))  
#****EJERCICIO 1****
cuadrados_m <- function(x0,n){
  x02 <- x0^{2}
  xi <- sprintf(paste0("%0", 2*n, "d"), x02)
  
  i <- floor(n/2) + 1
  f <- i + n - 1
  xiaux <- substr(xi, i, f)
  xi <- as.numeric(xiaux)
  return(xi)
}

#a) 
Generar_valores <- function(N, x0, n) {
  v <- numeric(N)
  v[1] <- x0/10^{n}
  for (i in 2:N) {
    x0 <- cuadrados_m(x0, n)
    if (x0 == 0) x0 <- 1237 
    v[i] <- x0 / 10^n 
  }
  return(v)
}
#Ejemplo
Generar_valores( N = 10, x0 = 4133, n = 4)

#b) Aproximacion de la integral
a <- pi
f <- function(x){
  return(3*x*(1+x^{2})^{-2})
} 
n <- 1000000
h <- function(y) {
  if (y <= 0 || y >= 1) return(0)
  x <- (1/y) - 1 + a
  return(f(x) / y^2)
}
y <- Generar_valores(n, x0 = 4122, n = 4)
y <- pmin(pmax(y_vals, 1e-6), 1 - 1e-6)

aprox <- mean(sapply(y, h))
cat("Valor estimado:", aprox, "\n")

real <- integrate(f, lower = a, upper = Inf)$value
cat("Valor teórico:", real, "\n")

#c) 

n_muestras <- 10000

res <- f_inv(n_muestras)
hist(res, breaks = 30, freq = FALSE, main = "Densidad de las muestras generadas",
     xlab = "x", ylab = "Densidad")
curve((2*x + 1)/2, add = TRUE, col = "blue", lwd = 2)  
legend("topright", legend = "PDF teórica", col = "blue", lwd = 2)

#d) 

f <- function(x){
  return(2/sqrt(pi)* sqrt(x)*exp(-x))
}
x <- seq(0,10, by = 0.01)

g <- function(x){
  lambda <- 0.7
  return(lambda*exp(-lambda*x))
}
plot(x, f(x), type = "l", ylim = c(0,1))
par(new=TRUE)
plot(x,g(x),type = "l", ylim= c(0,1),col = "blue")

# constante c
h <- function(x){
  ifelse(g(x)==0, -Inf, f(x)/g(x))
}
optimize(h, interval = c(0,4), maximum = TRUE)


cg <- function(x){
  return( 1.262217*g(x))
}
par(new = TRUE)
plot(x,cg(x),type = "l", ylim= c(0,1),col = "green")

#simulacion
fun <- function(){
  c <-  1.262217
  U1 <- mcm(x0 = 1237, n = 4)/10^{4}
  lambda <- 0.7
  Y <- (-1/lambda)*log(U1)  
  U2 <- mcm(x0 = 1237, n = 4)/10^{4}
  if (U2 <= f(Y)/(c*g(Y))){
    return(Y)
  }else{
    NA
  }
}

fun()
res2 <- sapply(1:100000,function(k){fun()})
res2 <- res2[!is.na(res2)]
hist(res2, freq = FALSE, breaks = "FD")
curve(f, add = TRUE, lw =2)
summary(res2)
#Porcentaje de aceptacion
length(res2)/100000
#Media
1/1.262217

#EJERCICIO 2 
set.seed(127)  
n_polizas <- 2800
# Función para simular un año para una póliza
simular_poliza <- function(lambda) {
  # Simular número de siniestros en 12 meses
  siniestros_anual <- sum(rpois(12, lambda))
  
  # Simular montos de indemnización
  if (siniestros_anual > 0) {
    montos <- sample(c(500, 1000, 2500, 5000), 
                     size = siniestros_anual, 
                     prob = c(0.35, 0.30, 0.25, 0.10), 
                     replace = TRUE)
    total_indemnizacion <- sum(montos)
    gastos_fijos <- siniestros_anual * 180
  } else {
    total_indemnizacion <- 0
    gastos_fijos <- 0
  }
  
  #  prima cobrada
  prima <- sample(c(200, 400, 600, 800, 1000), 
                  size = 1, 
                  prob = c(0.04, 0.42, 0.29, 0.18, 0.07))
  
  
  resultado <- prima - (total_indemnizacion + gastos_fijos)
  
  return(c(resultado, ifelse(resultado < 0, 1, 0)))
}

# Simulación para lambda = 28

resultados <- replicate(n_polizas, simular_poliza(28))
resultado_promedio <- mean(resultados[1,])
proporcion_perdedoras <- mean(resultados[2,])

# Simulación para lambda* = 56
resultados_duplicado <- replicate(n_sim, simular_poliza(56))
resultado_promedio_duplicado <- mean(resultados_duplicado[1,])

# Resultados
resultado_anual <- n_polizas * resultado_promedio
impacto_duplicado <- n_polizas * (resultado_promedio_duplicado - resultado_promedio)

cat("1. Resultado económico anual estimado (λ=28):", resultado_anual, "USD\n")
cat("2. Proporción de asegurados que generan pérdida:", proporcion_perdedoras * 100, "%\n")
cat("3. Impacto en el resultado económico al duplicar λ (λ*=56):", impacto_duplicado, "USD\n")

