# Generacion de variables aleatorias normales estandar

# funcion de densidad de una distribucion normal
fx <- function(x){
  return(2/sqrt(2*pi)*exp(-0.5*x^2))
}

x_vals <- seq(0, 5, by = 0.1)
plot(x_vals, fx(x_vals), type = "l", col = 2)
integrate(f = fx, lower = 0, upper = 5)


gx <- function(x){
  return(1.315489*exp(-x))
}

x_vals <- seq(0, 5, by = 0.1)
plot(x_vals, fx(x_vals), type = "l", col = 2, ylim = c(0,0.9))
par(new = TRUE)
plot(x_vals, gx(x_vals), type = "l", col = 3, ylim = c(0,0.9))

# Busqueda de la constante c
h <- function(x){
  ifelse(gx(x) == 0, -Inf, fx(x)/gx(x))
}
optimize(h, interval = c(0,4), maximum = TRUE)

# valor teórico
sqrt(2*exp(1)/pi)

# Simulacion valores
fun_normal <- function(N){
  n <- 0
  vec <- numeric(N)
  while(n < N){
    U1 <- runif(1)
    Y <- -log(U1)
    U2 <- runif(1)
    U3 <- runif(1)
    if(U2 <= exp(-0.5*(Y-1)^2)){
      if(U3 < 0.5){
        vec[n + 1] <- -Y
      } else {
        vec[n + 1] <- Y
      }
    } else {
      vec[n + 1] <- NA_real_
    }
    n <- n + 1
  }
  return(vec)
}

# Porcentaje rechazados
# sum(is.na(fun_normal(100000)))/100000
X <- fun_normal(100000)
hist(X[!is.na(X)])

fun_normal2 <- function(N){
  n <- 0
  vec <- numeric(N)
  while(n < N){
    Y1 <- -log(runif(1))
    Y2 <- -log(runif(1))
    if(Y2 >= 0.5*(Y1-1)^2){
      if(runif(1) < 0.5){
        vec[n + 1] <- -Y1
      } else{
        vec[n + 1] <- Y1
      }
    } else {
      vec[n + 1] <- NA_real_
    }
    n <- n + 1
  }
  return(vec)
}

# Porcentaje rechazados
X <- fun_normal2(100000)
hist(X[!is.na(X)])

system.time(fun_normal(1000000))
system.time(fun_normal2(1000000))

# Transformaciones de Box Muller
fun_box <- function(N){
  vec <- numeric(N)
  for(i in 1:(N/2)){
    R <- sqrt(-2*log(runif(1)))
    theta <- 2*pi*runif(1)
    vec[2*i-1] <- R*cos(theta)
    vec[2*i] <- R*sin(theta)
  }
  return(vec)
}

hist(fun_box(100000))
system.time(hist(fun_box(1000000)))

plot(c(0,X), c(0,Y), type = "b")

fun_box2 <- function(N){
  vec <- numeric(N)
  for(i in 1:(N/2)){
    V1 <- 2*runif(1) - 1
    V2 <- 2*runif(1) - 1
    S <- V1^2 + V2^2
    if(S <= 1){
      X <- sqrt(-2*log(S)/S)*V1
      Y <- sqrt(-2*log(S)/S)*V2
    } else {
      X <- NA_real_
      Y <- NA_real_
    }
    vec[2*i-1] <- X
    vec[2*i] <- Y
  }
  return(vec)
}

hist(fun_box2(1000000))
system.time(hist(fun_box2(1000000)))

# Porcentaje rechazados teorico
1 - pi/4


