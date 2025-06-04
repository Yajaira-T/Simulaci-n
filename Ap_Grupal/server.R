library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)


#función que genera números aleatorios bajo el método congruencial multiplicativo
random_cong <-function(a, m, x0, n){
  res<-numeric(n+1)
  res[1]<-x0
  for(k in 2:length(res)){
    res[k]<-(a*res[k-1] )%% m
  }
  return(round((res[-1])/m, 6))# para que no me devuelva la semilla
}

#función que genera números aleatorios bajo el método congruencial mixto
random_mixt <-function(a, c, m, x0, n){
  res<-numeric(n+1)
  res[1]<-x0
  for(k in 2:length(res)){
    res[k]<-(a*res[k-1]+c )%% m
  }
  return(round((res[-1])/m, 6))# para que no me devuelva la semilla
}

generar_aleatorios_integral <- function(n, metodo) {
  if (metodo == "Congruencial Multiplicativo") {
    return(random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(now()), n = n))
  } else if (metodo == "Congruencial Mixto") {
    return(random_mixt(a = 7^5, c = 12345, m =2^31 - 1 , x0 = as.numeric(now()), n = n))
  } else {
    return(NULL)
  }
}

conv_matrix<-function(vector, cols=10){
  res<-rep(NA_real_,ceiling(length(vector)/cols)*cols) #ceiling redondea hacia arriba
  res[1: length(vector)]<- vector
  res<- as.data.frame(matrix(res, nrow=ceiling(length(vector)/cols), ncol=cols, byrow= TRUE))
  colnames(res)<-paste0("Cols", 1:cols) #paste0() concatena texto sin espacios.
  return(res)
}

conv_matrix(c(1,2,3,4,5,6,7,8), cols=5)

as.data.frame(conv_matrix(c(1,2,3,4,5,6,7,8), cols=5))

#FUNCIONES DE VARIABLES DISCRETAS
#Binomial
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
#Poisson
fun_poisson <- function(lambda) {
  U <- runif(1)
  k <- 0
  L <- exp(-lambda)
  p <- L
  
  while(U > p) {
    k <- k + 1
    L <- L * lambda / k
    p <- p + L
  }
  return(k)
}

#Binomial Negativa
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
# Función para distribución personalizada
fun_otro_caso <- function(valores, probabilidades) {
  U <- runif(1)
  Fx <- 0
  for (i in 1:length(valores)) {
    Fx <- Fx + probabilidades[i]
    if (U <= Fx) {
      return(valores[i])
    }
  }
  return(valores[length(valores)]) # En caso de error, devolver el último valor
}

# Define server logic required to draw a histogram
function(input, output, session) {
  
  aleatorios1 <- eventReactive(input$mostrar, {
    random_cong(a = input$constante, m = input$divisor, x0 = input$semilla, n = input$num)
  })
  
  aleatorios2 <- eventReactive(input$mostrar, {
    random_mixt(a = input$constante, c = input$c, m = input$divisor, x0 = input$semilla, n = input$num)
  })
  
  output$tabla <- function(){
    res <- conv_matrix(aleatorios1())
    kbl(res, booktabs = TRUE, escape=FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#00CD00", color = "#ffffff") %>%
      scroll_box(width = "100%", height = "200px") 
  }
  
  output$tabla2 <- function(){
    res <- conv_matrix(aleatorios2())
    kbl(res, booktabs = TRUE, escape=FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#FF4500", color = "#ffffff") %>%
      scroll_box(width = "100%", height = "200px") 
  }
  
  output$distPlot <- renderPlot({
    x <- aleatorios1()
    bins <- seq(min(x), max(x), length.out = input$barras + 1)
    hist(x, breaks = bins, col = "#00CD00", border = 'white',
         xlab = 'Numeros aleatorios generados',
         main = 'Histograma del Método 1')
  })
  
  output$distPlot2 <- renderPlot({
    x <- aleatorios2()
    bins <- seq(min(x), max(x), length.out = input$barras + 1)
    hist(x, breaks = bins, col = "#FF4500", border = 'white',
         xlab = 'Numeros aleatorios generados',
         main = 'Histograma del Método 2')
  })
  
  datos_integral <- eventReactive(input$calcular, {
    x_vals <- seq(input$lim_inf, input$lim_sup, length.out = 100)
    y_vals <- sapply(x_vals, function(x){ eval(parse(text = input$funcion)) })
    data.frame(x = x_vals, y = y_vals)
  })
  
  # Calcular área bajo la curva (regla trapezoidal)
  area_aprox <- eventReactive(input$calcular, {
    f <- function(x) eval(parse(text = input$funcion))
    a <- input$lim_inf
    b <- input$lim_sup
    
    if (b >= 99999 && a > -99999) {
      # Integral de [a, ∞)
      h <- function(y) {
        if (y <= 0 || y >= 1) return(0)
        x <- (1/y) - 1 + a
        return(f(x) / y^2)
      }
      y_vals <- seq(1e-6, 1 - 1e-6, length.out = 1000)
      delta_y <- diff(y_vals)[1]
      area <- (delta_y / 2) * (h(y_vals[1]) + 2 * sum(sapply(y_vals[2:(length(y_vals)-1)], h)) + h(y_vals[length(y_vals)]))
      return(area)
    } else if (a <= -99999 && b < 99999) {
      # Integral de (-∞, b]
      h <- function(y) {
        if (y <= 0 || y >= 1) return(0)
        x <- b - ((1 / y) - 1)
        return(f(x) / y^2)
      }
      y_vals <- seq(1e-6, 1 - 1e-6, length.out = 1000)
      delta_y <- diff(y_vals)[1]
      area <- (delta_y / 2) * (h(y_vals[1]) + 2 * sum(sapply(y_vals[2:(length(y_vals)-1)], h)) + h(y_vals[length(y_vals)]))
      return(area)
    }   else if (input$lim_inf <= -99999 && input$lim_sup >= 99999) {
      h_pos <- function(y) {
        if (y <= 0 || y >= 1) return(0)
        x <- (1 / y) - 1
        return(f(x) / y^2)
      }
      h_neg <- function(y) {
        if (y <= 0 || y >= 1) return(0)
        x <- -((1 / y) - 1)
        return(f(x) / y^2)
      }
      
      y_vals <- seq(1e-6, 1 - 1e-6, length.out = 1000)
      delta_y <- diff(y_vals)[1]
      
      area_pos <- (delta_y / 2) * (h_pos(y_vals[1]) + 2 * sum(sapply(y_vals[2:(length(y_vals)-1)], h_pos)) + h_pos(y_vals[length(y_vals)]))
      area_neg <- (delta_y / 2) * (h_neg(y_vals[1]) + 2 * sum(sapply(y_vals[2:(length(y_vals)-1)], h_neg)) + h_neg(y_vals[length(y_vals)]))
      
      return(area_pos + area_neg)
    } else {
      datos <- datos_integral()
      delta_x <- (b - a) / (length(datos$x) - 1)
      return((delta_x / 2) * (datos$y[1] + 2 * sum(datos$y[2:(length(datos$y) - 1)]) + datos$y[length(datos$y)]))
    }
  })
  
  
  
  
  # Gráfica de la función
  output$graf_fun01 <- renderPlot({
    req(input$calcular)
    datos <- datos_integral()
    area <- area_aprox()
    
    datos %>%
      ggplot(aes(x = x, y = y)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_area(aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
      geom_vline(xintercept = input$lim_inf, linetype = "dashed", color = "red") +
      geom_vline(xintercept = input$lim_sup, linetype = "dashed", color = "red") +
      labs(
        title = paste("Área bajo la curva de f(x) =", input$funcion),
        subtitle = paste("Intervalo:", input$lim_inf, "a", input$lim_sup),
        caption = paste("Área aproximada:", round(area, 4)),
        x = "x", y = "f(x)"
      ) +
      theme_minimal()
  })
  
  # Aproximación por método de Monte Carlo
  output$graf_aprox01 <- renderPlot({
    req(input$calcular)  # Asegura que haya sido presionado
    secuencia <- seq(100, 10000, by = 250)
    f <- function(x) eval(parse(text = input$funcion))
    a <- input$lim_inf
    b <- input$lim_sup
    
    valores <- eventReactive(input$calcular, {
      if (a == 0 && b == 1) {
        aprox <- sapply(secuencia, function(k) {
          res <- generar_aleatorios_integral(n = k, metodo = input$metodo)
          mean(sapply(res, f))
        })
        teorico <- integrate(f, lower = 0, upper = 1)$value
      } else if (b >= 99999 && a > -99999) {
        # [a, ∞)
        h <- function(y) {
          if (y <= 0 || y >= 1) return(0)
          x <- (1/y) - 1 + a
          return(f(x) / y^2)
        }
        aprox <- sapply(secuencia, function(k) {
          y_vals <- generar_aleatorios_integral(n = k, metodo = input$metodo)
          y_vals <- pmin(pmax(y_vals, 1e-6), 1 - 1e-6)
          mean(sapply(y_vals, h))
        })
        teorico <- tryCatch(integrate(f, lower = a, upper = Inf)$value, error = function(e) NA)
      } else if (a <= -99999 && b < 99999) {
        # (-∞, b]
        h <- function(y) {
          if (y <= 0 || y >= 1) return(0)
          x <- b - ((1 / y) - 1)
          return(f(x) / y^2)
        }
        aprox <- sapply(secuencia, function(k) {
          y_vals <- generar_aleatorios_integral(n = k, metodo = input$metodo)
          y_vals <- pmin(pmax(y_vals, 1e-6), 1 - 1e-6)
          mean(sapply(y_vals, h))
        })
        teorico <- tryCatch(integrate(f, lower = -Inf, upper = b)$value, error = function(e) NA)
      } else if (a <= -99999 && b >= 99999) {
        # (-∞, ∞)
        h_pos <- function(y) {
          if (y <= 0 || y >= 1) return(0)
          x <- (1 / y) - 1
          return(f(x) / y^2)
        }
        h_neg <- function(y) {
          if (y <= 0 || y >= 1) return(0)
          x <- -((1 / y) - 1)
          return(f(x) / y^2)
        }
        aprox <- sapply(secuencia, function(k) {
          y_vals <- generar_aleatorios_integral(n = k, metodo = input$metodo)
          y_vals <- pmin(pmax(y_vals, 1e-6), 1 - 1e-6)
          mean(sapply(y_vals, h_pos)) + mean(sapply(y_vals, h_neg))
        })
        teorico <- tryCatch({
          integrate(f, lower = -Inf, upper = Inf)$value
        }, error = function(e) NA)
      } else {
        aprox <- sapply(secuencia, function(k) {
          res <- a + (b - a) * generar_aleatorios_integral(n = k, metodo = input$metodo)
          mean(sapply(res, function(x) (b - a) * f(x)))
        })
        teorico <- tryCatch(integrate(f, lower = a, upper = b)$value, error = function(e) NA)
      }
      return(data.table(Aproximacion = aprox, Teorico = teorico))
    })
    
    
    valores() %>%
      gather(key = "Etiqueta", value = "Valor") %>%
      mutate(Aleatorios = rep(secuencia, times = 2)) %>%
      ggplot(aes(x = Aleatorios, y = Valor, group = Etiqueta, colour = Etiqueta)) +
      geom_line() + theme_minimal()
  })
  
  #Pestaña Variables Discretas
  # Pestaña Variables Discretas - Versión corregida
  
  # Variable para controlar el estado de la distribución seleccionada
  dist_seleccionada <- reactiveVal(NULL)
  parametros_mostrados <- reactiveVal(FALSE)
  
  # Observador para el botón "Seleccionar"
  observeEvent(input$siguiente, {
    dist_seleccionada(input$distribucion)
    parametros_mostrados(TRUE)
  })
  
  # UI Dinámica para parámetros (solo se muestra después de presionar "Seleccionar")
  output$parametrosUI <- renderUI({
    req(dist_seleccionada(), parametros_mostrados())
    
    if(dist_seleccionada() == "Binomial") {
      tagList(
        numericInput("ncols_bin", "número columnas (tabla de resultados)", 10, min = 5),
        numericInput("n_bin", "Número de ensayos (n):", value = 10, min = 1),
        numericInput("p_bin", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("num_bin", "Número de simulaciones:", value = 1000, min = 100),
        actionButton("calc_bin", "Calcular", class = "btn-success")
      )
    } else if(dist_seleccionada() == "Poisson") {
      tagList(
        numericInput("ncols_pois", "número columnas (tabla de resultados)", 10, min = 5),
        numericInput("lambda", "Valor de lambda (λ):", value = 4, min = 0.1, step = 0.1),
        numericInput("num_pois", "Número de simulaciones:", value = 1000, min = 100),
        actionButton("calc_pois", "Calcular", class = "btn-success")
      )
    } else if(dist_seleccionada() == "Binomial Negativa") {
      tagList(
        numericInput("ncols_bin_neg", "número columnas (tabla de resultados)", 10, min = 5),
        numericInput("r_bin_neg", "Número de éxitos (r):", 10, min = 1),
        numericInput("p_bin_neg", "Probabilidad de éxito (p):", 0.5, min = 0, max = 1, step = 0.01),
        numericInput("num_bin_neg", "Número de simulaciones:", 1000, min = 100),
        actionButton("calc_bin_neg", "Calcular", class = "btn-success")
      )
    }else if(dist_seleccionada() == "Otro caso") {
      tagList(
        numericInput("ncols_otro", "Número columnas (tabla de resultados)", 10, min = 5),
        textInput("valores_otro", "Valores de X (separados por comas):", "1, 2, 3, 4"),
        textInput("probabilidades_otro", "Probabilidades (separadas por comas, deben sumar 1):", "0.1, 0.2, 0.3, 0.4"),
        numericInput("num_otro", "Número de simulaciones:", value = 1000, min = 100),
        actionButton("calc_otro", "Calcular", class = "btn-success")
      )
    }
  })
  
  # Lógica para Binomial
  sim_bin <- eventReactive(input$calc_bin, {
    req(input$n_bin, input$p_bin, input$num_bin)
    replicate(input$num_bin, fun_binomial(input$n_bin, input$p_bin))
  })
  
  output$histBin <- renderPlot({
    req(sim_bin())
    hist(sim_bin(), breaks = 30, col = "#1E90FF", 
         main = "Distribución Binomial Simulada",
         xlab = "Valores", ylab = "Frecuencia")
  })
  
  output$statsBin <- renderPrint({
    req(sim_bin())
    x <- sim_bin()
    cat("Media:", mean(x), "\nVarianza:", var(x))
  })
  
  output$tablaBin <- function(){
    
    req(input$distribucion, input$siguiente > 0, input$calc_bin > 0)
    
    res <- conv_matrix(sapply(1:input$num_bin, function(k){fun_binomial(input$n_bin, input$p_bin)}), cols = input$ncols_bin)
    kbl(res, booktabs = TRUE, escape=FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#FF4500", color = "#ffffff") %>%
      scroll_box(width = "100%", height = "200px")
    
  }
  
  # Lógica para Poisson (VERSIÓN CORREGIDA)
  observeEvent(input$calc_pois, {
    req(input$lambda, input$num_pois)
    # Forzamos la actualización de los resultados
    output$histPois <- renderPlot({
      x <- replicate(input$num_pois, fun_poisson(input$lambda))
      hist(x, breaks = 30, col = "#FF6347", 
           main = "Distribución Poisson Simulada",
           xlab = "Valores", ylab = "Frecuencia")
    })
    
    output$statsPois <- renderPrint({
      x <- replicate(input$num_pois, fun_poisson(input$lambda))
      cat("Media:", mean(x), "\nVarianza:", var(x))
    })
    
    output$tablaPois <- function(){
      
      req(input$distribucion, input$siguiente > 0)
      
      res <- conv_matrix(sapply(1:input$num_pois, function(k){fun_poisson(input$lambda)}), cols = input$ncols_pois)
      kbl(res, booktabs = TRUE, escape=FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#FF4500", color = "#ffffff") %>%
        scroll_box(width = "100%", height = "200px")
      
    }
  })
  
  # Lógica para Binomial Negativa
  observeEvent(input$calc_bin_neg, {
    req(input$r_bin_neg, input$p_bin_neg, input$num_bin_neg)
    # Forzamos la actualización de los resultados
    output$histBin_neg <- renderPlot({
      x <- replicate(input$num_bin_neg, fun_binomial_negativa(input$r_bin_neg, input$p_bin_neg))
      hist(x, breaks = 30, col = "#FF6347", 
           main = "Distribución Binomial Negativa Simulada",
           xlab = "Valores", ylab = "Frecuencia")
    })
    
    output$statsBin_neg <- renderPrint({
      x <- replicate(input$num_bin_neg, fun_binomial_negativa(input$r_bin_neg, input$p_bin_neg))
      cat("Media:", mean(x), "\nVarianza:", var(x))
    })
    
    output$tablaBin_neg <- function(){
      
      req(input$distribucion, input$siguiente > 0)
      
      res <- conv_matrix(sapply(1:input$num_bin_neg, function(k){fun_binomial_negativa(input$r_bin_neg, input$p_bin_neg)}), cols = input$ncols_bin_neg)
      kbl(res, booktabs = TRUE, escape=FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#FF4500", color = "#ffffff") %>%
        scroll_box(width = "100%", height = "200px")
      
    }
  })
  # Lógica para Otro Caso
  observeEvent(input$calc_otro, {
    req(input$valores_otro, input$probabilidades_otro, input$num_otro)
    
    # Procesar los valores y probabilidades ingresados
    valores <- as.numeric(unlist(strsplit(input$valores_otro, ",")))
    probabilidades <- as.numeric(unlist(strsplit(input$probabilidades_otro, ",")))
    
    # Validar que las probabilidades sumen 1 (con cierta tolerancia)
    if(abs(sum(probabilidades) - 1) > 0.001) {
      showModal(modalDialog(
        title = "Error",
        "Las probabilidades deben sumar 1",
        easyClose = TRUE
      ))
      return()
    }
    
    # Validar que haya la misma cantidad de valores y probabilidades
    if(length(valores) != length(probabilidades)) {
      showModal(modalDialog(
        title = "Error",
        "Debe haber el mismo número de valores y probabilidades",
        easyClose = TRUE
      ))
      return()
    }
    
    # Función para generar valores según la distribución personalizada
    generar_otro_caso <- function(n) {
      replicate(n, fun_otro_caso(valores, probabilidades))
    }
    
    # Generar los valores
    x <- generar_otro_caso(input$num_otro)
    
    # Mostrar resultados
    output$histOtro <- renderPlot({
      hist(x, breaks = 30, col = "#C8A2C8", 
           main = "Distribución Personalizada Simulada",
           xlab = "Valores", ylab = "Frecuencia")
    })
    
    output$statsOtro <- renderPrint({
      cat("Media:", mean(x), "\nVarianza:", var(x))
    })
    
    output$tablaOtro <- function(){
      req(input$distribucion, input$siguiente > 0)
      res <- conv_matrix(generar_otro_caso(input$num_otro), cols = input$ncols_otro)
      kbl(res, booktabs = TRUE, escape=FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#8FBC8F", color = "#ffffff") %>%
        scroll_box(width = "100%", height = "200px")
    }
  })
  
  # Observador para resetear cuando cambia la distribución seleccionada
  observeEvent(input$distribucion, {
    if(!is.null(dist_seleccionada())) {
      if(input$distribucion != dist_seleccionada()) {
        parametros_mostrados(FALSE)
      }
    }
  })
  
  
  
}
