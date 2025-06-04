library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

source("fun_auxiliares.R")
#congruencial multiplicativo
random_cong <- function(a, m, x0, n) {
  res <- numeric(n + 1)#Crea un vector numérico llamado res con n+1 elementos, todos inicialmente en cero.
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k - 1]) %% m
  }
  return(res[-1] / m) #divide cada uno por m para normalizar los resultados a números reales en el intervalo(0,1)
}
#congruencial mixto
random_mixt <- function(a, c, m, x0, n) {
  res <- numeric(n + 1)
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k - 1] + c) %% m
  }
  return(round(res[-1] / m, 6))
}

conv_matrix <- function(vector, cols = 10) {
  res <- rep(NA_real_, ceiling(length(vector) / cols) * cols)
  res[1:length(vector)] <- vector
  res <- as.data.frame(matrix(res, nrow = ceiling(length(vector) / cols), ncol = cols, byrow = TRUE))
  colnames(res) <- paste0("Col", 1:cols)
  return(res)
}

function(input, output, session) {
  
  get_limits <- reactive({
    if (!input$usar_inf) {
      c(input$lim_inf, input$lim_sup)
    } else {
      c(as.numeric(input$lim_inf_inf), as.numeric(input$lim_sup_inf))
    }
  })
  #La función get_limits() devuelve un vector de longitud 2 con los límites de integración a usar
  output$tabla_mult <- function(){
    
    res <- conv_matrix(random_cong(a = input$constante, m = input$divisor, x0 = input$semilla, n = input$num))
    
    kbl(res, booktabs = TRUE, escape = FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#1D3889", color = "#ffffff") %>% scroll_box(width = "100%", height = "200px")
    #scroll_box(...)Hace que la tabla sea desplazable si ocupa más de 200px 
  }
  
  output$tabla_mixto <- function(){
    res <- conv_matrix(random_mixt(a = input$constante, c = input$incremento, m = input$divisor, x0 = input$semilla, n = input$num))
    
    kbl(res, booktabs = TRUE, escape = FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#1D3889", color = "#ffffff") %>%
      scroll_box(width = "100%", height = "200px")
  }
  
  output$hist_mult <- renderPlot({
    datos <- random_cong(a = input$constante, m = input$divisor, x0 = input$semilla, n = input$num)
    ggplot(data.frame(x = datos), aes(x)) +
      geom_histogram(bins = 30, fill = "#1ABC9C", color = "#2C3E50", alpha = 0.9) +
      labs(title = "Método Multiplicativo", x = "Valores generados", y = "Frecuencia") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
  })
  
  output$hist_mixto <- renderPlot({
    datos <- random_mixt(a = input$constante, c = input$incremento, m = input$divisor, x0 = input$semilla, n = input$num)
    ggplot(data.frame(x = datos), aes(x)) +
      geom_histogram(bins = 30, fill = "#9B59B6", color = "#922B21", alpha = 0.9) +
      labs(title = "Método Mixto", x = "Valores generados", y = "Frecuencia") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
  })
  
 
  
  

  
  
  output$graf_fun01 <- renderPlot({
    f <- function(x) eval(parse(text = input$funcion))
    lims <- get_limits()
    plot_lims <- lims
    if (any(is.infinite(plot_lims))) {
      plot_lims[is.infinite(plot_lims)] <- c(-10, 10)[is.infinite(plot_lims)]
    }
    x_vals <- seq(plot_lims[1], plot_lims[2], length.out = 100)
    y_vals <- sapply(x_vals, f)
    area <- tryCatch({
      integrate(f, lower = lims[1], upper = lims[2])$value
    }, error = function(e) NA)
    
    data.frame(x = x_vals, y = y_vals) %>%
      ggplot(aes(x = x, y = y)) +
      geom_line(color = "blue") +
      geom_area(fill = "lightblue", alpha = 0.3) +
      labs(title = paste("Área bajo la curva de f(x) =", input$funcion),
           subtitle = paste("Intervalo:", lims[1], "a", lims[2]),
           caption = paste("Área teórica aproximada:", round(area, 4))) +
      theme_minimal()
  })
  
  output$graf_aprox01 <- renderPlot({
    f_expr <- input$funcion
    lims <- get_limits()
    secuencia <- seq(100, 10000, by = 250)
    
    generador <- switch(input$metodo,
                        "Congruencial Multiplicativo" = function(k) random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = k),
                        "Congruencial Mixto" = function(k) random_mixt(a = 7^5, c = 12345, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = k)
    )
    
    aprox <- sapply(secuencia, function(k) {
      u <- generador(k)
      
      if (lims[2] == Inf && lims[1] == 0) {
        t <- u
        x <- t / (1 - t)
        w <- 1 / (1 - t)^2
        f_vals <- sapply(x, function(xi) eval(parse(text = f_expr), envir = list(x = xi)))
        mean(f_vals * w)
        
      } else if (lims[2] == Inf && is.finite(lims[1])) {
        a <- lims[1]
        t <- u
        x <- a + t / (1 - t)
        w <- 1 / (1 - t)^2
        f_vals <- sapply(x, function(xi) eval(parse(text = f_expr), envir = list(x = xi)))
        mean(f_vals * w)
        
      } else {
        x <- lims[1] + (lims[2] - lims[1]) * u
        f_vals <- sapply(x, function(xi) eval(parse(text = f_expr), envir = list(x = xi)))
        mean(f_vals) * (lims[2] - lims[1])
      }
    })
    
    teorico <- tryCatch({
      integrate(function(x) eval(parse(text = f_expr), envir = list(x = x)), lower = lims[1], upper = lims[2])$value
    }, error = function(e) NA)
    
    data.frame(N = secuencia,
               Aproximacion = aprox,
               Teorico = rep(teorico, length(aprox))) %>%
      pivot_longer(cols = c("Aproximacion", "Teorico"), names_to = "Metodo", values_to = "Valor") %>%
      ggplot(aes(x = N, y = Valor, color = Metodo, linetype = Metodo)) +
      geom_line(size = 1) +
      labs(title = paste("Aproximación Monte Carlo "),
           x = "Número de puntos aleatorios",
           y = "Aproximación de la integral") +
      theme_minimal()
  })
}