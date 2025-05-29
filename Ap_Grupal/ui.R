library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(shinycssloaders)
# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel(HTML("<h1 style='font-family: Poppins; font-weight:bold; color: #1F4E79;'>Números Aleatorios - Aproximación de Integrales</h1>")),
  
  tabsetPanel(
    tabPanel("Números Aleatorios",
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 sliderInput("semilla",
                             "Ingrese un valor inicial:",
                             min=1,
                             max=500,
                             value = 30),
                 sliderInput("divisor",
                             "Ingrese un valor de m:",
                             min=5,
                             max=500,
                             value = 37),
                 sliderInput("constante",
                             "Ingrese un valor de a:",
                             min=10,
                             max=500,
                             value = 123),
                 sliderInput("c",
                             "Ingrese un valor de c para el método mixto:",
                             min=1,
                             max=500,
                             value = 200),
                 sliderInput("num",
                             "Cantidad de números a generar:",
                             min=10,
                             max=200,
                             value = 30),
                 actionButton("mostrar", "Mostrar resultados",class = "btn-danger",style = "color : #F0FFF0; background-color: #EE0000; border-color: #F0FFF0", icon("eye"))
               ),
               # 
               # Show a plot of the generated distribution
               mainPanel(
                 conditionalPanel(condition = "input.mostrar!=0",
                                  h4("Tabla de resultados - Metodo congruencial multiplicativo:"),
                                  br(), #generar una linea en blanco
                                  tableOutput("tabla"),
                                  br(),
                                  h4("Tabla de resultados - Metodo congruencial mixto:"),
                                  br(),
                                  tableOutput("tabla2"),
                                  br(),
                                  h4("Gráfico:"),
                                  fluidRow(
                                    column(width=2,
                                           numericInput("barras", "Número de barras:", value=10, min=2, max=50),
                                    ),
                                    column(width=5,
                                           h5("Congruencial Multiplicativo"),
                                           plotOutput("distPlot")
                                           
                                    ),
                                    column(width=5,
                                           h5("Congruencial Mixto"),
                                           plotOutput("HistBin")
                                    )
                                  )
                 )
               )
             )
    ),
    tabPanel("Integrales",
             sidebarLayout(
               sidebarPanel(
                 textInput("funcion", "Ingrese la funcion a integrar:", value = "1-x"),
                 numericInput("lim_inf", "Límite inferior del intervalo:", value = 0),
                 numericInput("lim_sup", "Límite superior del intervalo:", value = 1),
                 radioButtons("metodo", "Seleccione el metodo para generar los numeros aleatorios:",
                              c("Congruencial Multiplicativo", "Congruencial Mixto")),
                 actionButton("calcular", "Calcular Área",class = "btn-lg btn-success", style = "color : #F0FFF0; background-color: #9ACD32; border-color: #8B8B83", icon("chart-area"))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 conditionalPanel( condition = "input.calcular!=0", #Boton tiene por defecto valor de 0
                                   h4("Gráfica de la funcion a integrar:"),
                                   plotOutput("graf_fun01"),
                                   h4("Aproximaciòn:"),
                                   plotOutput("graf_aprox01")
                 )
               )
             )
    ),
    tabPanel("Distribuciones discretas",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("distribucion", "Seleccione distribución:",
                              c("Binomial", "Poisson")),
                 actionButton("siguiente", "Seleccionar", class = "btn-primary"),
                 
                 # Panel de parámetros
                 uiOutput("parametrosUI")
               ),
               mainPanel(
                 # Resultados para Binomial
                 conditionalPanel(
                   condition = "input.distribucion == 'Binomial' & input.siguiente > 0 & input.calc_bin > 0",
                   h4("Resultados - Distribución Binomial"),
                   withSpinner(plotOutput("histBin"),type = 3, color = "#007bff",, color.background = "#ffffff"),
                   verbatimTextOutput("statsBin")
                 ),
                 
                 # Resultados para Poisson (CORREGIDO)
                 conditionalPanel(
                   condition = "input.distribucion == 'Poisson' & input.siguiente > 0 & input.calc_pois > 0",
                   h4("Resultados - Distribución Poisson"),
                   plotOutput("histPois"),
                   verbatimTextOutput("statsPois")
                 ),
                 
                 # Mensaje cuando no hay resultados
                 conditionalPanel(
                   condition = "(input.distribucion == 'Poisson' & input.siguiente > 0 & input.calc_pois == 0) | 
                           (input.distribucion == 'Binomial' & input.siguiente > 0 & input.calc_bin == 0)",
                   wellPanel(
                     h4("Instrucciones:"),
                     p("Por favor ingrese los parámetros y presione 'Calcular' para ver los resultados")
                   )
                 )
               )
             )
    )
  ),
  hr(),
  div("Autor: Ivan Fuertes", style = "text-align: center; font-size: 13px; color: #666;")
)

