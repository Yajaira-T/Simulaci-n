library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)     # Para mejores controles
# Define UI for application that draws a histogram
fluidPage(
  # Añadir CSS personalizado
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Poppins:400,700"),
    tags$style(HTML("
      body {
        font-family: 'Poppins', sans-serif;
      }
      .well {
        background-color: #f9f9f9;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .nav-tabs>li.active>a {
        background-color: #1F4E79;
        color: white !important;
        font-weight: bold;
      }
      .btn {
        border-radius: 5px;
        transition: all 0.3s;
      }
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .loading-message {
        color: #1F4E79;
        font-weight: bold;
        font-size: 16px;
      }
    "))
  ),
  
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
                                   withSpinner(plotOutput("graf_fun01"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                  
                                   h4("Aproximaciòn:"),
                                   withSpinner( plotOutput("graf_aprox01"),  type = 6, color = "#17a2b8", color.background = "#ffffff")
                                  
                 )
               )
             )
    ),
    tabPanel("Distribuciones discretas",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("distribucion", "Seleccione distribución:",
                              c("Binomial", "Poisson", "Binomial Negativa", "Otro caso")),
                 actionButton("siguiente", "Seleccionar", class = "btn-primary"),
                 
                 # Panel de parámetros
                 uiOutput("parametrosUI")
               ),
               mainPanel(
                 # Resultados para Binomial
                 conditionalPanel(
                   condition = "input.distribucion == 'Binomial' & input.siguiente > 0 & input.calc_bin > 0",
                   h4("Resultados - Distribución Binomial"),
                   withSpinner(tableOutput("tablaBin"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   withSpinner(plotOutput("histBin"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                  
                   verbatimTextOutput("statsBin")
                 ),
                 
                 # Resultados para Poisson (CORREGIDO)
                 conditionalPanel(
                   condition = "input.distribucion == 'Poisson' & input.siguiente > 0 & input.calc_pois > 0",
                   h4("Resultados - Distribución Poisson"),
                   withSpinner(tableOutput("tablaPois"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   withSpinner(plotOutput("histPois"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   verbatimTextOutput("statsPois")
                 ),
                 
                 # Resultados para Binomial Negativa
                 conditionalPanel(
                   condition = "input.distribucion == 'Binomial Negativa' & input.siguiente > 0 & input.calc_bin_neg > 0",
                   h4("Resultados - Distribución Binomial Negativa"),
                   withSpinner(tableOutput("tablaBin_neg"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   withSpinner(plotOutput("histBin_neg"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   verbatimTextOutput("statsBin_neg")
                 ),
                 # Resultados para Otro Caso
                 conditionalPanel(
                   condition = "input.distribucion == 'Otro caso' & input.siguiente > 0 & input.calc_otro > 0",
                   h4("Resultados - Distribución Personalizada"),
                   withSpinner(tableOutput("tablaOtro"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   withSpinner(plotOutput("histOtro"),  type = 6, color = "#17a2b8", color.background = "#ffffff"),
                   verbatimTextOutput("statsOtro")
                 ),
                 # Mensaje cuando no hay resultados
                 conditionalPanel(
                   condition = "(input.distribucion == 'Poisson' & input.siguiente > 0 & input.calc_pois == 0) | 
                           (input.distribucion == 'Binomial' & input.siguiente > 0 & input.calc_bin == 0) | 
                           (input.distribucion == 'Binomial Negativa' & input.siguiente > 0 & input.calc_bin_neg == 0)|
                     (input.distribucion == 'Otro caso' & input.siguiente > 0 & input.calc_otro == 0)",
                   wellPanel(
                     h4("Instrucciones:"),
                     p("Por favor ingrese los parámetros y presione 'Calcular' para ver los resultados"), 
                     icon("info-circle", class = "fa-2x", style = "color: #1F4E79")
                   )
                 )
               )
             )
    )
  ),
  hr(),
  div("Autores: Ivan Fuertes, Yajaira Toaquiza, Kevin Apolo", style = "text-align: center; font-size: 13px; color: #666;")
)
