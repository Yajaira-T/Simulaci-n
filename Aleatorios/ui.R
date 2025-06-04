library(shiny)
library(kableExtra)
library(shinycssloaders)  

fluidPage(
  tags$head(
    tags$style(HTML("
  .panel-color {
    background-color: #f8f9fa; /* gris muy claro */
    color: #2c3e50; /* azul grisáceo profesional */
    padding: 20px;
    border-radius: 12px;
    border: 1px solid #dee2e6;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
  }

  .btn-eye {
    background-color: #5c6bc0; /* azul profesional suave */
    color: white;
    font-weight: 500;
    border: none;
    padding: 10px 18px;
    border-radius: 8px;
    transition: background-color 0.3s ease;
  }

  .btn-eye:hover {
    background-color: #3f51b5; /* azul un poco más oscuro al pasar el mouse */
  }

  body {
    background-color: #e9ecef; /* fondo general más suave */
  }
"))
    
    
  ),
  
  titlePanel("Generación de números aleatorios - Método congruencial"),
  
  tabsetPanel(
    tabPanel("Números Aleatorios",
             sidebarLayout(
               sidebarPanel(
                 div(class = "panel-color",
                     numericInput("semilla", "Ingrese un valor inicial:", value = 30, min = 1),
                     numericInput("divisor", "Ingrese el valor de m:", value = 37, min = 1),
                     numericInput("constante", "Ingrese el valor de a:", value = 123, min = 1),
                     numericInput("incremento", " Ingrese el valor de c (constante del método congruencial mixto):", value = 17, min = 0),
                     numericInput("num", "Cantidad de números a generar:", value = 30, min = 1, max = 10000),
                     br(),
                     actionButton("mostrar", "Mostrar resultados", icon = icon("eye"), class = "btn-eye")
                 )
               ),
               mainPanel(
                 conditionalPanel(condition = "input.mostrar!=0",
                                  h4("Tabla de resultados - Método Congruencial Multiplicativo:"),
                                  withSpinner(tableOutput("tabla_mult"), type = 6, color = "#007BFF"),
                                  br(),
                                  h4("Tabla de resultados - Método Congruencial Mixto:"),
                                  withSpinner(tableOutput("tabla_mixto"), type = 6, color = "#FF5733"),
                                  br(),
                                  h4("Histogramas:"),
                                  fluidRow(
                                    column(6, withSpinner(plotOutput("hist_mult"), type = 4)),
                                    column(6, withSpinner(plotOutput("hist_mixto"), type = 4))
                                  )
                 )
               )
             )
    ),
    
    tabPanel("Integrales",
             sidebarLayout(
               sidebarPanel(
                 textInput("funcion", "Ingrese la función a integrar:", value = "1-x"),
                 checkboxInput("usar_inf"," ¿Incluir infinito en los límites?", FALSE),
                 conditionalPanel(
                   condition = "!input.usar_inf",
                   numericInput("lim_inf", "Límite inferior:", value = 0),
                   numericInput("lim_sup", "Límite superior:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.usar_inf",
                   selectInput("lim_inf_inf", "Límite inferior:", choices = c("−∞" = "-Inf", "0" = "0", "5" = "5")),
                   selectInput("lim_sup_inf", "Límite superior:", choices = c("+∞" = "Inf", "1" = "1"))
                 ),
                 radioButtons("metodo", "Seleccione el método para generar los números aleatorios:", 
                              c("Congruencial Multiplicativo", "Congruencial Mixto")),
                 actionButton("calcular", "Calcular Área", style = "color: #FFFFFF; background-color: #F5426C; border-color: #9932CC")
               ),
               mainPanel(
                 conditionalPanel(condition = "input.calcular!=0",
                                  h4("Gráfica de la función a integrar:"),
                                  withSpinner(plotOutput("graf_fun01"), type = 6),
                                  h4("Aproximación:"),
                                  withSpinner(plotOutput("graf_aprox01"), type = 6)
                 )
               )
             )
    )
  )
)