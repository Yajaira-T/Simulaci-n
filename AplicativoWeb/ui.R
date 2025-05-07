#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
#Tamaño del grafico
tamaños <- list(
  Pequeno = list(width = "300px", height = "300px"),
  Mediano = list(width = "400px", height = "400px"),
  Grande = list(width = "500px", height = "500px")
  
)

# Define UI for application that draws a histogram
fluidPage(
    tags$style("h1{color: #9A9A9A; font=size: 35px}"),
    tags$style("h2{color: #A569BD; font=size: 30px}"),
  fluidRow(column(width=3, tags$img(src="EPN_logo_big.png", width="60px", height="60px")),
           column(width=9,h1( "Primer aplicativo Simulación", style="text-align:center"))), 
    # Application title
    #titlePanel("Primer aplicativo simulación"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("tipo", "Seleccione el tipo de variables a analizar",choiceNames =  c("Cuantitativas", "Cualitativas"),
                         choiceValues = c("numeric", "character"), selected="numeric"),
          selectInput("var", "Variable seleccionada", choices = NULL), #sugerencia
            
           #selectInput("var", "Variable seleccionada", choices = tvar$Variable, selected = tvar$Variable[2]),
           selectInput("barras", "Seleccione el color de las barras",
                       choices = c(
                         "Coral" = "#FF7F50",
                         "Verde pálido" = "#98FB98",
                         "Aguamarina" = "#7FFFD4",
                         "Trigo" = "#F5DEB3",
                         "Rosa claro" = "#FFB6C1",
                         "Ciruela" = "#DDA0DD",
                         "Aceituna" = "#808000",
                         "Turquesa" = "#40E0D0"
                       )
           ),
           
           selectInput("borde", "Seleccione el color del borde", 
                       choices=c(
                         "Rojo oscuro"="#8B0000",
                         "Violeta medio rojo"="#C71585",
                         "Índigo"="#4B0082",
                         "Azul medianoche"="#191970")),
           
           radioButtons("tamaño", "Seleccione el tamaño del gráfico:", choiceNames  = c("Pequeño", "Mediano", "Grande"), choiceValues = names(tamaños), selected = "Mediano")
        #)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Resumen:"),
           verbatimTextOutput("resumen"),
           h2("Gráfico"),
           uiOutput("grafico_ui")
           #plotOutput("gráfico", width="350px")
        )
    )
)
