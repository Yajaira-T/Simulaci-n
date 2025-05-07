#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(data.table) #sugerencia
#carga de datos
load("./data/Info (1).RData")

#Ajuste de formato fecha
datos[, fecha_corte:=ymd(fecha_corte)]
datos[, fecha_nacimiento:=ymd(fecha_nacimiento)]
#Tamaño del grafico
tamaños <- list(
  Pequeno = list(width = "300px", height = "300px"),
  Mediano = list(width = "400px", height = "400px"),
  Grande = list(width = "500px", height = "500px")
  
)



# Define server logic required to draw a histogram
function(input, output, session) {
    
  #filtro de variables por su tipo
  tvar<- datos |> map_chr(class)
  tvar<- data.table(Variable= names(tvar), Tipo= unname(tvar))
  
  observe({
    updateSelectInput(session, "var", choices= tvar[Tipo==input$tipo])
  } )
  output$resumen <- renderPrint({
    datos %>% dplyr:: select(input$var) %>%  summary(.)
  })
  output$grafico_ui <- renderUI({
    req(input$tamaño)  # “pequeno”, “mediano” o “grande”
    tam <- tamaños[[ input$tamaño ]]
    plotOutput(
      "grafico",
      width  = paste0(tam$width),
      height = paste0(tam$height)
    )
  })
  
  
  output$grafico <- renderPlot({
    if(input$tipo=="numeric"){
      datos %>% dplyr:: select(input$var) %>% pull(.) %>% hist(col = input$barras, border = input$borde, main = paste("Histograma de", input$var), xlab   = input$var, ylab   = "Frecuencia") #hist para histograma
     
    } else{
      datos %>% dplyr:: select(input$var) %>% table(.) %>% barplot(col = input$barras, border = input$borde, main = paste("Histograma de", input$var), xlab   = input$var, ylab   = "Frecuencia") #table para contar
    }
    
  })


}
