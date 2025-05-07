

library(shiny)
library(kableExtra)
# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Generación de números aleatorios - Método congruencial"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("semilla",
                        "Ingrese un valor inicial:",
                        min=0,
                        value = 30),
            numericInput("divisor",
                        "Ingrese un valor de m:",
                        min=0,
                        value = 30),
            numericInput("constante",
                        "Ingrese un valor de a:",
                        min=0,
                        value = 30),
            numericInput("num",
                        "Cantidad de números a generar:",
                        min=0,
                        value = 30),
        ),
# 
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("tabla")
        )
    )
)
