
library(shiny)
library(kableExtra)
#función que genra numeros aleatorios bajo el método congruencial
random_cong <-function(a, m, x0, n){
  res<-numeric(n+1)
  res[1]<-x0
  for(k in 2:length(res)){
    res[k]<-(a*res[k-1] )%% m
  }
  return(res[-1]) # para que no me devuelva la semilla
}
random_cong(a=5, x0=3, m=7, n=6)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$tabla <- function(){
    
    res <<-data.table(Valor=random_cong(a=input$constante, m=input$divisor, x0= input$semilla, n=input$num)/input$divisor)
    res[,n:=1:nrow(res)]
    res<-res[, c("n", "Valor"), with= FALSE]
    
    kbl(res, booktabs = TRUE, escape=FALSE) %>% 
      kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
      row_spec(0, background = "#1D3889", color = "#ffffff")
    
  }

}
