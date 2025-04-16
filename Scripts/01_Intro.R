#Primer script del curso
#Configuraciones iniciales
179/5378641
options(scipen = 999) #eliminar notacion cientifica 
options (digits = 2) # que salga con 4 digitos 

#Instalacion de paquetes
install.packages("data.table")
install.packages("tidyverse", dependencies =TRUE)
library(data.table)
library(tidyverse)

#Rprofile
if(!file.exists("~/.Rprofile")){
  file.create("~/.Rprofile")
} else {
  file.edit("~/.Rprofile")
}

#Vectores

c(2,5,8,12)
sum(c(2,5,8,12))
mean(c(2,5,8,12))

x <-c(2,5,8,12)
sum(x)
mean(x)


