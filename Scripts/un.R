secuencia <- seq(100, 100000, by=1500)
sapply(secuencia, function(k){
  res<- random_cong(a=7^5, m=2^31-1, x0=as.numeric(now()), n=k) #no me sale los # entre 0 y 1
  mean( sapply(res, function(x){
    eval(parse(text = ))
  }))
})
summary(random_cong(a=7^5, m=(2^31-1), x0= as.numeric(now()), n=100))

#probar

secuencia <- seq(100, 100000, by=1500)

f<- function(x) eval(parse(text= "sqrt(1-x^2)"))

  data.table(Aproximacion=sapply(secuencia, function(k){
  res<- random_cong(a=7^5, m=2^31-1, x0=as.numeric(now()), n=k)
  mean( sapply(res, function(x){ eval(parse(text = "sqrt(1-x^2)"))}))
}),
Teorico = integrate(f, lower=0, upper=1)$value, Aleatorios= secuencia
) %>% gather(key= "Etiqueta", value= "Valor") %>% mutate(Aleatorios= c(secuencia, secuencia))%>%
    ggplot(aes(x= Aleatorios, y=Valor, group = Etiqueta, colour = Etiqueta))+geom_line() 
  
  
  
  