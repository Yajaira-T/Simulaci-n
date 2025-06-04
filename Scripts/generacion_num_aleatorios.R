# generacion de numeros aleatorios
set.seed(12345) #fijar la semilla


g<- function(u){
  return (exp(exp(u)))
}

res<-sapply(seq(100, 1000000, by=2000), function(k){
  U<-runif(n=k)
  mean(g(U))
})
plot(seq(100, 1000000, by=2000), res, main="Valor de aproximación",
     xlab="Cantidad de números aleatorios", ylab= "Aproximación",
     type="l", col="red")

#Aproximacion integral y grafico
func<-"sqrt(1-x^2)"
res<- sapply(seq(100, 10000, by=100), function(k){
  mean(sapply(runif(k),function(x){ eval(parse(text = func))}))
})
 #sapply es como un for
pi/4

f<- function(x) eval(parse(text= func))
val<-integrate(f, lower=0, upper=1)$value
plot(seq_along(xvals), res)


