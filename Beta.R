"
@Author : Lopez Pedraza Aaron
@Date   : 08-10-2020
@Version: 1.0

Código que simula variables aleatorias a traves del metodo de aceptacion y rechazo.
En particular simulamos una variable aleatoria BETA con determindos parametros.

"


sim_acept_rechazo <- function() {
  c <- 0.4 / 0.25
  p_j <- c(0.2, 0.15, 0.25, 0.4)
  prop <- floor(4 * runif(1)) + 1
  while(runif(1) > p_j[prop] / (c * 0.25)){
    prop <- floor(4 * runif(1)) + 1
  }
  prop
}
sims <- rerun(1000, sim_acept_rechazo()) %>% flatten_dbl()
100 * prop.table(table(sims))
#> sims
#>    1    2    3    4 
#> 21.9 15.3 24.4 38.4

#---------2


rz1<-function () 
{
  repeat {
    x <- runif(1, 0, 1)
    y <- runif(1, 0, 3/2)
    fx <- 6 * x * (1 - x)
    if (y < fx) 
      return(x)
  }
}



rz3<-function (n) 
{
  xvec <- runif(n, 0, 1)
  yvec <- runif(n, 0, 3/2)
  fvec <- 6 * xvec * (1 - xvec)
  xvec[yvec < fvec]
}

rz3(100)




############################3 GAMA(2,2)

# We are first going to look at a simple example of rejection sampling 
# of the random variable Z which has pdf f(z)=6z(1-z) on [0,1]. 
# Note that Z has a Beta(2,2) distribution. 
# Also note that f(z) has a maximum of 3/2 at 1/2. 
# So to simulate Z we can simulate X from Unif(0,1) and Y from Unif(0,3/2) 
# and accept X if Y<f(X).


#La distribucion esta acotada en x & y
f_z <- function(z){(6*z)*(1-z)}
dom_f <- seq(0,1,0.000001)#simulamos el dominio
length(dom_f)

#primero observamos que Z~Gamma(2,2)
alfa <- 2
beta <- 2
esperanza_gamma <- alfa*beta
desvi_est_gamma <- sqrt(alfa*beta^2)#algunas medidas

#La visualizamos
rango <- seq(0,5*desvi_est_gamma,0.01)
y <- dgamma(rango,alfa,rate = 1/beta)
plot(rango,y,type = 'l', ylim = c(0,max(y))+.02)
#points(3/2,max(y),col='red')
# line(0:14,max(y),col= 'blue')
abline(h=max(y),col = 'blue')


#LA QUE VAMOS A SIMULAR:

rango2 <- seq(0,desvi_est_gamma/2,0.01)
length(rango2)
y2 <- dgamma(rango2,alfa,rate = 1/beta)
plot(rango2,y2,type = 'l', ylim = c(0,max(y))+.02)
#points(3/2,max(y),col='red')
# line(0:14,max(y),col= 'blue')
abline(h=max(y),col = 'blue')



rz3<-function (n) 
{
  xvec <- runif(n, 0, 1)#generamos los dos numero aleatorios
  yvec <- runif(n, 0, max(y2))
  #fvec <- 6 * xvec * (1 - xvec)
  f_z(xvec)#evaluamos f en ese punto
  t <- xvec[yvec < f_z]#extraemos los valores que cumplen esta codicion
  return(t)
}


rz4<-function (n) {
  t #los que cumplieron
  tamanio <- length(x)#cuantos son
  aprob <- tamanio/n #la proporcion de las que SI cumplieron
  diferenca <- n - tamanio#cantidad de los que NO cumplieron 
  n2 <- round(diferenca/aprob)
  x2 <- rz3(n2)#recalculamos
  x3 <- c(x, x2)#extraemos en vector la simulacion
  return(x3)
}



rz4(1000)







# plot(0:14,rz4(1000))

hist(rz4(100000))
# hist(rz4(10000),30)
barplot(rz4(1000))






#######version 3----------------

f_z <- function(z){(6*z)*(1-z)}
dom_f <- seq(0,1,0.000001)#simulamos el dominio
length(dom_f)


#primero observamos que Z~Gamma(2,2)
alfa <- 2
beta <- 2
esperanza_gamma <- alfa*beta
desvi_est_gamma <- sqrt(alfa*beta^2)#algunas medidas

#La visualizamos
rango <- seq(0,5*desvi_est_gamma,0.01)
y <- dgamma(rango,alfa,rate = 1/beta)
plot(rango,y,type = 'l', ylim = c(0,max(y))+.02)
#points(3/2,max(y),col='red')
# line(0:14,max(y),col= 'blue')
abline(h=max(y),col = 'blue') #imagen acotada


#LA QUE VAMOS A SIMULAR es: f(z) con Dominio en [0,1]

rango2 <- seq(0,desvi_est_gamma/2,0.01)
length(rango2)
y2 <- dgamma(rango2,alfa,rate = 1/beta)
plot(rango2,y2,type = 'l', ylim = c(0,max(y))+.02)
#points(3/2,max(y),col='red')
# line(0:14,max(y),col= 'blue')
abline(h=max(y),col = 'blue')


#################################-----SIMULAMOS-------

simulacion_rechazo_beta <- function(n){
  
  simulacion <- vector('numeric',length = n)
  x2 <- vector('numeric',length = n)
  r1 <- runif(n, 0, 1)#generamos los dos numero aleatorios n veces
  r2 <- runif(n, 0, max(y2))
  
for (i in length(n)) {#iteramos n veces
  
  x2[i] <- 0+(1-0)*r1[i] #garantizamos que la VA este totalmente acotada
  
  f_z(x2[i])#evaluamos
  
  if (r2[i]<(f_z(x2[i])/max(y2))) {#ahora si la condicion de acepatcion o rechazo
    simulacion[i] <- x2[i]
  }
  
}
  return(x2)#en case de ser cierto, almacenamos
}


simulacion_rechazo_beta(1000)














# Visualizamos a  Beta ----------------------------------------------------


p = seq(0,1, length=100)
plot(p, dbeta(p, 100, 100), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 10, 10), type ="l", col=3)
lines(p, dbeta(p, 2, 2), col=2) 
lines(p, dbeta(p, 1, 1), col=1) 
legend(0.7,8, c("Be(100,100)","Be(10,10)","Be(2,2)", "Be(1,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))


rz3<-function (n){
  xvec <- runif(n, 0, 1)          #generamos n numeros aleatorios [0,1]
  yvec <- runif(n, 0, 3/2)        #generamos n numeros aleatorios [0,3/2]
  fvec <- 6 * xvec * (1 - xvec)   #evaluamos
  xvec[yvec < fvec]               #estos son los que cumplen la desigualdad
}


rz4<-function (n) 
{
  x <- rz3(n)                 #los elementos que cumplieron la desigualdad
  len <- length(x)            #la cantidad que son
  aprob <- len/n              #la proporcion de las que SI cumplieron
  shortby <- n - len          #cantidad de los que NO cumplieron
  n2 <- round(shortby/aprob)  #la proporcion de las diferencias
  x2 <- rz3(n2)               #sobre esos calculamos
  x3 <- c(x, x2)              #vector de la simulacion
  x3
}


# Aplicamos la funcion ----------------------------------------------------

rz4(1000)

#visualizamos las simulaciones en un histograma
hist(rz4(100000))

#distribucion original
p = seq(0,1, length=100)                                    #el rango
plot(p, dbeta(p, 2, 2), ylab="density", type ="l", col=4)   #la grafica
