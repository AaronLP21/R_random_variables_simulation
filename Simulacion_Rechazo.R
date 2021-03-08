"

Programa que simula a traves del metodo de aceptacion y rechazo la distribucion 
de una variable aleatoria X~Beta(2,2)

En un principio, X debe estar totalmente acotada, lo cual, este caso lo cumple.
Primero observamos la distribucion 'teorica', despues aplicamos el algoritmo
y por ultimo visualizamos el histograma de las simulaciones.

@Author  : Aaron Lopez Pedraza
@Version : v1.0
@Date    : 03/07/2020
"


# Visualizamos la  distribucion Beta ----------------------------------------------------

p = seq(0,1, length=100)                                    #el rango
plot(p, dbeta(p, 2, 2),main = 'X~Beta(2,2)', xlab = 'n',
     ylab="densidad", type ="l", col=4)                     #la grafica


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
hist(rz4(100000),main = 'Histograma de X~Simulada',col = '4',xlab = 'n',
     ylab = 'Frequencias')

#distribucion teorica
p = seq(0,1, length=100)                                    #el rango
plot(p, dbeta(p, 2, 2),main = 'X~Beta(2,2)', xlab = 'n',
     ylab="densidad", type ="l", col=4)                     #la grafica


#Podemos ver la similitud de la simulacion