rm(list=ls())

AddNewtonCeoff <- function(x, y, a){
  n <- length(x)
  nasobeni <- 1
  scitani <- 0
  for(i in 1:(n-1)){
    scitani <- scitani+a[i]*nasobeni
    nasobeni <- nasobeni*(x[n]-x[i])
  }
  return((y-scitani)/nasobeni)
}
NewtonPolynomial<-function(z, a, x){
  n <- length(a)
  res <- a[n]
  if(n>1){
    for (i in (n-1):1) {
      res <- res*(z-x[i]) + a[i]
    }
  }
  return(res)
}


l<-function(xa, x, j){
  res <- 1
  n <- length(x)
  for(i in 1:n){
    if(i!=j){
      res <- res*(xa-x[i])/(x[j]-x[i])
    }
  }
  return(res)
}

lagrange<-function(xa, x, y){
  res <- 0
  n <- length(x)
  for(i in 1:n){
    res <-res + y[i] * l(xa,x,i)
  }
  return(res)
}


n <- 10
x <- 1:n
y <- sin(x)
plot(x,y)
a <- y[1]
for(i in 2:n) a[i] <- AddNewtonCeoff(x[1:i], y[i], a)


z <- seq(x[1],x[n], 0.01)
for (i in 2:n)lines(z, NewtonPolynomial(z, a[1:n], x[1:n]),col=i+1)

print(l(x[1],x,1))
plot(x,y)
lines(z,lagrange(z,x,y))