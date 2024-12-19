rm(list = ls())

Horner <-function(coef, x){
  n<-length(coef)
  res <- coef[n]
  if(n>1){
    for(i in (n-1):1){
      res <- res * x + coef[i]
      }  
  }
  return(res)
}

HornerNewton <-function(coef, x){
  n<-length(coef)
  y <- coef[n]
  if(n>1){
    yd <- y
    if(n>2){
      for(i in (n-1):2){
        y <- y * x + coef[i]
        yd <- yd * x + y
      }  
    }
    y <- y * x + coef[1]
    return(c(y, yd))
  }
  return(c(y,0))
}

polx<- function(coef,x){
  n <- lenght(coef)
  y <- coef[n]
  yd <- 0
  for (i in 1:n) {
    y <- y + coef[i]*x^(i-2)
    yd <- yd 
  }
}


NewtonScheme <- function(f, fd, x0, tol=0.0000001){
  x <- x0
  repeat{
    dx <- f(x)/fd(x)
    if(abs(dx)<tol){
      return(x)
    }
    x <- x - dx
  }
}

NewtonHorner <- function(a, x0, tol=0.0000001){
  x <- x0
  repeat{
    res <- HornerNewton(a,x)
    dx <- res[1]/res[2]
    if(abs(dx) < tol) return(x)
    x <- x - dx
  }
}
ChebishevCoef <- function(n){
  a0 <- numeric(n)
  a0[1] <- 1
  if(n==1) return(a0)
  a1 <- numeric(n)
  a1[2] <- 1
  if(n==2) return(a1)
  for(i in 3:n){
    a <- 2*c(0,a1[-n])-a0
    a0 <- a1
    a1 <- a
    }
  return(a)
}


n <- 2
x <- 2
coef <- runif(n)
#coef<- c(1,3,-4)
Horner(coef,x)
HornerNewton(coef,x)
x <- seq(0,1,0.0001)
plot(x, cos(x), ylim = c(0,1), type='l', col='red')
lines(x,x)
inters <- NewtonScheme(f = function(x) cos(x)-x, fd = function(x) -sin(x)-1, 0, tol=0.0000001)
points(inters,cos(inters),col='blue',cex=2)

n <- 5
NewtonHorner(c(-2,0,1), 2)
a <- ChebishevCoef(n)
x <- seq(-1,1,0.0001)
for (i in 1:n) {
  lines(x, sapply(x, function(x) HornerNewton(ChebishevCoef(i),x)[1]), ylim = c(-1,1), col=i+1)  
}

 