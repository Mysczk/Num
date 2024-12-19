rm(list=ls())
GeomMethod <- function(f, a, b, h, n){
  x <- runif(n, a, b)
  y <- runif(n, 0, h)
  return(h*(b-a)*sum(f(x) > y)/n)
}
AveMethod <- function(f, a, b, n){
  return((b-a)*mean(f(runif(n,a,b))))
}

Gauss2 <- function(f, a, b){
  x<-1/sqrt(3)
  slope <- (b-a)/2
  intercept <- (b+a)/2
  return(sum(f(slope*c(x,-x)+intercept))*slope)
} 

Gauss3 <- function(f, a ,b){
  x<-sqrt(0.6)
  x <- c(-x, 0, x)
  w <- c(5, 8, 5)/9
  slope <- (b-a)/2
  intercept <- (b+a)/2
  return(sum(w*f(slope*x+intercept))*slope)
}

Gauss4 <- function(f, a ,b){
  x1 <- sqrt(3/7-sqrt(6/5)*2/7)
  x2 <- sqrt(3/7+sqrt(6/5)*2/7)
  x <- c(x1,-x1, x2, -x2)
  w1 <- c(18+sqrt(30))/36
  w2 <- c(18-sqrt(30))/36
  w <- c(w1,w1,w2,w2)
  slope <- (b-a)/2
  intercept <- (b+a)/2
  return(sum(w*f(slope*x+intercept))*slope)
}

a <- 2
b <- 7

exp(1) - exp(0)
Gauss2(exp, 0, 1)
Gauss3(exp, 0, 1)
Gauss4(exp, 0, 1)


na <- 1000

x <- replicate(na, GeomMethod(sin, 0, pi, 1, n=4000))
cat(mean(x),"+-", sd(x), "\n")


x <- replicate(na, AveMethod(sin, 0, pi, n=4000))
cat(mean(x),"+-", sd(x), "\n")

x <- replicate(na, AveMethod(function(x) (sin(x)+sin(a+b-x))/2, 0, pi, n=4000))
cat(mean(x),"+-", sd(x), "\n")


Horner <-function(coef,x){
  n<-length(coef)
  res <- coef[n]
  for(i in (n-1):1){res <- res * x + coef[i]}
  return(res)
}
Integratepolynom <- function(coef,a,b){
  n<-length(coef)
  coef <- coef/1:n
  x <- c(a,b)
  res <- coef[n]
  for(i in (n-1):1){res <- res * x + coef[i]}
  res <- res*x
  return(res[2]-res[1])
}
coef <- c(1,2,-3)
Integratepolynom(coef,a,b)
Gauss2(function(x) Horner(coef,x), a, b)
Gauss3(function(x) Horner(coef,x), a, b)
Gauss4(function(x) Horner(coef,x), a, b)