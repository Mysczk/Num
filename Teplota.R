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

MidPointRule <- function(f, a, b, n = 1){
  h <- (b-a)/n
  return(h*sum(f(a+h*(1:n)-h*0.5)))
}

MidPointRuleRicharson <- function(f, a, b, n = 1){
  res <- sapply(2^(0:(n-1)), function(n) MidPointRule(f, a, b, n))
  if(n>1){
    for (i in 1:(n-1)) {
      m <- n-i+1
      power <- 4^i
      res <- (power*res[2:m]-res[1:(m-1)])/(power-1)
    }
  }
  return(res)
}

f <- function(Tr){
  return(((8*Tr)/(3*Vr-1))-(3/Vr^2))
}



n <- 2
Tr <- 0.89
coef <- runif(n)
x <- seq(0,1,0.0001)
coefs <- Horner(coef, x)

res <- MidPointRuleRicharson(function(x) ((8*Tr)/(3*x-1))-(3/x^2)-0.6, coefs[1], coefs[2],n)
plot(function(x) ((8*Tr)/(3*x-1))-(3/x^2)-0.6,xlim = c(0.5,3), ylim=c(-1,2))
abline(a=0,b=0)
points(res)
