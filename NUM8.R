fder <- function(f, x, hmax, n = 1){
  h <- hmax/2^(0:(n-1))
  res <- (f(x+h)-f(x-h))/(2*h)
  if(n>1){
    for (i in 1:(n-1)) {
      m <- n-i+1
      power <- 4^i
      res <- (power*res[2:m]-res[1:(m-1)])/(power-1)
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


for(x in 0:10) {
  cat(exp(x), fder(exp,x,0.001, 1),"\n")
}
print("///////////////////////////////////")

Tr <- 0.89
f <- ((8*Tr)/(3*Vr-1))-(3/Vr^2)

print(exp(1)-1)
MidPointRule(exp,0,1,235)
MidPointRuleRicharson(exp,0,1,3)

MidPointRuleRicharson(function(x) ((8*x)/(3*x-1))-(3/x^2),0.5,2.5,3)

plot(function(x) ((8*Tr)/(3*x-1))-(3/x^2)-0.6,xlim = c(0.5,3), ylim=c(-1,2))
abline(a=0,b=0)

