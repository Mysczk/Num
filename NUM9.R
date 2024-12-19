rm(list = ls())

bisekce <- function(f, a , b){
  fa <- f(a)
  fb <- f(b)
  if(fa*fb<0){
    repeat{
      c <- (a+b)/2
      if(c==a || c==b){return(c)}
      fc <- f(c)
      if((fa * fc) < 0){
        b <- c
        fb <- fc
      }else{
        a <- c
        fa <- fc
      }
    }
  }else{
    stop("Spatne vstupni zadani")
    return(FALSE)
  }
}


print(bisekce(function(x) x*x-2, 0, 2))


A<- matrix(0, 50, 50)
A[1,] <- 100
for(k in 1:1000){
  for(i in 2:49) {
    for(j in 2:49){
      A[i,j]<-0.25*(A[i-1,j]+A[i+1,j]+A[i,j-1]+A[i,j+1])
    }
  }
}
library(ggplot2)
library(scales)
ggplot(as.data.frame(as.table(A)),aes(Var1,Var2,fill=Freq))+geom_tile()+scale_fill_gradient(low = "blue", high = "red")
print(A[25,25])
n <- 48*48
A <- matrix(0,n,n)
b <- numeric(n)
for(i in 2:49) {
  for(j in 2:49){
    k <- (i-2)*48+j-1
    A[k,k]<- 4
    if(i>2) A[k,k-48] <- -1
    if(i==2) b[k] <- 100
    if(j>2) A[k,k-1] <- -1
    if(i<49) A[k,k+48] <- -1
    if(j<49) A[k,k+1] <- -1
  }
}
x<-solve(A,b)
print(x[(i-2)*48+j-1])