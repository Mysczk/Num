g <- function(x){
  return(sqrt((x+10))/x)
}


plot(g,xlim=c(0,5), ylim=c(0,5), col='blue')
abline(a=0,b=1, col='red') # a+bx

n<-100
x <- numeric(n)
x[1] <- 5

for (i in 2:n) {
  x[i] <- g(x[i-1])
  segments(x0=x[i-1],x1=x[i-1], y0=x[i-1],y1=x[i], col = 'green')
  segments(x0=x[i-1],x1=x[i], y0=x[i],y1=x[i], col = 'green')
#print(x)
}
# plot(x, type = 'b')

# ----

g <- function(x){
  return((x+2/x)/2)
}

plot(g,xlim=c(0,5), ylim=c(0,5), col='blue')
abline(a=0,b=1, col='red') # a+bx

n<-20
x <- numeric(n)
x[1] <- 5

for (i in 2:n) {
  x[i] <- g(x[i-1])
  segments(x0=x[i-1],x1=x[i-1], y0=x[i-1],y1=x[i], col = 'green')
  segments(x0=x[i-1],x1=x[i], y0=x[i],y1=x[i], col = 'green')
  #print(x)
}
# plot(x, type = 'b')
# -----------------------
#g <- function(y1){
#  return(y0+hpul*(f(x1,y1)+f(x0,y0)))
#}

step <- function(f, h, x0, y0){
  hpul <- 0.5*h
  x1 <- x0+h
  y1 <- y0+h*f(x0,y0)
  for (i in 1:10) {
    y1 <- y0+hpul*(f(x1,y1)+f(x0,y0))
  }
  return(y1)
}

f <- function(x,y){
  return((1+y*y)/(x*y*(1+x*x)))
}


h<- 0.01
x <- seq(1,2,h)
n<-length(x)
y<-numeric(n)
y[1] <- 1

for (i in 2:n) {
  y[i] <- step(f,h,x[i-1], y[i-1])
}
plot(x,y, col = 'blue')
lines(x, sqrt((3*x*x-1)/(1+x*x)), col = 'red')
# -------------

f <- function(x,y){
  beta <- 1
  nu <- 1
  dS <- beta*y[1]*y[2]
  dI <- nu*y[2]
  return(c(-dS,dS-dI,dI))
}

h<- 0.001
x <- seq(0,10,h)
n<-length(x)
y<-matrix(0, nrow=n,ncol=3)
y[1,] <- c(999, 1, 0)

for (i in 2:n) {
  y[i,] <- step(f,h,x[i-1], y[i-1,])
}
plot(x,y[,1], col = 'blue')
points(x, y[,2], col='red')
points(x, y[,3], col = 'green')