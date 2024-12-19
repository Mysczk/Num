rm(list = ls())

Gauss <- function(A, b){
  # přímý chod
  Ab <- cbind(A, b)
  n <- length(b)
  for(k in 1:(n-1)){
    for(i in (k+1):n){
      j <- (k+1):(n+1)
      nasobek <- - Ab[i,k]/Ab[k,k]
      Ab[i,j] <- Ab[i,j] + nasobek*Ab[k,j]
    }
  }
  # zpětný chod
  x <- b
  x[n] <- Ab[n,n+1]/Ab[n,n]
  for(i in (n-1):1){
    j <- (i+1):n
    x[i] <- (Ab[i,n+1] - sum(Ab[i,j]*x[j]))/Ab[i,i]
  }
  return(x)
}


GaussPivot <- function(A, b){
  # přímý chod
  Ab <- cbind(A, b)
  n <- length(b)
  for(k in 1:(n-1)){
    pivot <- which.max(abs(Ab[(k:n),k])) + k - 1
    if(pivot != k){
      j<- k:(n+1)
      pom <- Ab[k,j]
      Ab[k,j] <- Ab[pivot,j]
      Ab[pivot,j] <- pom  
    }
    for(i in (k+1):n){
      j <- (k+1):(n+1)
      nasobek <- - Ab[i,k]/Ab[k,k]
      Ab[i,j] <- Ab[i,j] + nasobek*Ab[k,j]
    }
  }
  # zpětný chod
  x <- b
  x[n] <- Ab[n,n+1]/Ab[n,n]
  for(i in (n-1):1){
    j <- (i+1):n
    x[i] <- (Ab[i,n+1] - sum(Ab[i,j]*x[j]))/Ab[i,i]
  }
  return(x)
}

GetLU <- function(A){
  n <- nrow(A)
  
  for(k in 1:(n-1)){
    for(i in (k+1):n){
      j <- (k+1):n
      nasobek <- - A[i,k]/A[k,k]
      A[i,j] <- A[i,j] + nasobek*A[k,j]
      A[j,k] <- nasobek
      }
  }
  return(A)
}

SolveLU <- function(LU, b){
  # Ly = b
  y <- b
  for(i in 2:n){
    j <- 1:(i-1)
    y[i] <- b[i] - sum(LU[i,j]*x[j])
  }
  
  # Ux = y
  x <- y
  x[n] <- y[n,n+1]/LU[n,n]
  for(i in (n-1):1){
    j <- (i+1):n
    x[i] <- (y[i,n+1] - sum(LU[i,j]*x[j]))/LU[i,i]
  }
  return(x)
}


n <- 5
A <- matrix(runif(n*n),nrow = n)
b <- runif(n)
x <- solve(A, b)
print(x)
print(Gauss(A, b))
print(GaussPivot(A, b))

LU <- GetLU(A)
print(LU)
print(SolveLU(LU,b))
