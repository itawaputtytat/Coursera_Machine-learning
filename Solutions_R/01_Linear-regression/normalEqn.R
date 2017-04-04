
# Objective ---------------------------------------------------------------
## Computes the closed-form solution to linear regression
## ... using the normal equation

normalEqn <- function(X, y) {
  
  theta <- rep(0, ncol(X))
  
  require(MASS)
  #theta <- ginv(t(X) %*% X) %*% t(X) %*% y
  theta <- solve(t(X) %*% X) %*% t(X) %*% y
  
  return(theta)
  
}

