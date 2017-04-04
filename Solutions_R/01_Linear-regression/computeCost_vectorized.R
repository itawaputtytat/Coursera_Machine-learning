computeCost <- function(X, y, theta) {
  
  ## Number of training example
  m <- length(y)
  
  ## Set initial cost to zero
  J <- 0

  ## Vectorized implementation (using matrix * vector multiplication)
  ## Cost function
  ## = 1/(2 * number of training examples) * Sum of squared deviations
  J <- 1/(2*m) * sum((X %*% theta - y)^2)
  
  return (J)
}
