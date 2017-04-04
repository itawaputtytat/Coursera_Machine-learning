computeCost <- function(X, y, theta) {
  
  ## Number of training examples
  m <- length(y) 
  
  ## Set initial cost to zero
  J <- 0 # 
  
  ## For each training example: Compute cost (incrementally)
  ## Theta * Feature value + ... - Outcome)^2
  ## ... = squared deviations
  for(i in 1:m) {
    J <- J + (theta[1]*X[i, 1] + theta[2] * X[i, 2] - y[i])^2
  }
  
  ## Complete cost function
  ## = 1/(2 * number of training examples) * Sum of squared deviations
  J = 1/(2*m) * J
  
  return(J)
}
