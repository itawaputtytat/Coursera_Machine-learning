predict <- function (theta, X) {
  
  ## Number of training examples
  m <- nrow(X)
  
  p <- matrix(0, 
              nrow = m,
              ncol = 1)
  
  p <- sigmoid(X %*% theta)
  p[p < 0.5] <- 0
  p[p >= 0.5] <- 1
  
  return (p)
  
}