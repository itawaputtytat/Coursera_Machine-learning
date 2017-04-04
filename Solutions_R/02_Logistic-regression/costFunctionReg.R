costFunctionReg <- function(theta, X, y, lambda) {
  
  ## Number of training examples
  m <- length(y)
  
  ## Initialization
  J <- 0
  grad <- rep(0, length(theta))
  
  J <- 1/m * sum(-y * log( sigmoid(X %*% theta) ) - 
                    (1 - y) * log( 1 - sigmoid(X %*% theta)) ) 
  
  J <- J + lambda/(2*m) * sum(theta^2)
  
  grad0 <- 1/m * t(X) %*% ( sigmoid(X %*% theta) - y )
  grad1 <- 1/m * t(X) %*% ( sigmoid(X %*% theta) - y ) + lambda/m * theta
  grad <- c(grad0[1], grad1[-1])
  #return(J)
   return(list(J = J,
               grad = grad))
}