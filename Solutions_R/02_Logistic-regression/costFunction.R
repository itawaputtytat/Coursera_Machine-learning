costFunction <- function(theta, X, y) {
  
  ## Number of training examples
  m <- length(y)
  
  ## Initialization
  J <- 0
  grad <- rep(0, length(theta))
  
  J <- 1/m * sum(-y * log( sigmoid(X %*% theta) ) - 
                    (1 - y) * log( 1 - sigmoid(X %*% theta)) ) 
  
  grad <- 1/m * t(X) %*% ( sigmoid(X %*% theta) - y )
  #return(J)
   return(list(J = J,
               grad = grad))
}