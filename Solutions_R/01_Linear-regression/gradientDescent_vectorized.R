gradientDescent <- function(X, y, theta, alpha, num_iters) {
  
  ## Number of training examples
  m <- length(y) 
  
  ## Prepare collector variable for J and theta
  J_history     <- rep(0, num_iters)
  theta_history <- matrix(0, 
                          nrow = num_iters, 
                          ncol = ncol(X))

  
  ## Iterations
  for(iter in 1:num_iters) {

    ## Show progress of iterations up to 100%
    progress <- 100/num_iters * iter
    setTxtProgressBar(progressBar100, progress)

    ## Cost for theta as vectorized implementation
    
    delta <- 1/m * t(X) %*% (X %*% theta - y)
    theta <- theta - alpha * delta

    ## Save thetas for visualization
    J_history[iter]       <- computeCost(X, y, theta)
    theta_history[iter, ] <- theta
    
  } #iter
  
  return(list(theta         = theta,
              theta_history = theta_history,
              J_history     = J_history
              ))

}
