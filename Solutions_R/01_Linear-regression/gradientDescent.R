gradientDescent <- function(X, y, theta, alpha, num_iters) {

  ## Number of training examples
  m <- length(y) 
  
  ## Prepare collector for J (overall cost) and j (cost for each theta)
  J_history <- rep(0, num_iters)
  j_history <- cbind(j_history_theta0 = rep(0, num_iters),
                     j_history_theta1 = rep(0, num_iters))
  
  ## Prepare collector for theta
  theta_history <- cbind(theta_0 = rep(0, num_iters),
                         theta_1 = rep(0, num_iters))

  
  ## Run/Iterations
  for(iter in 1:num_iters) {

    ## Show progress of iterations up to 100%
    progress <- 100/num_iters * iter
    setTxtProgressBar(progressBar, progress)

    
    ## Cost for theta 0:
    #-------------------
    ## Reset cost to zero 
    j0 <- 0
    for(i in 1:m) {
      j0 <- j0 + (theta[1] * X[i, 1] + theta[2] * X[i, 2] - y[i])
    }
    ## Complete cost function
    j0 <- 1/m * j0

    
    ## Cost for theta 1:
    #-------------------
    ## Reset cost to zero
    j1 <- 0
    for(i in 1:m) {
      j1 <- j1 + (theta[1] * X[i, 1] + theta[2] * X[i, 2] - y[i]) * X[i, 2]
    }
    ## Complete cost function
    j1 <- 1/m * j1

    
    ## Adjust thetas with alpha weighted cost
    theta <- c(theta[1] - alpha * j0,
               theta[2] - alpha * j1)

    
    ## Save J and j for visualization
    J_history[iter] <- computeCost(X, y, theta)
    j_history[iter, ] <- c(j0, j1)
    
    
    ## Save theta for visualisation
    theta_history[iter, ] <- theta
    
  } # End of iterations
  
  cat("\n\n") ## For breaks under progess bar
  
  return(list(theta = theta,
              theta_history = theta_history,
              J_history = J_history,
              j_history = j_history))

}