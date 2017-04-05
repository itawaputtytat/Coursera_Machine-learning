# ONEVSALL trains multiple logistic regression classifiers and returns all
# the classifiers in a matrix all_theta, where the i-th row of all_theta 
# corresponds to the classifier for label i
# [all_theta] = ONEVSALL(X, y, num_labels, lambda) trains num_labels
# logisitc regression classifiers and returns each of these classifiers
# in a matrix all_theta, where the i-th row of all_theta corresponds 
# to the classifier for label i

oneVsAll <- function(X, y, num_labels, lambda) {

  # Some useful variables
  m <- nrow(X)
  n <- ncol(X)
  
  # You need to return the following variables correctly 
  all_theta <- matrix(0, nrow = num_labels, ncol = n + 1)
  
  # Add ones to the X data matrix
  X <- cbind(1, X)
  
  # ====================== YOUR CODE HERE ======================
  # Instructions: You should complete the following code to train num_labels
  #               logistic regression classifiers with regularization
  #               parameter lambda. 
  #
  # Hint: theta(:) will return a column vector.
  #
  # Hint: You can use y == c to obtain a vector of 1's and 0's that tell use 
  #       whether the ground truth is true/false for this class.
  #
  # Note: For this assignment, we recommend using fmincg to optimize the cost
  #       function. It is okay to use a for-loop (for c = 1:num_labels) to
  #       loop over the different classes.
  #
  #       fmincg works similarly to fminunc, but is more efficient when we
  #       are dealing with large number of parameters.
  #
  # Example Code for fmincg:
  #
  #     # Set Initial theta
  #     initial_theta = zeros(n + 1, 1);
  #     
  #     # Set options for fminunc
  #     options = optimset('GradObj', 'on', 'MaxIter', 50);
  # 
  #     # Run fmincg to obtain the optimal theta
  #     # This function will return theta and the cost 
  #     [theta] = ...
  #         fmincg (@(t)(lrCostFunction(t, X, (y == c), lambda)), ...
  #                 initial_theta, options);
  #
  
  cost <- c()
  
  for(c in 1:num_labels) {
    
    cat("Computing parameters for label:", c, "... \n")
    
    initial_theta <- rep(0, n + 1)
    
    ## Run optim to obtain the optimal theta
    ## Equivalent to fminunc
    ## fminunc uses unconstrained minimzation
    ## Use methods: L-BFGS-B, Nelder-Mead or BFGS
    ## http://stackoverflow.com/questions/7920590/what-is-the-r-equivalent-of-matlabs-fminunc-function
    optim_result <- 
      optim(par = initial_theta, 
            fn = function(t) lrCostFunction(t, X, y == c, lambda)$J,
            gr = function(t) lrCostFunction(t, X, y == c, lambda)$grad,
            #method = c("L-BFGS-B"),
            method = c("BFGS"),
            ## "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
            control = c(maxit = 400))
    # fmincg(f=costFunction, X=par, Maxiter=500)
    
    
    all_theta[c, ] <- as.vector(optim_result$par)
    cost[c]  <- optim_result$value
    
  }
  
  # =========================================================================
  
  return(list(theta = all_theta, cost = cost))

}