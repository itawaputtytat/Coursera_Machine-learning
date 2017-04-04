# Function that returns the 5x5 identity matrix

warmUpExercise <- function() {
  
  A <- c()
  
  # ============= YOUR CODE HERE ==============
  # Instructions: Return the 5x5 identity matrix 
  
  # A <- matrix(0, nrow = 5, ncol = 5)
  # for(i in 1:nrow(A))
  #   A[i, i] <- 1
  A <- diag(5)
  print(A)
  
  return(A)
  
  # ===========================================
  
}