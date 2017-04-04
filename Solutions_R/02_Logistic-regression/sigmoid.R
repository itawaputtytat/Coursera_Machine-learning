sigmoid <- function(z) {
  
  z <- as.matrix(z)
  g <- matrix(0, 
              nrow = nrow(z),
              ncol = ncol(z))
  g <- 1 / (1 + exp(-z))
  
  return (g)
  
}