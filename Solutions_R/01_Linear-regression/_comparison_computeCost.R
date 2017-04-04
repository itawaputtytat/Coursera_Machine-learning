runs = 10000
cat("Unvectorized vs. vectorized implementation \n")
cat(paste("Time comparison in ", runs, " runs", sep =""))
cat("\n\n")

cat("Unvectorized implementation ... \n")
ptm <- proc.time()
for(r in 1:runs) { 
  m <- length(y) # number of training examples
  J <- 0
  for(i in 1:m) {
    J <- J + (theta[1]*X[i, 1] + theta[2] * X[i, 2] - y[i])^2
  }
  J = 1/(2*m) * J
}
cat(paste("J = ", J, sep = ""))
cat(paste("\n elapsed time: ", (proc.time()-ptm)[3], sep = ""))


readline("Program paused. Press enter to continue.")


cat("Vectorized implementation ... \n")
ptm <- proc.time()
for(r in 1:runs) {
  m <- length(y) # number of training examples
  J <- 0

  # vectorized implementation
  J <- 1/(2*m) * sum((X %*% theta - y) ^ 2)
}
cat(paste("J = ", J, sep = ""))
cat(paste("\n elapsed time: ", (proc.time()-ptm)[3], sep = ""))