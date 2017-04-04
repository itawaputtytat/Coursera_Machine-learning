# Set working directions
setwd("F:/Copy/exercises_current/Coursera/Machine learning/01_Linear regression/R")
source("computeCost_vectorized.R")

runs = 10
cat("Unvectorized vs. vectorized implementation \n")
cat(paste("Time comparison in ", runs, " runs", sep =""))
cat("\n\n")


# Data preparation --------------------------------------------------------

# Load ex1data1.txt
data <- read.table("ex1data1.txt", header = F, sep = ",")
X <- data[, 1]
X <- cbind(1, X) # Accommodate theta_zero intercept term
y <- data[, 2]

# Number of training examples
m <- length(y)

# Some gradient descent settings
num_iters <- 1500
alpha <- 0.01

# Comparison --------------------------------------------------------------

# Unvectorized implementation ---------------------------------------------

cat("Unvectorized implementation ... \n")

ptm <- proc.time()
for(r in 1:runs) {
  theta <- c(0, 0) # Initialize fitting parameters
  theta_history <- cbind(theta_0 = rep(0, num_iters),
                         theta_1 = rep(0, num_iters))
  J_history <- rep(0, num_iters)

  for(iter in 1:num_iters) {

    # Show progress of iterations up to 100%
    progress <- 100/num_iters * iter
    flush.console(); cat(paste(round(progress, 2), " % \r"))

    # Cost for theta 0
    j0 <- 0
    for(i in 1:m) {
      j0 <- j0 + (theta[1] * X[i, 1] + theta[2] * X[i, 2] - y[i])
    }
    j0 <- 1/m * j0

    # Cost for theta 1
    j1 <- 0
    for(i in 1:m) {
      j1 <- j1 + (theta[1] * X[i, 1] + theta[2] * X[i, 2] - y[i]) * X[i, 2]
    }
    j1 <- 1/m * j1

    # Adjust thetas
    theta <- c(theta[1] - alpha * j0,
               theta[2] - alpha * j1)

    # Save thetas for visualization
    J_history[iter] <- computeCost(X, y, theta)
    theta_history[iter, ] <- theta

  } # iter
}
cat(paste("thetas = ", theta, sep = ""))
cat(paste("\n elapsed time: ", (proc.time()-ptm)[3], sep = ""))


readline("Program paused. Press enter to continue.")


# Vectorized implementation -----------------------------------------------

cat("Vectorized implementation ... \n")
ptm <- proc.time()
for(r in 1:runs) {
  theta <- c(0, 0) # Initialize fitting parameters
  theta_history <- cbind(theta_0 = rep(0, num_iters),
                         theta_1 = rep(0, num_iters))
  J_history <- rep(0, num_iters)
  for(iter in 1:num_iters) {

    # Show progress of iterations up to 100%
    progress <- 100/num_iters * iter
    flush.console(); cat(paste(round(progress, 2), " % \r"))

    # vectorized implementation
    theta <- theta - alpha * 1/m * t(X) %*% (X %*% theta - y)

    J_history[iter] <- computeCost(X, y, theta)
    theta_history[iter, ] <- theta
  }
}
cat(paste("thetas = ", theta, sep = ""))
cat(paste("\n elapsed time: ", (proc.time()-ptm)[3], sep = ""))
