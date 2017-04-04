# Machine Learning Online Class - Exercise 1: Linear Regression

# Instructions ------------------------------------------------------------

#  This file contains code that helps you get started on the
#  linear exercise. You will need to complete the following functions 
#  in this exericse:
#
#     warmUpExercise.m
#     plotData.m
#     gradientDescent.m
#     computeCost.m
#     gradientDescentMulti.m
#     computeCostMulti.m
#     featureNormalize.m
#     normalEqn.m
#
#  For this exercise, you will not need to change any code in this file,
#  or any other files other than those mentioned above.
#
# x refers to the population size in 10,000s
# y refers to the profit in $10,000s



# Resources ---------------------------------------------------------------

dir <- "01_Linear-regression"
library(puttytat4R)
source("plotData.R")
source("computeCost.R")
source("gradientDescent.R")



# Part 1: Basic function --------------------------------------------------

# Complete warmUpExercise.m 

cat("Running warmUpExercise ... \n")
cat("5x5 Identity Matrix: \n")

warmUpExercise()

pauseAndContinue()



# Plotting ----------------------------------------------------------------

cat("Plotting Data ... \n")

data <- read.table("ex1data1.txt", header = F, sep = ",")
X <- data[, 1]
y <- data[, 2]

m <- length(y) # Number of training examples

# Plot Data
# Note: You have to complete the code in plotData.m
plotData(X, y)

pauseAndContinue()



# Gradient descent --------------------------------------------------------

cat("Running Gradient Descent ... \n")

X <- cbind(1, X)  # Add a column of ones to x
theta <- c(0, 0)  # Initialize fitting parameters

# Some gradient descent settings
iterations <- 1500
#alpha <- 0.01
alpha <- 0.02

# Compute and display initial cost
J <- computeCost(X, y, theta)

# Run gradient descent
theta <- gradientDescent(X, y, theta, alpha, iterations)$theta

# Print theta to screen
cat("Theta found by gradient descent: \n")
cat(theta[1], theta[2], sep = "\n")
cat("\n")

# Plot the linear fit
lines(X[, 2], X %*% theta, col = "blue")
legend("bottomright",
       c("Training data", 
         "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))

# Predict values for population sizes of 35,000 and 70,000
predict1 <- c(1, 35000) %*% theta
cat("For population = 35,000, we predict a profit of: \n") 
cat(predict1, "\n")
predict2 <- c(1, 70000) %*% theta
cat("For population = 70,000, we predict a profit of: \n") 
cat(predict2, "\n")



# Visualising J(theta_0, theta_1) -----------------------------------------

cat('Visualizing J(theta_0, theta_1) ...\n')

# Grid over which we will calculate J
theta0_vals = seq(-10, 10, length.out = 100)
theta1_vals = seq( -1,  4, length.out = 100)

# initialize J_vals to a matrix of 0's
J_vals <- 
  matrix(0,
         ncol = length(theta0_vals),
         nrow = length(theta1_vals))

# Fill out J_vals
for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], 
           theta1_vals[j])
    J_vals[i,j] <- computeCost(X, y, t)
  }
}

## Surface plot
## Workaround for plot colors similar to matlab
source("etc/matlabColours.R") 
## For each example value pair of theta_0 and theta_1 (see above)
## ... the corresponding values from cost function are displayed
require(rgl)
theta_history <- gdoutput$theta_history
persp3d(x = theta0_vals,
        y = theta1_vals,
        z = J_vals,
        color = col_vals_surface,
        xlab = "theta_0",
        ylab = "theta_1")

# Contour plot
dev.new()
contour(theta0_vals,
        theta1_vals,
        J_vals,
        levels = 10^seq(-2, 3, length.out = 20),
        drawlabels = F,
        col =  col_vals_contour)
points(theta[1], 
       theta[2], 
       pch = 4, 
       col = "red")