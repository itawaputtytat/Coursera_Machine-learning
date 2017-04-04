
# Preparatory settings ----------------------------------------------------

wdReset()


## Set new working directory
dir_ex  <- "01_Linear regression"
setwd(file.path(getwd(), dir_ex))


## Should surface plots be created?
plotSurfacePlots <- F


## Call functions
source("featureNormalize.R")
source("computeCostMulti.R")
source("gradientDescentMulti.R")
source("normalEqn.R")

graphics.off()

cat("\n\n")



# Data preparation --------------------------------------------------------

cat("Data preparation ... ")


## Load ex1data2.txt
## Column 1: Size of the house (in square feet)
## Column 2: Number of bedrooms
## Column 3: Price of the house
data <- read.table("ex1data2.txt", header = F, sep = ",")
X <- data[, 1:2]
y <- data[, 3]


## ... and visualisation
cat("\n")
cat("(and 3d scatter plot visualitions)")
require(rgl)
plot3d(X$V1, X$V2, y, 
       col = "red", 
       size = 5,
       xlab = "Size of the house",
       ylab = "Number of bedrooms",
       zlab = "Price of the house")

cat("Done!\n\n")


## Number of training examples
m <- length(y)


## Print out some data points
cat("First 10 examples from the dataset: \n\n")

print(cbind(X[1:10, ], y = y[1:10]))


pauseAndContinue()
#rgl.close() # closing plot3d device 
#graphics.off() # (otherwise error in convergence graph)


## Scale features and set them to zero mean
cat("Normalizing features ...")

mu    <- featureNormalize(X)$mu
sigma <- featureNormalize(X)$sigma
X     <- featureNormalize(X)$X_norm


## Add intercept term to X
X <- cbind(1, X)
X <- as.matrix(X) # workaround


## Print out some normalized data points
cat("\n\n")
cat("Normalized: First 10 examples from the dataset: \n\n")

print(cbind(X[1:10, 2:3], y = y[1:10]))


pauseAndContinue()



# Gradient descent --------------------------------------------------------

cat("Running gradient descent...\n\n")


## Choose some alpha values
alpha     <- 0.1

## Choose some alpha values for comparison of convergence
alpha_J   <- c(0.5, 0.25, alpha)
num_iters <- 400


## Init theta and run gradient descent:
## Initialize fitting parameters (with first alpha value)
theta <- c(0, 0, 0) 
gradientDescent_call <- 
  gradientDescent(X, y, theta, alpha, num_iters)


## Compute cost convergence for different alpha values (see above)
## Must be called here, before saving new thetas
J_history1 <- gradientDescent(X, y, theta, alpha_J[1], num_iters)$J_history
J_history2 <- gradientDescent(X, y, theta, alpha_J[2], num_iters)$J_history
J_history3 <- gradientDescent(X, y, theta, alpha_J[3], num_iters)$J_history


theta <- gradientDescent_call$theta
J_history <- gradientDescent_call$J_history

pauseAndContinue()


## Display gradient descent's result
cat("Theta computed from gradient descent: \n")
cat(theta, sep = "\n")
cat("\n")


pauseAndContinue()


# Estimate the price of a 1650 sq-ft, 3 br house
# first column of X is all-ones = no need to be normalized
price <- 0
price <- c(1, (c(1650, 3) - mu) / sigma) %*% theta
cat(paste("Predicted price of 1650 sq-ft, 3 br house ... ",
          "\n",
          "(using gradient descent): \n\n",
          price,
          sep = ""))

pauseAndContinue()



# Visualization -----------------------------------------------------------

cat("Plotting convergence graph ...")


## Plot the convergence graph
plot(x = 1:length(J_history), 
     y = J_history,
     xlab = "Number of iterations",
     ylab = "Cost J",
     type = "l")


## Plot the convergence graph for different alpha values (see above)
dev.new()
plot(x = 1:50, 
     y = J_history1[1:50],
     xlab = "Number of iterations",
     ylab = "Cost J",
     main = "Comparison of J for different alpha",
     col = "red",
     type = "l")
lines(x = 1:50,
      y = J_history2[1:50],
      col = "blue")
lines(x = 1:50,
      y = J_history3[1:50],
      col = "green")
legend("bottomright",
       c(paste("alpha = ", alpha_J, sep = "")),
       cex = 0.75,
       pch = NA,
       lty = 1,
       col = c("red", "blue", "green"))


pauseAndContinue()


## Plot the linear fit (size of house vs. price)

cat("Plotting linear fit for size of house ... \n\n")

plot(x = X[, 2] * sigma[1] + mu[1], 
     y = y,
     col = "red",
     pch = 4, # symbols used for data points
     xlab = "Size of house (square feet)",
     ylab = "Price of house")
lines(x = X[, 2] * sigma[1] + mu[1], 
      y = X[, 1:2] %*% theta[1:2], 
      col = "blue")
legend("bottomright",
       c("Training data", "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))


pauseAndContinue()


## Plotting linear fit  of number of beedrooms vs. price
## ... won't make sense, see plane here

cat("Plotting linear fit for both parameters ... \n")
cat("... does not make sense: see plane")

plot3d(data$V1, data$V2, data$V3, 
       col = "red", 
       size = 5,
       xlab = "Size of the house",
       ylab = "Number of bedrooms",
       zlab = "Price of the house")

fit <- lm(V3 ~ V1 + V2, data = data)
planes3d(fit$coefficients[2], ## Size of house
         fit$coefficients[3], ## Number of bedrooms
         -1, 
         fit$coefficients[1], ## Price of house
         alpha = 0.5)


pauseAndContinue()



# Visualizing J(theta_0, theta_1, theta_2) --------------------------------

if (plotSurfacePlots == T) {
  
  cat("Visualizing J(theta_0, theta_1, theta_2) ...\n")
  
  
  ## Grid over which we will calculate J
  theta0_vals = seq(-1000000, 1000000, length.out = 100)
  theta1_vals = seq(-1000000, 1000000, length.out = 100)
  theta2_vals = seq(  -10000,   10000, length.out = 100)
  
  
  ## Initialize J_vals to a matrix of 0's
  J_vals1 = matrix(0,
                   ncol = length(theta0_vals),
                   nrow = length(theta1_vals))
  
  J_vals2 = matrix(0,
                   ncol = length(theta0_vals),
                   nrow = length(theta2_vals))
  
  J_vals3 = matrix(0,
                   ncol = length(theta1_vals),
                   nrow = length(theta2_vals))
  
  
  ## Fill out J_vals
  for (i in 1:length(theta0_vals)) {
    for (j in 1:length(theta1_vals)) {
      t <- c(theta0_vals[i], theta1_vals[j])
      J_vals1[i,j] <- computeCost(X[, 1:2], y, t)
    }
  }
  
  for (i in 1:length(theta0_vals)) {
    for (j in 1:length(theta2_vals)) {
      t <- c(theta0_vals[i], theta2_vals[j])
      J_vals2[i,j] <- computeCost(X[, c(1,3)], y, t)
    }
  }
  
  for (i in 1:length(theta1_vals)) {
    for (j in 1:length(theta2_vals)) {
      t <- c(theta1_vals[i], theta2_vals[j])
      J_vals3[i,j] <- computeCost(X[, 2:3], y, t)
    }
  }
  
  jet.colors <- colorRampPalette(c("#00007F", "blue",
                                   "#007FFF", "cyan",
                                   "#7FFF7F", "yellow",
                                   "#FF7F00", "red",
                                   "#7F0000"))
  colorzjet <- jet.colors(100)  # 100 separate color
  
  
  ## Surface plot
  require(rgl)
  
  cat("Visualizing J(theta_0, theta_1) ...\n")
  
  col_vals_surface <-
    colorzjet[ findInterval(J_vals1,
                            seq(min(J_vals1),
                                max(J_vals1),
                                length=100))]
  
  persp3d(x = theta0_vals,
          y = theta1_vals,
          z = J_vals1,
          color = col_vals_surface,
          xlab = "theta_0",
          ylab = "theta_1")
  
  pauseAndContinue()
  
  cat("Visualizing J(theta_0, theta_2) ...\n")
  
  col_vals_surface <-
    colorzjet[findInterval(J_vals2,
                           seq(min(J_vals2),
                               max(J_vals2),
                               length = 100))]
  
  persp3d(x = theta0_vals,
          y = theta2_vals,
          z = J_vals2,
          color = col_vals_surface,
          xlab = "theta_0",
          ylab = "theta_2")
  
  pauseAndContinue()
  
  cat("Visualizing J(theta_1, theta_2) ...\n")
  
  col_vals_surface <-
    colorzjet[findInterval(J_vals3,
                           seq(min(J_vals3),
                               max(J_vals3),
                               length = 100))]
              
  persp3d(x = theta1_vals,
          y = theta2_vals,
          z = J_vals3,
          color = col_vals_surface,
          xlab = "theta_1",
          ylab = "theta_2")
  

  pauseAndContinue() 
  
}



# Normal equations --------------------------------------------------------

cat("Solving with normal equations ... \n\n")


## Load data
data <- read.table("ex1data2.txt", header = F, sep = ",")
X <- data[, 1:2]
y <- data[, 3]
m <- length(y)


## Just for testing purpose
## (What happen's if features are normalized before? >>> same result)
# X <- featureNormalize(X)$X_norm


## Add intercept term to X
X <- cbind(1, X)
X <- as.matrix(X)


## Just for comparison
#theta_old <- theta


## Calculate the parameters from the normal equation
theta <- normalEqn(X, y)


## Plot the linear fit
plot(x = X[, 2], 
     y = y,
     col = "red",
     pch = 4, # symbols used for data points
     xlab = "Size of house (square feet)",
     ylab = "Price of house")
lines(x = X[, 2], 
      y = X[, 1:2] %*% theta[1:2], col = "green")
legend("bottomright",
       c("Training data", 
         "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))


## Display normal equation's result
cat("Theta computed from the normal equations: \n")
cat(theta, sep = "\n")
cat("\n")


## Compare to results of linear model function from R
cat("Theta computed by lm(): \n")
cat(fit$coefficients, sep = "\n")
cat("\n")


## Estimate the price of a 1650 sq-ft, 3 br house
price <- 0
price <- c(1, c(1650, 3)) %*% theta
cat(paste("Predicted price of 1650 sq-ft, 3 br house ... \n",
          "(using gradient descent): \n\n",
          price,
          sep = ""))
cat("\n")