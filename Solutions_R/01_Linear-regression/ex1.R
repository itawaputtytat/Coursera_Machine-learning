
# Preparatory settings ----------------------------------------------------

wdReset()


## Set new working directory
dir_ex  <- "01_Linear regression"
setwd(file.path(getwd(), dir_ex))


## Call functions
source("plotData.R")
source("computeCost.R")
source("gradientDescent.R")

## Call function with vectorized implentations
#source("gradientDescent_vectorized.R")
#source("computeCost_vectorized.R")


graphics.off()

cat("\n\n")



# Data preparation --------------------------------------------------------

cat("Data preparation ... ")

## Load ex1data1.txt
## Column 1: Population of a city
## Column 2: Profit of a foot truck
data <- read.table("ex1data1.txt", header = F, sep = ",")

X <- data[, 1]
y <- data[, 2]


## Number of training examples
m <- length(y)

cat("Done!\n\n")



# Plotting ----------------------------------------------------------------

cat("Plotting data ... ")

plotData(X, y)

cat("Done!\n\n")

pauseAndContinue()



# Gradient descent --------------------------------------------------------

cat("Running gradient descent ...")


## Stop time
ptm <- proc.time()


## Accommodate theta_zero intercept term
X <- cbind(1, X) 


## Initialize fitting parameters
theta <- c(0, 0) 


## Some gradient descent settings
iterations <- 1500
alpha <- 0.01


## Compute and display initial cost
J <- computeCost(X, y, theta)
cat("Initial cost:")
cat(J)
cat("\n\n")


## Run gradient descent
gradientDescent_call <- gradientDescent(X, y, theta, alpha, iterations)


## Get thetas
theta <- gradientDescent_call$theta

cat("Theta found by gradient descent: \n")
cat(theta[1], theta[2], sep = "\n")
cat("\n\n")


## Plot the linear fit
lines(X[, 2], X %*% theta, col = "blue")
legend("bottomright",
       c("Training data", "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))


## Predict values for population sizes of 35,000 and 70,000
predict1 <- c(1, 35000) %*% theta
cat("For population = 35,000, we predict a profit of: \n") 
cat(predict1)
cat("\n\n")

predict2 <- c(1, 70000) %*% theta
cat("For population = 70,000, we predict a profit of: \n") 
cat(predict2)
cat("\n\n")

ptm <- round((proc.time()-ptm)[3], 2)
cat(paste("Elapsed time: ", ptm, " s", sep = ""))

pauseAndContinue()



# Visualizing J(theta_0, theta_1) -----------------------------------------

cat("Visualizing J(theta_0, theta_1) ...")


## Grid over which we will calculate J
## Will create example values for thetas
## e.g. values from - 10 to 10 for theta_1, and -1 to 4 for theta_1
theta0_vals = seq(-10, 10, length.out = 100)
theta1_vals = seq( -1,  4, length.out = 100)


## Initialize J_vals to a matrix of 0's
## Will create a 100x100 matrix, see length.out of grid above
J_vals <- 
  matrix(0,
         ncol = length(theta0_vals),
         nrow = length(theta1_vals))


## Fill out J_vals
## For each theta_0 (i) and each theta_1 (j) 
## ... compute J (cost function for chosen thetas)
## ... and save to J_vals matrix
for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], theta1_vals[j])
    J_vals[i,j] <- computeCost(X, y, t)
  }
}


## Workaround for plot colors similar to matlab
source("matlabColours.R") 


## Surface plot
## For each example value pair of theta_0 and theta_1 (see above)
## ... the corresponding values from cost function are displayed
require(rgl)
persp3d(x = theta0_vals,
        y = theta1_vals,
        z = J_vals,
        color = col_vals_surface,
        xlab = "theta_0",
        ylab = "theta_1")


## Contour plot
## Shows (similar to surface plot) the cost distribution 
## ... for theta_0 and theta_1 from birds view
dev.new()
contour(theta0_vals,
        theta1_vals,
        J_vals,
        levels = 10^seq(-2, 3, length.out = 20),
        drawlabels = F,
        col =  col_vals_contour)


## Add theta history to contour plot
## Shows the trace of cost for each computed theta
## ... in each itereation of gradient descent function
theta_history <- gradientDescent_call$theta_history
points(theta_history[, 1], 
       theta_history[, 2], 
       pch = 4, 
       col = "orange")
points(theta[1], 
       theta[2], 
       pch = 4, 
       col = "red")

pauseAndContinue()



# Additional visualisations -----------------------------------------------

cat("Plots for J_history... ")


## Plot J_history for iterations
## Shows decreasing cost values over all iterations
## ... until converging to a minimum
J_history <- gradientDescent_call$J_history
dev.new()
plot(J_history,
     xlab = "Number if terations",
     ylab = "Cost J",
     type = "l",
     main = "Development of cost J in iterations")


## Plot J_history for theta_0 and theta_1 over all iterations
dev.new()
par(mfrow = c(1, 2))

plot(x = c(1:iterations),
     y = gradientDescent_call$j_history[, 1],
     type = "l",
     xlab = "Number of iterations",
     ylab = "Cost j for theta 0",
     col = "green")
plot(x = c(1:iterations),
     y = gradientDescent_call$j_history[, 2],
     type = "l",
     xlab = "Number if terations",
     ylab = "Cost j for theta 1",
     col = "green")
title("Development of cost j for theta 0 and theta 1", 
      outer = T,
      line = -2)


## Plot developement of values found theta_0 and theta_0
dev.new()
par(mfrow = c(1, 2))

plot(x = c(1:iterations),
     y = theta_history[, 1],
     type = "l",
     xlab = "Number if terations",
     ylab = "theta 0",
     col = "green")
plot(x = c(1:iterations),
     y = theta_history[, 2],
     type = "l",
     xlab = "Number if terations",
     ylab = "theta 1",
     col = "green")
title("Development of theta 0 and theta 1", 
      outer = T,
      line = -2)

