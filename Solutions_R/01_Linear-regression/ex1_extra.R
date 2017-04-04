
# Preparatory settings ----------------------------------------------------

## Misc functions
library(puttytat4R)
.outputFunProc_status(F)

## Set new working directory
wdReset()
dir  <- "01_Linear-regression"
setwd(file.path(getwd(), dir))

## Reset graphics
graphics.off()

## Load functions
source("plotData.R")
source("computeCost.R")
#source("computeCost_vectorized.R")
source("gradientDescent.R")
#source("gradientDescent_vectorized.R")



# Data preparation --------------------------------------------------------

outputSectionTitle("Data preparation")

## Load ex1data1.txt
## Column 1: Population of a city
## Column 2: Profit of a foot truck
data <- read.table("ex1data1.txt", header = F, sep = ",")

X <- data[, 1]
y <- data[, 2]

## Number of training examples
m <- length(y)

outputDone(T)



# Plotting ----------------------------------------------------------------

outputSectionTitle("Data visualisation")

plotData(X, y)

outputDone(T)

pauseAndContinue()



# Gradient descent --------------------------------------------------------

outputSectionTitle("Gradient descent")

## Stop time
ptm <- proc.time()

## Accommodate theta_zero intercept term
X <- cbind(1, X) 

## Initialize fitting parameters
#theta <- c(0, 0) 
theta <- c(8, 3) 

## Some gradient descent settings
iterations <- 1500
#alpha <- 0.01
alpha <- 0.02

## Compute and display initial cost
J <- computeCost(X, y, theta)
cat("Initial cost: \n")
cat(J, "\n\n")


## Run gradient descent
gdoutput <- gradientDescent(X, y, theta, alpha, iterations)



# Results -----------------------------------------------------------------

outputSectionTitle("Results")

## Get thetas
theta <- gdoutput$theta

cat("Theta found by gradient descent: \n")
cat(theta[1], theta[2], sep = "\n")
cat("\n\n")

## Plot the linear fit
lines(X[, 2], X %*% theta, col = "blue")
legend("bottomright",
       c("Training data", 
         "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))

## Predict values for population sizes of 35,000 and 70,000
predict1 <- c(1, 35000) %*% theta
cat("For population = 35,000, we predict a profit of: \n") 
cat(predict1, "\n\n")

predict2 <- c(1, 70000) %*% theta
cat("For population = 70,000, we predict a profit of: \n") 
cat(predict2, "\n\n")

outputProcTime(ptm)

pauseAndContinue()



# Visualizing J(theta_0, theta_1) -----------------------------------------

outputSectionTitle("Visualise J(theta_0, theta_1)")

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
    t <- c(theta0_vals[i], 
           theta1_vals[j])
    J_vals[i,j] <- computeCost(X, y, t)
  }
}

## Workaround for plot colors similar to matlab
source("etc/matlabColours.R") 

## Surface plot
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
points3d(theta_history[, 1], 
         theta_history[, 2], 
         J_history + 10, 
         col="red",size=3.5)
lines3d(theta_history[, 1],
        theta_history[, 2], 
        J_history+10, col="red")

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
points(theta_history[, 1], 
       theta_history[, 2], 
       pch = 4, 
       col = "orange")
points(theta[1], 
       theta[2], 
       pch = 4, 
       col = "red")
lines(theta_history[, 1], theta_history[, 2], col="red")

pauseAndContinue()



# Additional visualisations -----------------------------------------------

# cat("Plots for J_history... ")
# 
# ## Plot J_history for iterations
# ## Shows decreasing cost values over all iterations
# ## ... until converging to a minimum
# J_history <- gdoutput$J_history
# dev.new()
# plot(J_history,
#      xlab = "Number if terations",
#      ylab = "Cost J",
#      type = "l",
#      main = "Development of cost J in iterations")
# 
# pauseAndContinue()
# 
# ## Plot J_history for theta_0 and theta_1 over all iterations
# dev.new()
# par(mfrow = c(1, 2))
# 
# plot(x = c(1:iterations),
#      y = gdoutput$j_history[, 1],
#      type = "l",
#      xlab = "Number of iterations",
#      ylab = "Cost j for theta 0",
#      col = "green")
# plot(x = c(1:iterations),
#      y = gdoutput$j_history[, 2],
#      type = "l",
#      xlab = "Number if terations",
#      ylab = "Cost j for theta 1",
#      col = "green")
# title("Development of cost j for theta 0 and theta 1", 
#       outer = T,
#       line = -2)
# 
# pauseAndContinue()
# 
# ## Plot developement of values found theta_0 and theta_0
# dev.new()
# par(mfrow = c(1, 2))
# 
# plot(x = c(1:iterations),
#      y = theta_history[, 1],
#      type = "l",
#      xlab = "Number if terations",
#      ylab = "theta 0",
#      col = "green")
# plot(x = c(1:iterations),
#      y = theta_history[, 2],
#      type = "l",
#      xlab = "Number if terations",
#      ylab = "theta 1",
#      col = "green")
# title("Development of theta 0 and theta 1", 
#       outer = T,
#       line = -2)

