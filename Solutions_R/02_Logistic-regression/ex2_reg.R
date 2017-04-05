# Preparatory settings ----------------------------------------------------

#wdReset()

## Set new working directory
#dir_ex  <- "02_Logistic regression"
#setwd(file.path(getwd(), dir_ex))

## Call functions
source("plotData.R")
source("costFunctionReg.R")
source("mapFeature.R")
source("plotDecisionBoundary.R")
source("predict.R")
source("sigmoid.R")

graphics.off()

cat("\n\n")



# Data preparation --------------------------------------------------------

cat("Data preparation ... ")

## Load ex1data1.txt
## Column 1: Microchip test 1
## Column 2: Microchip test 2
## Column 3: Acception (1) vs. rejection (0)
data <- read.table("ex2data2.txt", header = F, sep = ",")

## For testing purposes of only one feature
#data <- data[, c(2,3)]

X <- as.matrix(data[, 1:(ncol(data)-1)])
y <- as.matrix(data[, ncol(data)])

## Remember number of features and cases
m <- nrow(X)
n <- ncol(X)

cat("Done!\n\n")



# Plotting ----------------------------------------------------------------

cat("Plotting data ... \n\n")

cat(paste("with ... \n", 
          "+ indicating (y = 1) examples and \n",
          "o indicating (y = 0) examples",
          sep = ""))

plotData(X, y)


pauseAndContinue()



# Regularized logistic regression -----------------------------------------

## Add Polynomial Features

## Note that mapFeature also adds a column of ones, 
## so the intercept term is handled
X <- mapFeature(data[, 1], data[, 2])

## Initialize fitting parameters
initial_theta <- matrix(0, nrow = ncol(X), ncol = 1)

## Set regularization parameter lambda to 1
lambda <- 100

## Compute and display initial cost 
## and gradient for regularized logistic regression
cost <- (costFunctionReg(initial_theta, X, y, lambda))$J
grad <- (costFunctionReg(initial_theta, X, y, lambda))$grad

cat(paste("Cost at initial theta (zeros): \n",
          cost,
          "\n\n",
          "Gradient at initial theta (zeros): \n",
          paste(grad, collapse = "\n"),
          sep = ""))


#pauseAndContinue()



# Regularization and accuracies -------------------------------------------

## Run optim to obtain the optimal theta
## Equivalent to fminunc
## fminunc uses unconstrained minimzation
## Use methods: L-BFGS-B, Nelder-Mead or BFGS
## http://stackoverflow.com/questions/7920590/what-is-the-r-equivalent-of-matlabs-fminunc-function
optim_result <- 
  optim(par = initial_theta, 
        fn = function(t) costFunctionReg(t, X, y, lambda)$J,
        gr = function(t) costFunctionReg(t, X, y, lambda)$grad,
        method = c("L-BFGS-B"),
## "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
        control = c(maxit = 400))

theta <- as.vector(optim_result$par)
cost  <- optim_result$value

## Plot boundary
plotDecisionBoundary(theta, X, y)

## Compute accuracy on our training set
p <- predict(theta, X)

cat("Train Accuracy: \n")
cat(paste(mean(as.double(p == y)) * 100,
          " %"))


pauseAndContinue()


## Plot 
cat("\n")
cat("Plotting data ... \n\n")

## Visualise correct and false predictions
p_pos <- which(p == 1)
p_neg <- which(p == 0)

true_pos <- p_pos[which(p == 1) %in% which(y == 1)]
points(x = X[true_pos, 2], y = X[true_pos, 3], pch = 3, col = "green3")

false_pos <- p_pos[!which(p == 1) %in% which(y == 1)]
points(x = X[false_pos, 2], y = X[false_pos, 3], pch = 3, col = "red")

true_neg <- p_neg[which(p == 0) %in% which(y == 0)]
points(x = X[true_neg, 2], y = X[true_neg, 3], pch = 21, col = "green3")

false_neg <- p_neg[!which(p == 0) %in% which(y == 0)]
points(x = X[false_neg, 2], y = X[false_neg, 3], pch = 21, col = "red")

## Visualise positive-predictions and negative-predictions
# points(x = X[p == 1, 2], y = X[p == 1, 3], pch = 3,  col = "green3")
# points(x = X[p == 0, 2], y = X[p == 0, 3], pch = 21, col = "red")