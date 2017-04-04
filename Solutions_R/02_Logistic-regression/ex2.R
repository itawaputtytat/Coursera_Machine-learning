# Preparatory settings ----------------------------------------------------

wdReset()

## Set new working directory
dir_ex  <- "02_Logistic regression"
setwd(file.path(getwd(), dir_ex))

## Call functions
source("plotData.R")
source("costFunction.R")
source("plotDecisionBoundary.R")
source("predict.R")
source("sigmoid.R")

graphics.off()

cat("\n\n")



# Data preparation --------------------------------------------------------

cat("Data preparation ... ")

## Load ex1data1.txt
## Column 1: Score on exam 1
## Column 2: Score on exam 2
## Column 3: Admission decision
data <- read.table("ex2data1.txt", header = F, sep = ",")

## For testing purposes of only one feature
# data <- data[, c(2,3)]

## (X is defined flexible for testing purposes: see above)
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


## Setup the data matrix appropriately
## ... and add ones for the intercept

## Add intercept term to X
X <- cbind(1, X)

## Initialize fitting parameters
## (zero for each feature and intercept)
initial_theta <- matrix(0, nrow = n + 1, ncol = 1)

## Compute and display initial cost and gradient
cost <- costFunction(initial_theta, X, y)$J
grad <- costFunction(initial_theta, X, y)$grad

cat(paste("Cost at initial theta (zeros): \n",
          cost,
          "\n\n",
          "Gradient at initial theta (zeros): \n",
          paste(grad, collapse = "\n"),
          sep = ""))


pauseAndContinue()



# Optimizing using optim (equivalent to Octave fminunc) -------------------

## Run optim to obtain the optimal theta
## Equivalent to fminunc
## fminunc uses unconstrained minimzation
## Use methods: L-BFGS-B, Nelder-Mead or BFGS
## http://stackoverflow.com/questions/7920590/what-is-the-r-equivalent-of-matlabs-fminunc-function
optim_result <- 
  optim(par = initial_theta, 
        fn = function(t) costFunctionReg(t, X, y, lambda)$J,
        gr = function(t) costFunctionReg(t, X, y, lambda)$grad,
        method = c("Nelder-Mead"),
## "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
        control = c(maxit = 400))

theta <- as.vector(optim_result$par)
cost  <- optim_result$value

## Print theta to screen
cat(paste("Cost at initial theta (zeros): \n",
          cost,
          "\n\n",
          "theta: \n",
          paste(theta, collapse = "\n"),
          sep = ""))

## Plot boundary
plotDecisionBoundary(theta, X, y)


pauseAndContinue()



# Predict and accuracies --------------------------------------------------

if(ncol(data) == 2) {
  cat("Error message due to shortened number of features \n")
  cat("(see line: data <- data[, c(2,3)] )")
}

prob <- sigmoid(theta %*% c(1, 45, 85))

cat(paste("For a student with scores 45 and 85 \n",
          "we predict an admission probability of \n",
          prob,
          sep = ""))
cat("\n\n")

## Compute accuracy on our training set
p <- predict(theta, X)

cat("Train Accuracy: \n")
cat(paste(mean(as.double(p == y)) * 100, " %"))


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