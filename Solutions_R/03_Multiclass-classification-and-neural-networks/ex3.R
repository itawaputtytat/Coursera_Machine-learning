# Preparatory settings ----------------------------------------------------

wdReset()

## Set new working directory
dir_ex  <- "03_Multiclass classification and neural networks"
setwd(file.path(getwd(), dir_ex))

## Call functions
source("lrCostFunction.R") # logistic regression cost function
source("oneVsAll.R")
source("predictOneVsAll.R")
source("predit.R")

graphics.off()

## Package for reading Matlab-Files
library(R.matlab)

## Package for equivalent to Matlab's randperm
library(pracma)

## 20x20 Input images of digits
input_layer_size <- 400

# 10 labens, from 1 to 10
num_labels <- 10
## (note that we have mapped "0" to label 10)

cat("\n\n")



# Data preparation --------------------------------------------------------

## Load training da
cat('Loading and Visualizing Data ...\n')

## Training data stored in arrays X, y
data <- readMat("ex3data1.mat") 
X <- data$X
m = nrow(X)

# Randomly select 100 data points to display
rand_indices = randperm(m)
sel = X[rand_indices[1:100], ]

displayData(sel)


cat("Done!\n\n")

pauseAndContinue()



# Vectorize logistic regression -------------------------------------------


#  %  In this part of the exercise, you will reuse your logistic regression
#%  code from the last exercise. You task here is to make sure that your
#%  regularized logistic regression implementation is vectorized. After
#%  that, you will implement one-vs-all classification for the handwritten
#%  digit dataset.
#%

cat("Training One-vs-All Logistic Regression...\n")

lambda <- 0.1
[all_theta] = oneVsAll(X, y, num_labels, lambda);


cat("Done!\n\n")

pauseAndContinue()



# Predict for One-Vs-All --------------------------------------------------

pred <- predictOneVsAll(all_theta, X)

cat("Training Set Accuracy: \n")
cat(paste(mean(as.double(pred == y)) * 100, " %"))

