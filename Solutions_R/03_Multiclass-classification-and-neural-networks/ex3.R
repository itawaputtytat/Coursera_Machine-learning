## Machine Learning Online Class - Exercise 3 | Part 1: One-vs-all

# Instructions ------------------------------------------------------------

# This file contains code that helps you get started on the
# linear exercise. You will need to complete the following functions 
# in this exericse:
#
#   lrCostFunction.m (logistic regression cost function)
#   oneVsAll.m
#   predictOneVsAll.m
#   predict.m
#
# For this exercise, you will not need to change any code in this file,
# or any other files other than those mentioned above.

## Setup the parameters you will use for this part of the exercise
input_layer_size <- 400  # 20x20 Input Images of Digits
num_labels <- 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)

source("displayData.R")
source("lrCostFunction.R")
source("oneVsAll.R")
source("predictOneVsAll.R")



# Part 1: Loading and visualizing data ------------------------------------

#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.

# Load Training Data
cat('Loading and Visualizing Data ...\n')

# Training data stored in arrays X, y
library(R.matlab) ## Package for reading Matlab-Files
dat <- readMat("ex3data1.mat")

X <- dat$X
y <- dat$y

m <- nrow(X)

# Randomly select 100 data points to display
#rand_indices <- sample(1:m)
library(pracma) ## Package for equivalent to Matlab's randperm
rand_indices = randperm(m)
sel <- X[rand_indices[1:100], ]

displayData(sel)

pauseAndContinue()



# Part 2: Vectorize logistic regression -----------------------------------

# In this part of the exercise, you will reuse your logistic regression
# code from the last exercise. You task here is to make sure that your
# regularized logistic regression implementation is vectorized. After
# that, you will implement one-vs-all classification for the handwritten
# digit dataset.

cat("Training One-vs-All Logistic Regression ... \n")

lambda <- 0.1
all_theta <- oneVsAll(X, y, num_labels, lambda)

pauseAndContinue()



# Part 3: Predict for One-Vs-All ------------------------------------------

# After ...
pred <- predictOneVsAll(all_theta, X)

cat("Training set accuracy: \n", 
    sum(pred == y)/nrow(pred) * 100)

