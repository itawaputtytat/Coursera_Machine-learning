
## Normalizes the features in x
## Function returns a normalized version X where
## The mean value of each feature is 0 and the standard deviation is 1

featureNormalize <- function(X) {
  
  ## Need to be set correctly
  X_norm <- X
  
  ## Declare values for feature means (mu) and standard deviations (sigma)
  mu <- rep(0, ncol(X))
  sigma <- rep(0, ncol(X))
  
  ## Compute means and standard deviations
  mu <- sapply(X, mean)
  sigma <- sapply(X, sd)
  
  ## For each feature:
  ## Compute normalized values (implicit: for each row)
  for(feature in 1:ncol(X_norm)) {
    X_norm[, feature] <- (X[, feature] - mu[feature]) / sigma[feature]
  }

  return(list(X_norm = X_norm, 
              mu = mu, 
              sigma = sigma))  
}