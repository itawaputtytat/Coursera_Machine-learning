# Data preparation --------------------------------------------------------

# load ex1data1.txt
data <- read.table(file.choose(), header = F, sep = ",")
X <- data[, 1]
y <- data[, 2]

# Number of training examples
m <- length(y)


# Explore data ------------------------------------------------------------

# Plot data
plot(X, y,
     col = "red",
     pch = 4, # symbols used for data points
     xlab = "Population of City in 10,000s",
     ylab = "Profit in $10,000s",
     xaxt = "n", # supresses automatically generated axis
     yaxt = "n",
     xlim = c(4, 24),
     ylim = c(-5, 25))
axis(side = 1, at = seq(4, 24, 2)) # adjust axis
axis(side = 2, at = seq(-5, 25, 5))


# Initialize theta parameters ---------------------------------------------

# Accommodate theta_zero intercept term
X <- cbind(1, X)

# Initialize fitting parameters
theta <- c(0, 0)


# Compute and display initial cost ----------------------------------------

iterations <- 1500
alpha <- 0.01

# Cost function
computeCost <- function(X, y, theta) {
  J <- 0
  for(i in 1:m) {
    J <- J + (theta[1]*X[i, 1] + theta[2] * X[i, 2] - y[i])^2
  }
  J = 1/(2*m) * J
  print(J)
  return(J)
}

# Cost function with thetas from initialization
computeCost(X, y, theta)
temp0 <- theta[1]
temp1 <- theta[2]

# Gradient descent
J_history <- c()
temp0_history <- c()
temp1_history <- c()

for(iter in 1:iterations) {

  # Cost for theta 0
  j0 <- 0
  for(i in 1:m) {
    j0 <- j0 + (temp0*X[i, 1] + temp1 * X[i, 2] - y[i])
  }
  j0 <- 1/m * j0

  # Cost for theta 1
  j1 <- 0
  for(i in 1:m) {
    j1 <- j1 + (temp0*X[i, 1] + temp1 * X[i, 2] - y[i]) *X[i, 2]
  }
  j1 <- 1/m * j1

  # Adjust thetas
  temp0 <- temp0 - alpha * j0
  temp1 <- temp1 - alpha * j1

  # Save thetas for visualization
  temp0_history <- c(temp0_history, temp0)
  temp1_history <- c(temp1_history, temp1)
  J_history[iter] <- computeCost(X, y, theta = c(temp0, temp1))
}

theta[1] <- temp0
theta[2] <- temp1

# Plot linear fit
lines(X[,2], X %*% theta, col = "blue")
legend("bottomright",
       c("Training data", "Linear Regression"),
       cex = 0.75,
       pch = c(4, NA),
       lty = c(NA, 1),
       col = c("red", "blue"))

# Visualize cost
theta0_vals = seq(-10, 10, length.out = 100)
theta1_vals = seq( -1,  4, length.out = 100)

J_vals = matrix(0,
                ncol = length(theta0_vals),
                nrow = length(theta1_vals))

# Fill out J_vals
for (i in 1:length(theta0_vals)) {
  for (j in 1:length(theta1_vals)) {
    t <- c(theta0_vals[i], theta1_vals[j])
    J_vals[i,j] <- computeCost(X, y, t)
  }
}

# Surface plot
library(rgl)
# Workaround for plot colors similar to matlab
jet.colors <- colorRampPalette(c("#00007F", "blue",
                                 "#007FFF", "cyan",
                                 "#7FFF7F", "yellow",
                                 "#FF7F00", "red",
                                 "#7F0000"))
colorzjet <- jet.colors(100)  # 100 separate color

col_vals <- colorzjet[ findInterval(J_vals,
                                    seq(min(J_vals),
                                        max(J_vals),
                                        length=100))]

persp3d(theta0_vals, theta1_vals, J_vals,
        color = col_vals)

# Contour plot
# Adjust color values
col_vals <- colorzjet[ findInterval(10^seq(-2, 3, length.out = 20),
                                    seq(min(10^seq(-2, 3, length.out = 20)),
                                        max(10^seq(-2, 3, length.out = 20)),
                                        length=180))]

dev.new()
contour(theta0_vals,
        theta1_vals,
        J_vals,
        levels = 10^seq(-2, 3, length.out = 20),
        drawlabels = F,
        col =  col_vals)

# Plot theta history
points(temp0_history, temp1_history, pch = 4, col = "orange")
points(temp0, temp1, pch = 4, col = "red")

# Plot cost history
dev.new(); plot(J_history)
