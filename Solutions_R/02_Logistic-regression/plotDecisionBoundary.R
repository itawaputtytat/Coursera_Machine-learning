plotDecisionBoundary <- function (theta, X, y) {
  
  if (n == 1) { # if (number of features)
    plotData(X, y) 
    lines(sigmoid(cbind(1, c(1:1000)) %*% theta),
          col = "blue")
  } else {
    
    # Plot data
    # plotData(x = X[, 2:ncol(X)], y = y) 
    #plotData(X, y)
    
    if (ncol(X) <= 3) { # re-compute number of features
      
      ## Only need 2 points to define a line
      ## ... so choose two endpoints
      plot_x <- c(min(X[, 2]) - 2,
                  max(X[, 2]) + 2)
      
      ## Calculate the decision boundary line
      plot_y <- (-1 / theta[3]) * (theta[2] * plot_x + theta[1])
      
      ## Plot, and ajdust axes for better viewing
      lines(x = plot_x,
            y = plot_y,
            type = "l",
            col = "blue")
      
      ## Legend, specific for the exercise
      legend("topright",
             c("Admitted", "Not admitted", "Decision boundary"),
             cex = 0.5,
             pch = c(4, 21, NA),
             lty = c(NA, NA, 1),
             col = c("black", "black", "blue"),
             pt.bg = c(NA, "yellow", NA))

    } else {
      
      ## Here is the grid range
      u = seq(-1, 1.5, length.out = 50)
      v = seq(-1, 1.5, length.out = 50)
      
      z = matrix(0,
                 nrow = length(u),
                 ncol = length(v))
      
      ## Evaluate z = theta * x over the grid
      for (i in 1:length(u)) {
        for (j in 1:length(v)) {
          
          z[i, j] <- mapFeature(u[i], v[j]) %*% theta 
          
        }
      }
      
      ## Important to transpose z before calling contour
      z <- t(z)
      
      ## Plot z = 0
      ## Notice you need to specify the range [0, 0]
      #contour(u, v, z, [0,0], "LineWidth", 2)
      contour(u, v, z, nlevel = c(0, 0), add = T, drawlabels = F, col = "green")
    }
  }
}