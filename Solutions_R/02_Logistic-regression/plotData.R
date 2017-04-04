plotData <- function(x, y) {
  
  neg <- y == 0
  pos <- y == 1
  
  col4theta0 <- unique(x[, 1])
  if (length(col4theta0) == 1)
    col4theta0 <- T else
      col4theta0 <- F

  
  if (ncol(x) == 1)
    plot(x = x, y = y) else {
      
      if (ncol(x) == 2 & col4theta0 == T) 
        plot(x = X[, 2], y = y) else {
          
          plot(x = x[neg, 1], 
               y = x[neg, 2],
               pch = 21, # symbols used for data points
               bg = "yellow")
               #xlab = "Exam 1 score",
               #ylab = "Exam 2 score",
               #xaxt = "n", # supresses automatically generated axis
               #yaxt = "n",
               #xlim = c(30, 100),
               #ylim = c(30, 100))
          #axis(side = 1, at = seq(30, 100, 10)) # adjust axis
          #axis(side = 2, at = seq(30, 100, 10))
          
          points(x = x[pos, 1],
                 y = x[pos, 2],
                 pch = 3)
          
          legend("topright",
                 #c("admitted", "not admitted"),
                 c("0", ""),
                 cex = 0.75,
                 pch = c(21, 3),
                 pt.bg = c("yellow", NA))
  
    }
  }
}
