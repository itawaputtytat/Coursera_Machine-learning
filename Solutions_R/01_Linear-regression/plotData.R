plotData <- function(x, y) {
  plot(x, y,
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
}
