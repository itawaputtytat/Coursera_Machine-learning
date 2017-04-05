# DISPLAYDATA Display 2D data in a nice grid
# [h, display_array] = DISPLAYDATA(X, example_width) displays 2D data
# stored in X in a nice grid. It returns the figure handle h and the 
# displayed array if requested.

displayData <- function(X, example_width = NA) {
  
  outputFunProc(R)

  # Set example_width automatically if not passed in
  if (is.na(example_width))
    example_width = round( sqrt(ncol(X)) )
  
  # Gray Image
  #colormap(gray);
  
  # Compute rows, cols
  m <- nrow(X)
  n <- ncol(X)
  example_height = (n / example_width)
  
  # Compute number of items to display
  display_rows = floor(sqrt(m))
  display_cols = ceiling(m / display_rows)
  cat("Display rows:", display_rows, "\n")
  cat("Display cols:", display_cols, "\n")
  
  # Between images padding
  pad = 1
  
  # Setup blank display
  display_array <- 
    matrix(1, 
           pad + display_rows * (example_height + pad),
           pad + display_cols * (example_width + pad))
  
  # Copy each example into a patch on the display array
  curr_ex <- 1
  for(j in 1:display_rows) {
    for(i in 1:display_cols) {
      if (curr_ex > m) 
        break
    
      # Copy the patch
      # Get the max value of the patch
      max_val <- max( abs(X[curr_ex, ]) )
      row_selector <- pad + (j - 1) * (example_height + pad) + (1:example_height)
      col_selector <- pad + (i - 1) * (example_width + pad) + (1:example_width)
      display_array[row_selector, col_selector] <- 
        matrix(X[curr_ex, ], nrow = example_height) / max_val
      curr_ex <- curr_ex + 1
      #cat(matrix(X[curr_ex, ], nrow = example_height) / max_val, "\n\n")
    }
    if (curr_ex > m)
      break
  }


  # Display Image
  image(t(display_array)[, nrow(display_array):1], axes = F, col = gray.colors(100))
  plotdat <- recordPlot()
  plotdat

  return(list(h = plotdat, example_width))
}
