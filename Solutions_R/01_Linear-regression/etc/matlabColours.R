jet.colors <- 
  colorRampPalette(c(
    "#00007F", "blue",
    "#007FFF", "cyan",
    "#7FFF7F", "yellow",
    "#FF7F00", "red",
    "#7F0000"
  ))

colorzjet <- jet.colors(100)  # 100 separate color

col_vals_surface <-
  colorzjet[ findInterval(J_vals,
                          seq(min(J_vals),
                              max(J_vals),
                              length=100)) ]

col_vals_contour <-
  colorzjet[ findInterval(10^seq(-2, 3, length.out = 20),
                          seq(min(10^seq(-2, 3, length.out = 20)),
                              max(10^seq(-2, 3, length.out = 20)),
                              length=180)) ]
