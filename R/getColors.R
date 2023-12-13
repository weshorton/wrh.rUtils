getColors <- function(n_v) {
  #' Get Colors
  #' @description
  #' Get vector of length n_v from RColorBrewer
  #' @param n_v number of colors to get
  #' @details
  #' Uses Dark2 (8) first, then Set3 (12), and finally Pastel1 (9).
  #' If n_v is greater than the total colors in those 3 sets, warning will be thrown.
  #' @return vector of color hex codes
  #' @export
  
  ### Build color list
  colors_v <- c(RColorBrewer::brewer.pal(8, "Dark2"),
                RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(9, "Pastel1"))
  
  ### Handle large n's
  if (n_v > length(colors_v)) {
    if (n_v > 2*length(colors_v)) {
      stop(sprintf("Provided n_v (%s) is too large. Max recommended is 29\n", n_v))
    } else {
      warning(sprintf("Provided n_v (%s) is larger than the recommended max of 29. The first %s colors will be recycled.\n",
                      n_v, (n_v-29)))
      colors_v <- rep(colors_v, 2)
    } # fi more than double
  } # more than length(colors_v)
  
  ### Get colors
  return(colors_v[1:n_v])
  
} # getColors_v