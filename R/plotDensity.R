plotDensity <- function(density_ls, colors_ls = brewer.pal(8, "Dark2"), main_v = "", x_v = "", names_v = NULL, lwd_v = 2) {
  #' Plot Densities
  #' @description Plot multiple density functions on same graph
  #' @param density_ls list of either densities or vectors
  #' @param colors_ls list of colors one for each element of density_ls
  #' @param main_v character vector for title
  #' @param x_v character vector for x-axis
  #' @param names_v names to plot to legend. If NULL, will use names of density_ls.
  #' @param lwd_v line width
  #' @return print density plot to console
  #' @examples
  #' set.seed(1)
  #' d_ls <- list("A" = rnorm(1000), "B" = rnorm(1000, mean = 2, sd = 1), "C" = rnorm(1000, mean = -1, sd = 0.5))
  #' plotDensity(d_ls)
  #' @export
  
  ## Check input class and handle
  class_v <- unique(class(density_ls))
  
  if (length(class_v) > 1) {
    stop(sprintf("density_ls must be all one class, 'density' or 'numeric'. You have: %s\n",
                 paste(class_v, collapse = " ; ")))
  } else if (class_v != "density") {
    density_ls <- lapply(density_ls, function(x) {
      if (class(x) != 'density') {
        return(density(x))
        } else {
          return(x)
        }})
  } 
  
  ## Get legend
  if (is.null(names_v)) {
    names_v <- names(density_ls)
  }
  legend_v <- !is.null(names_v)
  legendColor_v <- unlist(colors_ls)
  
  ## Get axis parameters
  xMin_v <- min(unlist(lapply(density_ls, function(x) min(x$x, na.rm = T))))
  xMax_v <- max(unlist(lapply(density_ls, function(x) max(x$x, na.rm = T))))
  yMin_v <- min(unlist(lapply(density_ls, function(x) min(x$y, na.rm = T))))
  yMax_v <- max(unlist(lapply(density_ls, function(x) max(x$y, na.rm = T))))
  
  ## Plot first
  plot(density_ls[[1]],
       xlim = c(xMin_v, xMax_v),
       ylim = c(yMin_v, yMax_v),
       main = main_v,
       xlab = x_v,
       col = colors_ls[[1]],
       lwd = lwd_v)
  
  ## Plot the rest
  density_ls <- density_ls[-1]
  colors_ls <- colors_ls[-1]
  sapply(seq_along(density_ls), function(x) lines(density_ls[[x]], col = colors_ls[[x]], lwd = lwd_v))
  
  ## Add legend
  if (legend_v){
    legend("topright", legend = names_v, col = legendColor_v, lwd = 2, bty = 'n')
  } # fi
  
} # plotDensity