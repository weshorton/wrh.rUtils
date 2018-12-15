plotColor <- function(color_v, title_v = NULL, save_v = F, pch_v = 15, cex_v = 3) {
  #' Plot Color
  #' @description Plot vector of colors and their labels in order to preview them for later use
  #' @param color_v - character vector of color names, hex codes, etc. to be plotted. If named, the names will be plotted as well.
  #' @param title_v - optional title for plot. Default is NULL
  #' @param save_v - logical. 
  #' F - print to console without saving; 
  #' T - save file to working directory. File name will be title_v with spaces changed to underscore. If title_v is NULL, title is 'colors.pdf'
  #' @param pch_v - standard graphical parameter controlling point type. Default is 15 (square). see ?pch for list of options
  #' @param cex_v - standard graphical parameter controlling point size. Default is 3.
  #' @return print plot to stdout or to file
  #' @examples 
  #' col1 <- c("red", "blue", "black", "green"); names(col1) <- col1
  #' col2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
  #' plotColor(col1, title_v = "Named Colors")
  #' plotColor(col2, title_v = "Hex Colors")
  #' @export
  
  ### Get file name
  if (is.null(title_v)) {
    file_v <- file.path(getwd(), "colors.pdf")
    title_v <- ''
  } else {
    file_v <- file.path(getwd(), paste0(gsub(" ", "_", title_v), ".pdf"))
  } # fi
  
  ### Open device
  if (save_v) pdf(file = file_v)
  
  ### Plot colors
  n_v <- length(color_v)
  plot(1:n_v, cex = cex_v, pch = pch_v, col = color_v, xlim = c(0, n_v+1), ylim = c(0, n_v+1),
       xlab = '', ylab = '', main = title_v)
  
  ### Add names
  if (!is.null(color_v)) {
    text(1:n_v, names(color_v), pos = 1, offset = .75, xpd = NA)
  } # fi
  
  ### Close device
  if (save_v) dev.off()
} # plotColor
