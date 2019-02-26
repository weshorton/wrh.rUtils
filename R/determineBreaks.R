determineBreaks <- function(data_mat, colors_v = rev(colorRampPalette(brewer.pal(11, "RdBu"))(100)),
                            cut_v = "max", print_v = F, verbose_v = T) {
  #' Determine Breaks
  #' @description Determine custom breaks for heatmap by excluding high values.
  #' @param data_mat matrix of counts. Most likely will be row-scaled, but doesn't have to be. if data.table provided, 1st column is assumed to be
  #' an identifier column and will be assigned to row.names.
  #' @param colors_v Vector of colors that will be used. Default is red (high) to blue (low)
  #' @param cut_v Either a numeric value indicating which percentile to use (3 would use the 3rd and 97th percentile). Or "max" (default)
  #' which will cut off the maximum value on either end.
  #' @param print_v logical indicating whether or not to print a test heatmap
  #' @param verbose_v logical indicating whether or not to print out messages
  #' @export
  
  ## Handle data
  if (class(data_mat)[1] == "data.table") {
    rowName_v <- colnames(data_mat)[1]
    warning(sprintf("Input data is of class 'data.table'. First column (%s) will be removed and used as row names.\n", rowName_v))
    data_mat <- convertDFT(data_mat)
  } else if (class(data_mat)[1] == "data.frame") {
    data_mat <- as.matrix(data_mat)
  }
  
  ## Get min and max
  min_v <- min(data_mat, na.rm = T); max_v <- max(data_mat, na.rm = T)
  
  ## Get cut-off
  if (cut_v == "max") {
    min2_v <- unique(sort(data_mat))[2]
    max2_v <- unique(sort(data_mat, decreasing = T))[2]
  } else {
    percentile_v <- quantile(data_mat, probs = seq(0, 1, 0.01), na.rm = T)
    min2_v <- percentile_v[[paste0(cut_v, "%")]]
    max2_v <- percentile_v[[paste0((100-cut_v), "%")]]
  }
  
  ## Make breaks and colors
  breaks_v <- c(min_v, seq(min2_v, max2_v, length.out = length(colors_v)+1), max_v)
  outColors_v <- c(colors_v[1], colors_v, colors_v[length(colors_v)])
  
  ## Print test
  if (print_v) {
    pheatmap(data_mat, cluster_rows = F, cluster_cols = F, color = outColors_v, breaks = breaks_v, cellwidth = 10)
  }
  
  ## Print number excluded
  if (verbose_v) {
    ## Get numbers
    numAbove_v <- length(which(data_mat > max2_v))
    numBelow_v <- length(which(data_mat < min2_v))
    ## Print
    if (cut_v == "max") {
      cat(sprintf("Using second-highest value of %s and second-lowest value of %s, %s cells were collapsed on the high end and %s on the low.\n",
                  round(max2_v, digits = 4), round(min2_v, digits = 4), numAbove_v, numBelow_v))
    } else {
      cat(sprintf("Using the %s and %s percentile values (%s and %s, respectively). %s cells were collapsed on the high end and %s on the low.\n",
                  (100-cut_v), cut_v, round(max2_v, digits = 4), round(min2_v, digits = 4), numAbove_v, numBelow_v))
    } # fi
  } # fi
  
  ## Return output
  return(list("breaks" = breaks_v, "colors" = outColors_v))
  
} # determineBreaks