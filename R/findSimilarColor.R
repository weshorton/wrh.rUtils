findSimilarColor <- function(color_v, adjustment_v = 'tint', factor_v = 0.4) {
  #' Find Similar Color
  #' @description
  #' Find a similar hex code to a given input using
  #' one of the available methods
  #' @param color_v vector of colors
  #' @param adjustment_v Type of adjustment - shade, tint, tone, layer, average
  #' @param factor_v value from 0 to 1 indicating how closely to white or black to pull color.
  #' @details if length(color_v) is greater than one, will average the hex code before adjusting
  #' shade - darken by a factor of black
  #' tint - lighten by a factor of white
  #' average - no adjustment, just take average of values in color_v
  #' To do - can I output more than one color?
  #' To do - can i do tone and layer?
  #' @return vector of hexcodes of length n_v
  #' @export
  
  ### Convert hex to rgb
  rgb_mat <- grDevices::col2rgb(color_v)
  
  ### Take averages if multiple columns
  if (ncol(rgb_mat) > 1) {
    rgb_v <- rowMeans(rgb_mat)
  } else {
    rgb_v <- as.vector(rgb_mat)
    names(rgb_v) <- rownames(rgb_mat)
  } # fi ncol
  
  ### Apply various methods ~~~~~~
  
  # Add a factor of black
  if (adjustment_v %in% c("shade", "s")) {
    rgb_v <- rgb_v - rgb_v * factor_v
    
    # Add a factor of white
  } else if (adjustment_v %in% c("tint", "ti")) {
    rgb_v <- rgb_v + rgb_v * factor_v
  } else {
    # Other option is do nothing. Throw error if none of these 3.
    if (!adjustment_v %in% c("average", "a", "avg")) {
      stop(sprintf("adjustment_v can only be 'average', 'shade', or 'tint'.\n%s was provided.\n", adjustment_v))
    }
  } # fi
  
  ### If any are > 255, max them out
  rgb_v <- sapply(rgb_v, function(x) {
    ifelse(x <= 255, x, 255)
  })
  
  ### Convert to hex and output
  hex_v <- wrh.rUtils::myRGB(rgb_v)
  return(hex_v)
  
} # findSimilarColor
