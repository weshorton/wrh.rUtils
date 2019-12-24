label_log2 <- function(x) {
  #' Label Log2 Ggplot
  #' @description label log2 axes as 2^x.
  #' Found on SO:
  #' https://stackoverflow.com/questions/47879117/how-to-format-x-axis-tick-label-in-2x-format
  #' @param x 
  #' @export

  y <- parse(text = paste0('2^', log(x, 2)))
  return(y)
  
} # label_log2