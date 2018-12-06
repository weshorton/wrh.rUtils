geomMean <- function(counts_v) {
  #' Geometric Mean
  #' @description Find geometric mean of numeric vector
  #' @param counts_v numeric vector of values
  #' @return geometric mean of vector
  #' @examples 
  #' set.seed(25)
  #' foo <- sample(1:100, 5)
  #' geomMean(foo)
  #' @export
  
  ## Get product of vector
  prod_v <- prod(counts_v)
  
  ## Get length of vector
  length_v <- length(counts_v)
  
  ## Apply formula; geomMean = (X1*X2*X3*...Xn)^(1/n)
  geomMean_v <- prod_v^(1/length_v)
  
  ## Return
  return(geomMean_v)
  
} # geomMean
