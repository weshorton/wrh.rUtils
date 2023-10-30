calcENS <- function(values_v, proportion_v = T) {
  #' Calc Effective Number of Species
  #' @description
    #' Use the proportions of populations to calculate effective number of species. This is a general function, but I use
    #' it mainly to check the maximum iLISI when comparing harmony integrations.
    #' Simpson Diversity Index (D) = sum(prop_i)^2, where prop_i = ni / N, where ni = abundance of species i and N = total sample number
    #' Inverse = 1/D
  #' @param values_v vector of values to compute with.
  #' @param proportion_v logical indicating if values are proportions or actual values
  #' @return inverse sum of squared proportions of values_v
  #' @export
  
  ### Compute proportions
  if (!proportion_v) {
    
    sum_v <- sum(values_v)
    values_v <- values_v / sum_v
    
  } # fi !proportion
  
  ### Get sum of squares
  ss_v <- sum(sapply(values_v, function(x) x^2))
  
  ### Get inverse
  iss_v <- 1/ss_v
  
  ### Return
  return(iss_v)
  
} # calcENS