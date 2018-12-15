nChooseK <- function(n_v, k_v) {
  #' n Choose k
  #' @description Given 'n' elements and combinations of size 'k', find number of unique combinations.
  #' @param n_v number of possible elements to create combinations
  #' @param k_v size of combinations (e.g. 3 means all unique combinations of 3 elements from n_v)
  #' @return numeric vector listing the total number of combinations
  #' @examples
  #' set.seed(1)
  #' n_v <- sample(1:50, size = 5)
  #' k1_v <- 3; k2_v <- 5
  #' nChooseK(n_v[1], k1_v)
  #' nChooseK(n_v[1], k2_v)
  #' nChooseK(n_v, k1_v)
  #' nChooseK(n_v, k2_v)
  #' @export
  
  ## Check lengths
  if (k_v > min(n_v)) warning("At least one value in n_v has fewer elements than combinations in k_v. NA will be returned.")
  
  ## Compute
  res_v <- factorial(n_v) / (factorial(k_v) * factorial(n_v - k_v))
  
  ## Return
  return(res_v)
} # nChooseK
