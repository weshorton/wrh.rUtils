isPrime <- function(x, div_v = T, print_v = T) {
  #' Is Prime?
  #' @description Determine if a number is a prime number
  #' @param x number to test
  #' @param div_v boolean determining if divisors should be output or not.
  #' @param print_v boolean determining if answer should be printed to console
  #' @return list. 1st element - boolean of if x is prime or not. 2nd element - vector of divisors, if specified
  #' @examples 
  #' test_v <- c(2, 10, 17, 567, 5678)
  #' sapply(test_v, isPrime)
  #' sapply(test_v, function(x) isPrime(x, print_v = F, div_v = F))
  #' @export
  
  ## Get quotients
  quotient_v <- sapply(1:x, function(y) x/y)
  
  ## Get whole-number divisors
  divisors_v <- which(round(quotient_v) == quotient_v)
  
  ## Search for extra divisors
  extraDivisors_v <- setdiff(divisors_v, c(1,x))
  
  ## Format output
  out_lsv <- list("prime" = length(extraDivisors_v) == 0,
                  "divisors" = divisors_v)
  
  ## Remove divisors, if specified
  if (!div_v) out_lsv$divisors <- NULL
  
  ## Print, if specified
  if (print_v) print(out_lsv$prime)
  
  ## Return
  return(out_lsv)
} # isPrime
