thirds <- function(x_v, na.rm = T){
  #' Thirds-based vector division
  #' @description Instead of dividing a vector into quartiles, divide into thirds and return values at those points.
  #' @param x_v numeric vector
  #' @param na.rm TRUE (default) remove NAs. FALSE - don't remove. returns all NA
  #' @return vector containing value of min, bottom 3rd, median, top 3rd, and max.
  #' @examples
  #' set.seed(1)
  #' a <- sample(1:1000, size = 100)
  #' b <- sample(1:100000, size = 1000)
  #' thirds(a)
  #' thirds(b)
  #' @export
  
  ## Check NAs
  isNA_v <- is.na(x_v)
  if (any(isNA_v)){
    if (na.rm) {
      x <- x[!isNA_v]
    } else {
      return(rep.int(NA,5))
    } # fi na.rm
  } # fi any()
  
  ## Sort
  x_v <- sort(x_v)
  
  ## Get length
  len_v <- length(x_v)
  
  ## Handle zero-length
  if (len_v == 0){
    out <- rep.int(NA,5)
  } else {
    ## Get 1/4 division (len3_v*3 == len_v+1)
    len3_v <- floor((len_v+2)/1.5)/2
    ## Get divisions
    d <- c(1, len3_v, (len_v+1)/2, len_v + 1 - len3_v, len_v)
    ## Take mean of the two values surrounding division (if not an integer)
    out <- 0.5 * (x_v[floor(d)] + x_v[ceiling(d)])
  } # fi len_v == 0
  
  ## Names
  names(out) <- c("Min", "1st T.", "Median", "2nd T.", "Max")
  
  ## Return
  return(out)
  
} # thirds