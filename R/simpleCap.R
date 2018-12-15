simpleCap <- function(x, lowerCaps_v = T) {
  #' Capitalize first letter of each word
  #' @description given multi-word vector, capitalize the first letter of each word.
  #' Taken from 'Examples' section of ?toupper
  #' @param x vector
  #' @param lowerCaps_v boolean indicating if capitalized letters (that are not the first letter in each word)
  #' should be left capitalized or changed to lower case.
  #' @return same as X, but each first letter is capitalized.
  #' @examples 
  #' a <- "capitalize the first letter"
  #' b <- "optionally RETURN other CAPS to lower"
  #' c <- c("works with multiple", "strings at once")
  #' simpleCap(a)
  #' simpleCap(b)
  #' simpleCap(b, F)
  #' simpleCap(c)
  #' @export
  
  y <- sapply(x, function(z) {
    s <- strsplit(z, " ")[[1]]
    if (lowerCaps_v) {
      paste(toupper(base::substring(s, 1, 1)), tolower(base::substring(s, 2)),
            sep = "", collapse = " ")
    } else {
      paste(toupper(base::substring(s, 1, 1)), base::substring(s, 2),
            sep = "", collapse = " ")
    }
  })
  
  return(unname(y))
  
} # simpleCap
