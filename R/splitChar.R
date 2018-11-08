splitChar <- function(vector_v){
  #' Split character string
  #' @description Split character string into vector where length(output) == nchar(input)
  #' @param vector_v any character vector that needs to be separated into its component parts
  #' @return another vector of length == nchar(input)
  #' @examples 
  #' foo <- "ABCDE"
  #' splitChar(foo)
  #' @export
  
  outVector_v <- unlist(strsplit(vector_v, split = ''), use.names = F)
  
  return(outVector_v)
  
} # splitChar