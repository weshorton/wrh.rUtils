splitComma <- function(string_v){
  #' Split a string along the commas in the string
  #' @description Just a concise way to split a string on the commas. Returning a vector of strings instead.
  #' @param String that will be split
  #' @return Vector of strings. One element for each section of string that was previously divided by a comma
  #' @examples 
  #' myString_v <- c("This,is,a,string")
  #' splitComma(myString_v)
  #' @export
  
  vector_v <- unlist(sapply(string_v, function(x) strsplit(x, split = ","), USE.NAMES = F))
  return(vector_v)
} # splitComma