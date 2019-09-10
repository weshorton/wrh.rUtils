revComp <- function(string_v) {
  #' Reverse Complement
  #' Given vector of strings of ATCG, provide reverse complement of each string
  #' @param string_v vector of strings to be reverse complemented
  #' @return Vector of same length, reverse complemented
  #' @export
  
  ### Split into characters
  charString_lsv <- lapply(string_v, splitChar)
  
  ### Reverse
  revCharString_lsv <- lapply(charString_lsv, rev)
  
  ### Complement
  revCompString_lsv <- lapply(revCharString_lsv, function(x) {
    sapply(x, function(y) {
      ifelse(y == "A", "T",
             ifelse(y == "T", "A",
                    ifelse(y == "C", "G", "C")))
    })
  })
  
  ### Collapse and turn to vector
  revComp_v <- sapply(revCompString_lsv, paste, collapse = "")
  
  ### Output
  return(revComp_v)
}
