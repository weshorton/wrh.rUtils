splitInputNumbers <- function(char_v, split_v = ",") {
  #' Split Input Numbers
  #' @description When list of numbers are provided as a character (usually as command-line argument, or user text input),
  #' split them into a numeric vector. Also supports splitting "4:8" into 4, 5, 6, 7, 8.
  #' @param char_v Character vector that is input
  #' @param split_v Delimiter used to separate numbers.
  #' @examples
  #' char1_v <- "1,2,3,4,5"
  #' char2_v <- "1:5"
  #' char3_v <- "1:5,9,10,20:25"
  #' splitInputNumbers(char1_v)
  #' splitInputNumbers(char2_v)
  #' splitInputNumbers(char3_v)
  #' @export
  
  ### Split
  splitChar_v <- strsplit(char_v, split = split_v)[[1]]
  
  ### Find which to expand
  whichExpand_v <- grep(":", splitChar_v)
  
  ### Expand
  expandChar_v <- NULL
  for (i in 1:length(splitChar_v)) {
    
    if (length(grep(":", splitChar_v[i])) == 0) {
      expand_v <- splitChar_v[i]
    } else {
      sep_v <- strsplit(splitChar_v[i], split = ":")[[1]]
      expand_v <- as.numeric(sep_v[1]):as.numeric(sep_v[2])
    } # fi
    
    expandChar_v <- c(expandChar_v, expand_v)
  }
  
  ### Change to numeric
  expandNum_v <- as.numeric(expandChar_v)

  ### Return
  return(expandNum_v)
  
}