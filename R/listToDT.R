listToDT <- function(input_lsv){
  #' Convert List to data.table
  #' @description 
  #' Transform a list of vectors into a data.table. 
  #' Vectors can be of differing lengths (shorter vectors padded with NA).
  #' Each list element becomes a column in the new table.
  #' @param input_lsv list of vectors. Can be of varying lengths and classes
  #' @return data.table object where each column corresponds to an element of the input list.
  #' Number of rows in output data.table is equal to the length of the longest vector in input list.
  #' @examples
  #' list1_ls <- list("A" = 1:5, "B" = LETTERS[1:10], "C" = letters[11:16])
  #' list2_ls <- list("2" = 10:1, "5" = LETTERS[1:10])
  #' list1_dt <- listToDT(list1_ls)
  #' list2_dt <- listToDT(list2_ls)
  #' @export
  
  ## Get maximum vector length
  maxLength_v <- max(unlist(lapply(input_lsv, function(x) length(x))))
  
  # Function to extend each vector in list with NA's to match length of maximum vector
  addNAToVector <- function(listElement_v, maxLength_v){
    difference_v <- maxLength_v - length(listElement_v)
    NAToAdd_v <- rep(NA, times = difference_v)
    newElement_v <- append(listElement_v, NAToAdd_v)
    return(newElement_v)
  } # addNAToVector
  
  # Apply NA-adding function to each vector in list
  output_lsv <- lapply(input_lsv, function(x,y) addNAToVector(x, maxLength_v))
  
  # Convert to data.table
  output_dt <- as.data.table(output_lsv)
  
  # Return
  return(output_dt)
} # listToDT()