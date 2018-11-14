listToDT <- function(input_lsv, bind_v = "col", name_v = NULL){
  #' Convert List to data.table
  #' @description 
  #' Transform a list of vectors into a data.table. 
  #' Vectors can be of differing lengths (shorter vectors padded with NA).
  #' Each list element becomes a column (or row) in the new table. Note that classes will likely
  #' not be maintained when joining by row.
  #' @param input_lsv list of vectors. Can be of varying lengths and classes.
  #' @param bind_v One of either 'col' (default) or 'row'. Indicating if list elements should be joined by column or row.
  #' @param name_v character string for the name of the extra column added when joining by row. Only used when bind_v == "row".
  #' If NULL (default), column name will default to V1.
  #' @return data.table object whose format depends on bind_v value.  
  #' For bind_v == 'col': each column corresponds to an element of the input list. Number of rows in output data.table is
  #' equal to the length of the longest vector in the input list. Colnames are the same as the list element names.  
  #' 
  #' For bind_v == 'row': each row corresponds to an element of the input list. Number of columns in output data.table is
  #' one more than the length of the longest vector in the input list, with the extra column containing the list element names.
  #' @examples
  #' list1_ls <- list("A" = 1:5, "B" = LETTERS[1:10], "C" = letters[11:16])
  #' list2_ls <- list("2" = 10:1, "5" = LETTERS[1:10])
  #' list1_dt <- listToDT(list1_ls)
  #' list2_dt <- listToDT(list2_ls)
  #' rList1_dt <- listToDT(list1_ls, bind_v = "row")
  #' rList1_dt2 <- listToDT(list1_ls, bind_v = "row", name_v = "Test")
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
  if (bind_v == 'col') {
    output_dt <- as.data.table(output_lsv)
  } else if (bind_v == 'row') {
    output_mat <- do.call(rbind, output_lsv)
    rownames(output_mat) <- names(output_lsv)
    colName_v <- ifelse(is.null(name_v), "V1", name_v)
    output_dt <- convertDFT(data_dft = output_mat, newName_v = colName_v)
  } else {
    stop(sprintf("Incorrect argument for bind_v. Must be one of 'col' or 'row', but %s was provided.\n", bind_v))
  }

  # Return
  return(output_dt)
} # listToDT()