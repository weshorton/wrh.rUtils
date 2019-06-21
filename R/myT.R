myT <- function(data_dt, newName_v = NA, toClass_v = 'numeric', outType_v = 'dt') {
  #' My Transpose
  #' @description Transpose data. Make the first column of original data be the colnames
  #' of the new data and the colnames of original data be either the 
  #' rownames or the 1st column of new data (depends on output type)
  #' @param data_dt data to transpose
  #' @param newName_v name for new 1st column of transposed data (only used if dt is output type)
  #' @param toClass_v which class to convert output to (only 'numeric' and 'character' work right now, NULL to avoid.)
  #' @param outType_v which type of object to return (dt for data.table, df for data.frame, mat for matrix)
  
  ### Transpose
  t_mat <- t(data_dt)
  
  ### New Column Names
  colnames(t_mat) <- t_mat[1,]
  t_mat <- t_mat[-1,,drop=F]
  
  ### Convert class, if desired
  if (!is.null(toClass_v)) {
    t_mat <- apply(t_mat, 2, function(x) {class(x) <- toClass_v; return(x)})
  }
  
  ### Convert to specified output
  if (outType_v == 'mat') {
    return(t_mat)
  } else if (outType_v == "df") {
    return(as.data.frame(t_mat))
  } else if (outType_v == "dt") {
    if (is.na(newName_v)) newName_v <- "Rows"
    return(convertDFT(t_mat, newName_v = newName_v))
  } # fi
  
} # myT