naTo0 <- function(data_dt, cols_v = NA, verbose_v = F){
  #' NA to 0
  #' @description Given a set of columns in a data.table, convert all NA values in those columns to 0
  #' @param data_dt data.table with NAs in some columns. Data.frames and matrices are accepted.
  #' @param cols_v vector of either column indices (numeric) or column names (character) that have NAs to be replaced
  #' @param verbose_v boolean values determining if verbose printing should be output.
  #' @return data.table with same dim(), but NAs are now 0 for all cols_v
  #' @examples 
  #' data_df <- data.frame(A = 1:5, B = c(5,4,NA,2,1), C = c(3,NA,2,NA,NA), D = c(NA, NA, 1:3), stringsAsFactors = F)
  #' data_mat <- as.matrix(data_df)
  #' data_dt <- as.data.table(data_df)
  #' naTo0(data_df, cols_v = "D")
  #' naTo0(data_dt, cols_v = colnames(data_dt))
  #' naTo0(data_mat, cols_v = c(1,3))
  #' naTo0(data_df)
  #' @export
  
  ## If cols_v not specified, all columns
  if (is.na(cols_v[1])) {
    if (verbose_v) message("cols_v argument is NA. Converting all columns.")
    cols_v <- colnames(data_dt)
  } else {
    if (verbose_v) message(sprintf("Converting columns: %s\n", paste(cols_v, collapse = " ")))
  }
  
  ## If all columns are selected, quick and easy
  if (identical(cols_v, colnames(data_dt))) {
    if (verbose_v) message("All columns were specified.")
    data_dt[is.na(data_dt)] <- 0
    return(data_dt)
  } # fi
  
  ## Get class
  class_v <- class(data_dt)
  
  ## Convert, if data.frame or matrix
  if (class_v[1] %in% c("data.frame", "matrix")) {
    
    ## Notify user
    if (verbose_v) { 
      message(sprintf("Instead of data.table, %s was input.\nNA to 0 conversion should still work, but be sure to check!",
                      class_v[1]))
    } # fi
    
    ## Convert
    data_dt <- convertDFT(data_dft = data_dt)
    
    ## If columns are numeric, have to add one to each
    # if (is.numeric(cols_v)) {
    #   cols_v <- cols_v + 1
    # }
    
  } else {
    data_dt[1,1] <- data_dt[1,1]
  }
  
  ## Convert columns
  if (is.numeric(cols_v)) cols_v <- as.integer(cols_v)
  for (j in cols_v) set(data_dt, which(is.na(data_dt[[j]])), j, 0)
  
  ## Convert back to input class
  if (class_v[1] %in% c("data.frame", "matrix")) {
    data_dt <- convertDFT(data_dt, rmCol_v = F)
    if (class_v[1] == "matrix") data_dt <- as.matrix(data_dt)
  } # fi
  
  ## Return 
  return(data_dt)
  
} # naTo0