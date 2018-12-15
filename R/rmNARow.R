rmNARow <- function(data_dt, cols_v = NULL, extract_v = F){
  #' Remove rows that have NA in certain columns
  #' @description Remove all rows that have an NA in at least one of the specified columns. 
  #' @param data_dt data.table or data.frame to have rows removed
  #' @param cols_v columns to check for NA in. If NULL (default) will check all columns
  #' @param extract_v logical, TRUE - output the NA rows; FALSE (default) - output the input table with NA rows removed
  #' @return data.table either containing the offending rows (extract_v == T), or the input table with those rows removed
  #' @examples
  #' data_df <- data.frame("A" = 1:5, "B" = c(1,2,NA,4,5), "C" = c(NA,2:5), "D" = c(1:4,NA))
  #' rmNARow(data_dt = data_df, cols_v = "A")
  #' rmNARow(data_dt = data_df, cols_v = c("A", "B"))
  #' rmNARow(data_dt = data_df)
  #' rmNARow(data_dt = data_df, extract_v = T)
  #' @export
  
  ## Handle cols_v
  if (is.null(cols_v)) cols_v <- colnames(data_dt)
  
  ## Logical of if row is complete or not
  if (class(data_dt) == "matrix") {
    stop("'data_dt' must be data.table or data.frame.")
  } else if (class(data_dt) == "data.frame") {
    completeRows_v <- complete.cases(data_dt[,cols_v])
  } else {
    completeRows_v <- complete.cases(data_dt[,..cols_v])
  }
  
  ## Return
  if (extract_v){
    return(data_dt[completeRows_v != T,])
  } else {
    return(data_dt[completeRows_v,])
  } # fi
  
} # rmNARow
