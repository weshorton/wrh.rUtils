mhead <- function(data_df, n = 5){
  #' Special head for table-like objects
  #' @description Show first n rows and columns of data.table/data.frame/matrix. Good for tables with many columns
  #' @param data_df any data.table, data.frame, or matrix
  #' @param n number of rows and columns to subset by. Default is 5
  #' @return prints to console data_df[1:n,1:n]
  #' @examples 
  #' data_mat <- matrix(1:10000, nrow = 100)
  #' mhead(data_mat)
  #' mhead(data_mat, 10)
  #' @export
  
  print(data_df[1:n,1:n])
} # mhead