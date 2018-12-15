dupCols <- function(data_dt, rev_v = F, remove_v = T, verbose_v = F){
  #' Duplicated Columns
  #' @description Identify duplicated column names in a data.table. Print them to console and optionally remove them from data.table.
  #' Will keep the first (or last) occurrence of a column name and remove all subsequent occurrences.
  #' @param data_dt input data.table with duplicated columns
  #' @param rev_v logical. TRUE - keep last occurrence of duplicated name; FALSE (default) keep 1st occurrence
  #' @param remove_v logical, TRUE - remove columns; FALSE - do not remove, just print column names to console (if verbose)
  #' @param verbose_v logical. TRUE - print out duplicated columns. FALSE - do not print
  #' @return data.table same as data_dt, optionally with extra columns removed.
  #' @examples
  #' dat <- data.table("A" = 1:5, "B" = 6:10, "A" = 11:15)
  #' dupCols(dat)
  #' dupCols(dat, rev_v = T)
  #' @export
  
  ## Count occurrences of each column name
  colCounts_dt <- as.data.table(table(colnames(data_dt)))
  
  ## Extract duplicated columns
  dupCols_dt <- colCounts_dt[N > 1,]
  
  ## Get indexes to remove, for each duplicated column
  removeIndexes_v <- unlist(sapply(dupCols_dt$V1, function(x){
    y <- grep(x, colnames(data_dt))
    if (rev_v) y <- rev(y)
    z <- y[2:length(y)]
    return(z)
  }, simplify = F))
  
  ## Print them
  if (verbose_v) {
    print("Columns with more than one occurrence: ")
    print(colnames(data_dt)[removeIndexes_v])
  }
  
  ## Remove them
  if (remove_v){
    data_dt <- data_dt[,-removeIndexes_v,with=F]
  }
  
  ## Return
  return(data_dt)
} # dupCols
