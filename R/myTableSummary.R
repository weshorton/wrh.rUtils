myTableSummary <- function(data_dt, cols_v, addSum_v = T, verbose_v = F) {
  #' My Table Summary
  #' @description Wrapper for table() that makes outputs nicer and the way I like them
  #' @param data_dt data to summarize
  #' @param cols_v column name(s) from data_dt that need to be summarized. Can only be 1, 2, or 3 columns (3 not recommended)
  #' @param addSum_v logical if totals should be added at the bottom.
  #' @details Works for 1 to 3 columns, although 3 not recommended.
  #' @return data.table
  #' @export
  
  ### Convert, if necessary
  if (!is.data.table(data_dt)) {
    warning("Input is not data.table. Doing simple as.data.table conversion.\n")
    data_dt <- as.data.table(data_dt)
  } # fi
  
  ### Table
  table_tab <- table(data_dt[,mget(cols_v)])
  
  ### Convert
  if (length(cols_v) == 1) {
    
    table_dt <- as.data.table(table_tab)
    colnames(table_dt)[colnames(table_dt) == "V1"] <- cols_v
    nameCols_v <- cols_v
    
  } else if (length(cols_v) == 2) {
    
    table_dt <- convertDFT(as.data.frame.matrix(table_tab), newName_v = cols_v[1])
    nameCols_v <- cols_v[1]
    
  } else if (length(cols_v) == 3) {
    
    if (verbose_v) warning("1- and 2-column summaries are best-supported by this. 3 columns works, but may be better doing something else.\n")
    
    ### Split 3-dimensional table into list of 2-D tables
    n_v <- dim(table_tab)[3]
    table_lstab <- lapply(1:n_v, function(x) table_tab[,,x])
    names(table_lstab) <- dimnames(table_tab)[[3]]
    
    ### Convert to data.table
    table_lsdt <- lapply(table_lstab, function(x) convertDFT(as.data.frame.matrix(x), newName_v = cols_v[1]))
    
    ### Rbind
    table_dt <- dtListBind(table_lsdt, newName_v = cols_v[3])
    nameCols_v <- cols_v[c(3,1)]
    
  } else {
    
    warning("Only supports up to 3 columns. Standard table() output will be returned.\n")
    return(table_tab)
    
  }
  
  ### Add totals
  if (addSum_v) {
    sdCols_v <- setdiff(colnames(table_dt), nameCols_v)
    total_dt <- table_dt[, lapply(.SD, sum), .SDcols = sdCols_v]
    
    ### Is there a cleaner way to do this?
    if (length(nameCols_v) == 1) {
      add_dt <- setNames(data.table("Total"), nameCols_v)
    } else {
      add_dt <- setNames(data.table("Total", "Total"), nameCols_v)
    }
    
    total_dt <- cbind(add_dt, total_dt)
    table_dt <- rbind(table_dt, total_dt)
  } # fi
  
  ### Output
  return(table_dt)
  
} # myTableSummary