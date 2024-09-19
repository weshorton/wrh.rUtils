readComplexArgs <- function(arg_v, colSep_v = ";", varValSep_v = "\\.", valSep_v = ",", simplify_v = T) {
  #' Read Complex Args
  #' @description
    #' Split complex command-line arguments into their component pieces
  #' @param arg_v vector containing the complex argument. See details for more info
  #' @param colSep_v character. default is ";". Whatever character separates different columns in arg_v. See details
  #' @param varvalSep_v character. default is "\\." Whatever character separates a column name from its factor levels in arg_v. See details
  #' @param valSep_v character. default is ",'. Whatever character separates factor levels in arg_v. See details
  #' @param simplify_v logical. default is TRUE. TO DO - should I simplify the empty list to a vector?
  #' @details
    #' When running a command-line Rscript, sometimes you need to pass complex arguments like comma-separated
    #' lists of variables. You can't have any spaces in command-line args, so have to provide the argument all together
    #' in one string and split it inside R.  
    #' 
    #' The format for a complex argument is: colName1.val1,val2,val3,...;colName2.val2,val2,...;colNameN...
    #' So first there is a semi-colon separated vector of column specifications. The column specification
    #' begins with the column name, optionally followed by factor levels for that column.
    #' 
    #' Each column can either have factor levels or not, they don't all have to be one or the other.
    #' 
    #' If you don't have factor levels, the argument is simply: colName1;colName2;colName3;...
    #' In this case, ";" and ',' will be treated the same for backwards compatibility.
    #' 
    #' Note that colSep_v (;), varValSep_v (.), and valSep_v (,) can all be specified.  
    #' 
  #' @return Returns a list of length = number of elements split by colSep_v, with those elements as the names. 
  #' If factors provided, lists will have values in factor order. If not provided, lists will be empty with names only.
  #' @export

  ### Count columns
  hasColSep_v <- grepl(colSep_v, arg_v)
  
  ### Split columns
  args_lsv <- strsplit(arg_v, split = colSep_v)[[1]]
  
  ### Check split
  if (length(args_lsv) == 1) {
    
    if (hasColSep_v) {
      warning(sprintf("After separating arg_v: %s with separator: %s, vector of length 1 returned: %s
                    Going to use ',' as separator instead. Make sure this is correct!\n",
                      arg_v, colSep_v, args_lsv))
      args_lsv <- strsplit(arg_v, split = ',')[[1]]
    } else {
      cat(sprintf("No column separator found, assuming only one column with values.\n"))
    } # fi hasColSep_v
    
  } # fi args_lsv -- 1
  
  ### Split vars from vals
  vars_v <- sapply(args_lsv, function(x) strsplit(x, split = varValSep_v)[[1]][1], USE.NAMES = F)
  vals_v <- sapply(args_lsv, function(x) strsplit(x, split = varValSep_v)[[1]][2], USE.NAMES = F)
  
  ### Make output
  out_ls <- list()
  for (i in 1:length(vars_v)) {
    out_ls[[vars_v[i]]] <- strsplit(vals_v[i], split = valSep_v)[[1]]
  }
  
  ### Return
  return(out_ls)

} # readComplexArgs
