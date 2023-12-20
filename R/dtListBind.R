dtListBind <- function(list_lsdt, newName_v = "Name") {
  #' Data table list bind
  #' @description
    #' rbind a list of data.tables and also add the
    #' names of the list elements as a new column in each dt.
  #' @param list_lsdt named list of data.tables
  #' @param newName_v character vector that will be the new column name
  #' @return single data.table with one more column that input data.tables and as 
  #' many rows as all the input tables combined.
  #' @export
  
  ### Check names
  if (is.null(names(list_lsdt))) {
    
    warning("list_lsdt doesn't have any names. If you don't want names added, then just run do.call(rbind, list_lsdt).
            This function will add a new column 'Name' to each dt in list_lsdt and the value will be the list element index.\n")
    names(list_lsdt) <- as.character(1:length(list_lsdt))
    
  } # fi is.null names
    
  ### Add new column
  list_lsdt <- sapply(names(list_lsdt), function(x) {
    y <- list_lsdt[[x]]
    y[[newName_v]] <- x
    return(y)}, simplify = F, USE.NAMES = T)
  
  ### Combine and output
  return(do.call(rbind, list_lsdt))
  
} #dtListBind