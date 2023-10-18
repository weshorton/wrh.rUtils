### Look into simplifying this using Reduce()

mergeDTs <- function(data_lsdt, mergeCol_v, keepCol_v = NULL, ...) {
  #' Merge many data.tables together
  #' @description Take many data.tables and merge on and ID column, extracting a single column from each data.table as the column of interest
  #' @param data_lsdt list of data.tables to merge
  #' @param mergeCol_v which column from all of the data.tables to use to merge
  #' @param keepCol_v which column from all of the data.tables to use as the column of interest. If NULL, use all columns
  #' @param ... extra parameters passed to merge. So far all = T and sort = F unless changed here.
  #' @return data.table with ncol == length(data_lsdt) + 1. Column names are names of list, or defaults to V1, V2,...
  #' @import data.table
  #' @export
  
  ## Grab extra arguments
  extraParams_lsv <- list(...)
  
  ## Handle extra arguments
  if (!is.null(extraParams_lsv$all)){
    all_v <- extraParams_lsv$all
  } else {
    all_v <- T
  } # fi
  
  if (!is.null(extraParams_lsv$sort)){
    sort_v <- extraParams_lsv$sort
  } else {
    sort_v <- F
  } # fi
  
  ## If keepCol_v is NULL, grab all other columns
    if (is.null(keepCol_v)){
    keepCol_v <- colnames(data_lsdt[[1]])[-which(colnames(data_lsdt[[1]]) %in% mergeCol_v)]
  } # fi
  
  ## Create initial table by extracting the 2 columns of interest from the rest
  merge_dt <- data_lsdt[[1]][,mget(c(mergeCol_v, keepCol_v))]

  ## Create initial column names (first check if list has names and add if not)
  if (is.null(names(data_lsdt))) {
    names_v <- paste("V", 1:length(data_lsdt))
    names(data_lsdt) <- names_v
  } # fi
  
  if (length(keepCol_v) > 1){
    colNames_v <- c(mergeCol_v, paste(names(data_lsdt)[1], keepCol_v, sep = "_"))
  } else {
    colNames_v <- c(mergeCol_v, names(data_lsdt)[1])
  } # fi
  
  for (i in 2:length(data_lsdt)) {
    
    ## If the data.tables in the list have different columns. First search for keepCol, then grab other columns
    columnCheck_v <- setdiff(colnames(data_lsdt[[i]]), c(mergeCol_v, keepCol_v))
    if (length(columnCheck_v) > 0) {
      warning(sprintf("There are %s column(s) in data %s that aren't merge or keep columns, meaning this data.table will be subset.\n\t%s\n",
                      length(columnCheck_v), i, paste(columnCheck_v, collapse = "; ")))
    } # fi
    
    columnCheck2_v <- setdiff(c(mergeCol_v, keepCol_v), colnames(data_lsdt[[i]]))
    if (length(columnCheck2_v) > 0) {
      keepCol_v <- colnames(data_lsdt[[i]])[-which(colnames(data_lsdt[[i]]) %in% mergeCol_v)]
      warning(sprintf("%s provided keep column(s) not in data %s. Grabbing all non-merge columns instead.\n\t%s\n\t%s\n",
                      length(columnCheck2_v), i, paste(columnCheck2_v, collapse = "; "), paste(keepCol_v, collapse = "; ")))
    } # fi
    

    ## Merge
    merge_dt <- merge(merge_dt,
                      data_lsdt[[i]][,mget(c(mergeCol_v, keepCol_v))],
                      by = mergeCol_v,
                      all = all_v, sort = sort_v)
    
    ## Update column names
    if (length(keepCol_v) > 1){
      colNames_v <- c(colNames_v, paste(names(data_lsdt)[i], keepCol_v, sep = "_"))
    } else {
      colNames_v <- c(colNames_v, names(data_lsdt)[i])
    } # fi
    
    ## Rename columns
    colnames(merge_dt) <- colNames_v
    
  } # for i
  return(merge_dt)
  
} # mergeDTs
