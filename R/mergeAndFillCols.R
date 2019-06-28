mergeAndFillCols <- function(data_dt, toMerge_v, outName_v = 'merged', search_lsv = NULL, replace_lsv = NULL) {
  #' Merge and Fill Columns
  #' @description Takes >= 2 columns with overlapping, incomplete data and merges them into 1 column.
  #' @param data_dt data.table with values
  #' @param toMerge_v vector of column names that should be merged
  #' @param outName_v name of the resulting merged column. Default is 'merged'.
  #' @param search_lsv list of values to search for within the columns. This is done before merge. 
  #' One example is the 'age' columns that have KJ in them. Can search for those and replace with NA to take the value in the other column.
  #' @param replace_lsv list of values to replace. Each element must match with search_lsv
  #' @export
  
  ## Search and replace
  if (!is.null(search_lsv)) {
    data_dt[, (toMerge_v) := lapply(.SD, function(x) {
      sapply(1:length(search_lsv), function(y) {
        search_v <- search_lsv[[y]]; replace_v <- replace_lsv[[y]]
        for (a in 1:length(search_v)) x <- gsub(as.character(search_v[a]), as.character(replace_v[a]), x)
        return(x)
    })}), .SDcols = toMerge_v]
  } # fi
  
  ## Paste together
  data_dt[, newCol := do.call(paste, c(.SD, sep = "_")), .SDcols = toMerge_v]
  
  ## Remove NAs
  data_dt$newCol <- gsub("_NA|NA_", "", data_dt$newCol)
  
  ## Remove original columns
  data_dt[, (toMerge_v) := NULL]
  
  ## Check new columns
  splitNewCol_v <- lapply(data_dt$newCol, function(x) strsplit(x, split = "_")[[1]])
  tooLong_v <- which(lapply(splitNewCol_v, length) > 1)
  if (length(tooLong_v) > 0) {
    warning(sprintf("At least one value in merged column may be incorrectly substituted. '_' was found, but could be part of true value.\nCheck these indices: %s\n", paste(tooLong_v, collapse = ", ")))
  }
  
  ## Rename new column
  colnames(data_dt)[colnames(data_dt) == "newCol"] <- outName_v
  
  ## Return
  return(data_dt)
  
} # mergeAndFillCols
