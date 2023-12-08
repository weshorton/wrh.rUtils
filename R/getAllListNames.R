getAllListNames <- function(list_ls) {
  #' Get List Names Pretty
  #' @description
  #' Get all names from all elements of a nested list
  #' and output into a data.table
  #' @param list_ls named list to get names of
  #' @details Calls listNames() internally to get all of the names. Output
  #' is then wrangled into a data.table
  #' @return data.table with ncol = max list depth. One row for every list_ls$elementName$elementName$etc.
  #' If different elements have different depths, shallower elements are padded with NAs. Example:
  #' If list_ls$element1$element1Child1$element1Child1Grand1 and list_ls$element2$element2Child1 are
  #' both present, then they will be output: 
  #' [1] element1  element1Child1 element1Child1Grand1
  #' [2] element2 element2Child1 NA
  #' @export
  
  names_obj <- listNames(list_ls)
  namesClass_v <- class(names_obj)
  if (!is.null(dim(names_obj))) {
    names_dt <- melt(as.data.table(names_obj), measure.vars = colnames(as.data.table(names_obj)))[,"value"]
  } else if (is.logical(all.equal(class(namesClass_v), "character"))) {
    names_dt <- data.table("value" = names_obj)
  }
  
  names_dt$value <- gsub("^_", "", names_dt$value)
  maxCols_v <- max(apply(names_dt, 1, function(x) length(grep("\\_", unlist(strsplit(x, split = "")))))) + 1
  
  columns_lsv <- sapply(1:maxCols_v, function(x) {
    y <- sapply(names_dt$value, function(z) strsplit(z, split = "_")[[1]][x])
  }, simplify = F)
  
  out_dt <- as.data.table(do.call(cbind, columns_lsv))
  
  return(out_dt)
  
} # getAllListNames

listNames <- function(list_ls, prefix_v = "") {
  #' Get List Names
  #' @description
  #' recursively get all the names of a nested list
  #' @param list_ls named list
  #' @param prefix_v optional prefix to past in front of each element. Default is "" (blank)
  #' @return either a character vector or a matrix, depending on evenness of list depth
  #' @export
  
  if (is.logical(all.equal(class(list_ls), "list"))) {
    unname(unlist(mapply(listNames, list_ls, paste0(prefix_v, "_", names(list_ls)))))
  } else {
    prefix_v
  }
} # listNames
