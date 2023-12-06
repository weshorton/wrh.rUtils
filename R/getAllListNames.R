getAllListNames <- function(list_ls, names_lsv = NULL, counter_v = 1) {
  #' Get All List Names
  #' @description
    #' Given a named list, compile names from each level until no more lists.
  #' @param list_ls named list
  #' @param names_lsv holds output. Recursively updated.
  #' @param counter_v counter for recursion tracking
  #' @export
  
  if (is.null(names_lsv)) names_lsv <- list()
  if (is.logical(all.equal(class(list_ls), "list"))) {
    names_lsv[[counter_v]] <- names(list_ls)
    list_ls <- list_ls[[counter_v]]
    getAllListNames(list_ls = list_ls, names_lsv = names_lsv, counter_v = counter_v+1)
  } else {
    return(names_lsv)
  } # fi is list
  
} # getAllListNames