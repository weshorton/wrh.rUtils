determineGrid <- function(obj, col_v = NULL, method_v = "grid", by_v = "column", verbose_v = T) {
  #' Determine Grid
  #' @description
    #' Determine rows and columns for a data.table for facet or a list for ggarrange 
    #' (or anything that takes rows and cols arguments)
  #' @param obj either a data.table or a list
  #' @param col_v if object is class "data.table" optional column to grab unique values from to determine grid.
  #' @param method_v either "grid" (default) for a compact grid (ggplot's facet_wrap default behavior), or a number indicating
  #' how intensely to skew the grid (e.g. 1, 2, etc.)
  #' @param by_v either "row" or "column". Determines which gets added first as grid size increases.
  #' @param verbose_v logical. will provide comments on what's happening to make sure you chose right params.
  #' @details
    #' Given an object of arbitrary dimension that needs to be output in a grid format,
    #' create desired row/column combinations based on two main methods - grid or long.
    #' Grid follows ggplot2:facet_wrap's behavior using ceiling(length/nrow(obj)) or celing(length/nrow(unique(obj[[col_v]])))
    #' Using the default by of "column" will match facet_wrap exactly, by = "row" will add rows first instead.
    #' 
    #' The long method is denoted by method = <integer>. In this case the ncol/nrow is determined the same as above,
    #' except the integer value provided will be added to the result, exaggerating either the row or column dimension accordingly
    #' ceiling(length(nrow(obj))). This method maxes out at the number of unique entries in obj
  #' @export
   
  ###
  ### Get class and check ~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Get class
  class_v <- class(obj)
  
  ### Simplify longer classes
  if (is.logical(all.equal(class_v, c("matrix", "array")))) {
    cat("Matrix provided\n")
    class_v <- "matrix"
  } else if (is.logical(all.equal(class_v, c("data.table", "data.frame")))) {
    cat("data.table provided\n")
    class_v <- "data.table"
  } 
  
  ### Check classes
  if (!class_v %in% c("data.table", "data.frame", "matrix", "list")) {
    stop(sprintf("Must provide one of data.table, data.frame, matrix, or list.\n%s was provided.\n",
                 paste(class_v, collapse = "; ")))
  } else {
    if (verbose_v) cat(sprintf("%s provided.\n", class_v))
  }
  
  ###
  ### Create fxn ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Determine method
  if (method_v == "grid") {
    if (verbose_v) cat("Using grid method.\n")
    add_v <- 0
    #fxn <- function(x) ceiling(sqrt(x))
  } else if (is.numeric(method_v) & length(method_v) == 1) {
    if (verbose_v) cat(sprintf("Using long method x%s.\n", method_v))
    add_v <- method_v
    #fxn <- function(x, method_v) ceiling(sqrt(x))+method_v
  } else {
    stop(sprintf("Method must be either 'grid' or a numeric vector of length 1.\n%s provided.\n",
                 paste(method_v, collapse = ", ")))
  } # fi
  
  ### Construct function
  fxn <- function(x, add_v = method_v) ceiling(sqrt(x))+add_v
  
  ###
  ### Determine values ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Get size
  if (class_v == "list") {
    size_v <- length(obj)
  } else {
    if (!is.null(col_v)) {
      size_v <- unique(obj[[col_v]])
    } else {
      size_v <- nrow(obj)
    } # fi is.null
  } # fi class
  
  ### Assign vals
  val1_v <- fxn(size_v)
  val2_v <- ceiling(size_v/val1_v)
  
  ### Assign to row and column
  if (by_v %in% c("row", "Row", "r")) {
    out_ls <- list("row" = val1_v, "column" = val2_v)
  } else if (by_v %in% c("column", "col", "Column", "Col", "c")) {
    out_ls <- list("row" = val2_v, "column" = val1_v)
  } else {
    stop(sprintf("by_v must be 'row' or 'column'.\n%s provided.\n", paste0(by_v, collapse = ", ")))
  } # fi
  
  ### Output
  return(out_ls)
  
} # determineGrid