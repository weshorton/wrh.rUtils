kableHighlightColumns <- function(data_dt, cols_v, condition_v, bgColor_v = c("white", "red"), tColor_v = c("black", "white"), ...) {
  #' Highlight Columns in Kable
  #' @description Basic highlighting of each cell in a table based on a binary condition. Note that this uses
  #' cell_spec, which is deprecated in favor of column_spec, but I haven't figured out how to achieve this is column_spec yet.
  #' @param data_dt data.table to color columns of
  #' @param cols_v vector of column names to highlight cells of
  #' @param condition_v character string of condition to determine cell color. See details
  #' @param bgColor_v vector of length two containing color values to color cell backgrounds with
  #' @param tColor_v vector of length two containing color values to color cell text with
  #' @details Condition will be fed into an ifelse statement and each column provided in cols_v will be checked against it. For example,
  #' condition_v = "> 5" will be evaluated such that background = ifelse(data_dt[[col_v[i]]] > 5, bgColor_v[1], bgColor_v[2]), where i is
  #' the index of the current column. (And the same for text color).
  #' @return Output a kable with "standard" formatting updated with highlighted cells as defined in arguments.
  
  ### Set condition
  condition_v <- paste0("data_dt[[col_v]] ", condition_v)
  
  ### Copy data.table to leave original unaltered
  cdata_dt <- copy(data_dt)
  
  ### Adjust data
  for (col_v in cols_v) {
    whichCol_v = which(colnames(cdata_dt) == col_v)
    set(cdata_dt, j = col_v, value = cell_spec(x = cdata_dt[[col_v]],
                                               color = ifelse(eval(parse(text = condition_v)), tColor_v[1], tColor_v[2]),
                                               background = ifelse(eval(parse(text = condition_v)), bgColor_v[1], bgColor_v[2])))
  }
  
  ### Make kable
  out_kb <- kbl(cdata_dt, escape = F, align = "c", ...) %>%
    kable_styling(position = "center", 
                  bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F)
  
  ### Output
  return(out_kb)
  
} # kableHighlightColumns

