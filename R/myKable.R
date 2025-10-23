myKable <- function(data, format_v = "html", width_v=100, position_v="center", ...) {
  #' My Kable
  #' @description Function with a few common parameters I use already coded in
  #' @param data data.table/frame/matrix for plotting
  #' @param format_v argument to kable's `format`. Usually 'html' or 'latex', see ?knitr for others.
  #' @param width number used as percentage of document width
  #' @param position_v given to kable_styling position argument. Center is default. float_left, right, etc.
  #' @param ... extra arguments passed to knitr::kable
  #' @details
    #' Use "float_left" and "right" in conjunction to put multiple tables on the same line
    #' 
  #' @return kable object
  #' @export
  
  ### Format width
  width_v <- paste0("style='width:", width_v, "%;'")
  
  ### Handle position
  if (position_v != "center") {
    full_width_v <- T
  } else {
    full_width_v <- F
  }
  
  ### Make table
  myKable <- knitr::kable(data, format = format_v, align = "c", table.attr = width_v, ...) %>%
    kable_styling(position = position_v, 
                  full_width = full_width_v,
                  bootstrap_options = c("striped", "hover", "condensed"))
  
  ### Output
  return(myKable)
}
