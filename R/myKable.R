myKable <- function(data, width_v) {
  #' My Kable
  #' @description Function with a few common parameters I use already coded in
  #' @param data data.table/frame/matrix for plotting
  #' @param width number used as percentage of document width
  #' @return kable object
  #' @export
  
  ### Format width
  width_v <- paste0("style='width:", width_v, "%;'")
  
  ### Make table
  myKable <- knitr::kable(data, format = "html", align = "c", table.attr = width_v) %>%
    kable_styling(position = "center", bootstrap_options = c("striped", "hover", "condensed"))
  
  ### Output
  return(myKable)
}