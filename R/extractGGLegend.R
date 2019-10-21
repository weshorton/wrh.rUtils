g_legend <- function(a.gplot){
  #' Extract ggplot legend
  #' @description Extract legend as separate gtable from ggplot object
  #' @param a.gplot a ggplot object with a legend
  #' @return a gtable object of the legend
  #' @export
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(legend)
  } else {
    message("No legend grob in provided ggplot")
  }
} # g_legend

