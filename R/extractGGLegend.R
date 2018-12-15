g_legend <- function(a.gplot){
  #' Extract ggplot legend
  #' @description Extract legend as separate gtable from ggplot object
  #' @param a.gplot a ggplot object with a legend
  #' @return a gtable object of the legend
  #' @export
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # g_legend

