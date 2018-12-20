### Needs improvement

plotSpecial <- function(grobs_ls, loc_lsv = list("vpPlot" = c(width = 0.65, height = 1, x = 0.3, y = 0.5),
                                                 "vpLeg" = c(width = 0.25, height = 0.7, x = 0.7, y = 0.88),
                                                 "vpTab" = c(width = 0.45, height = 0.3, x = 0.7, y = 0.6)),
                        file_v = NA, width_v = 7, height_v = 7){
  #' Plot ggplot with extra table grob
  #' @description Plot ggplot object, legend, and at least one other information table
  #' @param grobs_ls list of grobs to plot (must be same length as loc_lsv).
  #' @param loc_lsv list of viewport location arguments. Each list element must have 4 elements in order of width, height, x, y. Must be same length as grobs_ls. vpPlot, vpLeg, and vpTab are required.
  #' @param file_v file to output plot to. Must be pdf.
  #' @details Works differently depending on if main plot is a ggplot2 object or a pheatmap (or other) object. For ggplot2 object: Minimum length is 3 objects (plot, legend, table). Table should
  #' be created using myTableGrob or similar. Legend should be made using g_legend. Defaults work just fine. For pheatmap object, the pheatmap must be saved to an object by using
  #' myObj <- grid::grid.grabExpr(pheatmap(data_dt, options)). myObj is then the first grob in grobs_ls. As of right now, I can't extract the legend from pheatmap, so grobs_ls will actually only
  #' be a length of 2 as a minimum (heatmap and table). The loc_lsv can still work with the default 3 (the vpLeg will be removed), but will also work if just two are specified. When ggplot2 is used
  #' the loc_lsv names aren't important, but when pheatmap is used, the loc_lsv names must be vpPlot and vpTab.
  #' @return print plot to viewer or as pdf
  #' @export
  
  ## Make Viewports
  vp_ls <- sapply(loc_lsv, function(z) viewport(width = z[1], height = z[2], x = z[3], y = z[4]), simplify = F)
  
  ## Subset Viewports if not ggplot
  if (!"ggplot" %in% class(grobs_ls[[1]])){
    vp_ls[["vpLeg"]] <- NULL
  }
  
  ## Open file and/or new page
  if (!is.na(file_v)) pdf(file = file_v, width = width_v, height = height_v)
  grid.newpage()
  
  ## ggplot printing
  if ("ggplot" %in% class(grobs_ls[[1]])){
    print(grobs_ls[[1]] + theme(legend.position = "none"), vp = vp_ls[[1]])
  }
  
  ## non-ggplot printing
  if (!"ggplot" %in% class(grobs_ls[[1]])){
    pushViewport(vp_ls[[1]])
    grid.draw(grobs_ls[[1]])
  }
  
  ## Plot rest
  for (i in 2:length(vp_ls)){
    upViewport(0)
    pushViewport(vp_ls[[i]])
    grid.draw(grobs_ls[[i]])
  } # for i
  if (!is.na(file_v)) dev.off()
  
}