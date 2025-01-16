annotatedBar <- function(data_dt, x_v, stat_v = "count", position_v = "stack", fill_v, fillColors_v = NULL,
                         title_v = NULL, annot_lsv = NULL, annotColors_lsv, testTime_v = F) {
  #' Annotated Bar Plot
  #' @description Standard bar-plot with heatmap-like x-axis annotations
  #' @param data_dt melted data table for plotting
  #' @param x_v name of x-axis variable (usually Sample_ID, Treatment, etc.). Must be column in data_dt
  #' @param stat_v argument to 'stat' parameter of geom_bar. Haven't played around with having this be anything but 'count'
  #' @param position_v argument to 'position' parameter of geom_bar. 'stack' (default) is counts; 'fill' turns to percentage by filling out of 1.
  #' @param fill_v name of fill variable. Must be column in data_dt
  #' @param fillColors_v optional named color vector. Values are colors, names are values of data_dt[[fill_v]]
  #' @param title_v optional plot title
  #' @param annot_lsv list of annotations to add below the plot. e.g. annot_lsv = list("outName1" = "colName1", "outName2" = "colName2")
  #' @param annotColors_lsv list of colors for annotations. list element names are same as annot_lsv names; list elements are named color vectors, names are values of annot_lsv colNames in data_dt
  #' @details make a standard ggplot bar plot with extra annotations
  #' @return list of gg objects output by ggarrange. Combo plot, data only, annotations only.
  #' @export
  
  if (testTime_v) tocs_lsv <- list()
  ### Make Base Plot
  plot_gg <- ggplot(data = data_dt, aes(x = !!sym(x_v), fill = !!sym(fill_v))) +
    geom_bar(stat = stat_v, position = position_v) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + # make 0 the bottom
    theme(legend.position = "bottom", 
          plot.title = element_text(size = 32, hjust = 0.5))
  
  ### Add title
  if (!is.null(title_v)) plot_gg <- plot_gg + ggtitle(title_v)
  
  ### Add colors
  if (!is.null(fillColors_v)) plot_gg <- plot_gg + scale_fill_manual(values = fillColors_v, breaks = names(fillColors_v))
  
  ### Begin annotations
  if (is.null(annot_lsv)) {
    warning("annot_lsv not provided...just make a regular barplot if you don't have annotations.\n")
    return(plot_gg)
  } else {
    
    if (testTime_v) tictoc::tic()
    
    ### Extract legend
    mainLegend_gg <- wrh.rUtils::g_legend(plot_gg)
    plot_gg <- plot_gg + theme(legend.position = "none")
    
    ### Extract axis
    mainXAxis_gg <- cowplot::get_x_axis(plot_gg)
    plot_gg <- plot_gg + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    ### Set margin
    plot_gg <- plot_gg + theme(plot.margin = unit(c(1, 1, 0, 1), "cm"))
    
    if (testTime_v) tocs_lsv[["legend"]] <- tictoc::toc()$callback_msg
    
    ### Build bar charts
    bar_lsgg <- barLegend_lsgg <- list()
    for (i in 1:length(annot_lsv)) {
      
      ### Get info
      currName_v <- names(annot_lsv)[i]
      currColName_v <- annot_lsv[[currName_v]]
      
      if (testTime_v) tictoc::tic()
      
      ### Make bar
      currBar_gg <- suppressWarnings(ggplot(data_dt, aes(x = !!sym(x_v), y = 1, fill = !!sym(currColName_v)))) +
        geom_bar(stat = "identity", width = 1) + 
        scale_y_continuous(limits = c(0,1)) +
        theme(panel.background = element_blank(), 
              axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(),
              legend.position = "bottom", 
              panel.grid = element_blank()) +
        scale_fill_manual(values = annotColors_lsv[[currName_v]], breaks = names(annotColors_lsv[[currName_v]])) +
        labs(fill = currName_v)
      
      ### Split legend
      currBarLegend_gg <- suppressWarnings(wrh.rUtils::g_legend(currBar_gg))
      currBar_gg <- currBar_gg + theme(legend.position = "none")
      
      ### Set margin
      currBar_gg <- currBar_gg + theme(plot.margin = unit(c(-0.1, 1, -0.1, 1), "cm"))
      
      ### Save
      bar_lsgg[[currName_v]] <- currBar_gg
      barLegend_lsgg[[currName_v]] <- currBarLegend_gg
      
      if (testTime_v) tocs_lsv[[currName_v]] <- tictoc::toc()$callback_msg
      
    } # for i
    
    ### Combine them into a list
    toPlot_lsgg <- c(list("plot" = plot_gg), bar_lsgg)
    toPlotLegend_lsgg <- c(list("plot" = mainLegend_gg), barLegend_lsgg)
    
    ### Get heights
    heights_v <- c(1, rep(0.05, length(bar_lsgg)))
    if (length(bar_lsgg) > 3) heights_v[1] <- 0.95
    
    if (testTime_v) tictoc::tic()
    ### Combine plots
    combo_gg <- suppressWarnings(ggpubr::ggarrange(plotlist = toPlot_lsgg, ncol = 1, nrow = length(toPlot_lsgg), align = "v", heights = heights_v))
    
    if (testTime_v) tocs_lsv[["combo1"]] <- tictoc::toc()$callback_msg
    
    if (testTime_v) tictoc::tic()
    ### Combine legends
    if (length(toPlotLegend_lsgg) > 3) {nrow_v <- 2; ncol_v <- ceiling(length(toPlotLegend_lsgg)/2)} else {nrow_v <- 1; ncol_v <- length(toPlotLegend_lsgg)}
    comboLegend_gg <- ggpubr::ggarrange(plotlist = toPlotLegend_lsgg, nrow = nrow_v, ncol = ncol_v)
    
    ### Combine both
    finalCombo_gg <- ggpubr::ggarrange(plotlist = list(combo_gg, comboLegend_gg), ncol = 1, nrow = 2, heights = c(5, 1))
    if (testTime_v) tocs_lsv[["combo2and3"]] <- tictoc::toc()$callback_msg
    
    ### Make outputt
    if (testTime_v) {
      out_ls <- list("annotPlot" = finalCombo_gg, "plot" = combo_gg, "legend" = comboLegend_gg, "tocs" = tocs_lsv)
    } else {
      out_ls <- list("annotPlot" = finalCombo_gg, "plot" = combo_gg, "legend" = comboLegend_gg)
    } # fi
    
    return(out_ls)
    
  } # fi is.null(annot_lsv)
} # annotatedBar