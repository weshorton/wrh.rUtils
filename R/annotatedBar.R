annotatedBar <- function(data_dt = NULL, plot_gg = NULL, x_v, y_v = NULL, 
                         stat_v = "count", position_v = "stack", yLab_v = "count", 
                         fill_v = NULL, fillColors_v = NULL, keepXAxis_v = F,
                         title_v = NULL, annot_lsv = NULL, annotColors_lsv, testTime_v = F, backgroundCol_v = "white") {
  #' Annotated Bar Plot
  #' @description Standard bar-plot with heatmap-like x-axis annotations
  #' @param data_dt melted data table for plotting (see details. Must supply this or plot_gg)
  #' @param plot_gg ggplot to annotate (see details. Must supply this or data_dt)
  #' @param x_v name of x-axis variable (usually Sample_ID, Treatment, etc.). Must be column in data_dt
  #' @param y_v name of y-axis variable (onnly used if stat_v = "identity")
  #' @param stat_v argument to 'stat' parameter of geom_bar. 'count' is default and does not use y_v. "identity" requires y_v to be set
  #' @param position_v argument to 'position' parameter of geom_bar. 'stack' (default) is counts; 'fill' turns to percentage by filling out of 1.
  #' @param yLab_v name of y-axis label.
  #' @param fill_v name of fill variable. Must be column in data_dt (only used if data_dt used)
  #' @param fillColors_v optional named color vector. Values are colors, names are values of data_dt[[fill_v]]
  #' @param keepXAxis_v logical indicating if x-axis labels should be included. Default is false. Should only be used if there aren't too many x-axis values
  #' @param title_v optional plot title
  #' @param annot_lsv list of annotations to add below the plot. e.g. annot_lsv = list("outName1" = "colName1", "outName2" = "colName2")
  #' @param annotColors_lsv list of colors for annotations. list element names are same as annot_lsv names; list elements are named color vectors, names are values of annot_lsv colNames in data_dt
  #' @param testTime_v logical indicating whether to see how long it takes to plot
  #' @param backgroundCol_v sent to panel.background element of ggplot theme. Needs to be white for a specific use case where I want to plot a percentage using fill_v = "fill", but I only want a subset of the identities to be shown.
  #' @details make a standard ggplot bar plot with extra annotations. Can provide a pre-existing plot instead of data.
  #' If a plot is provided, the x_v variable is still required, along with annot_lsv and annotColors_lsv. 
  #' Not required if plot is given: stat_v, position_v, fill_v, fillColors_v
  #' Note that data_dt overrules plot_gg. If both are provided, plot_gg is ignored
  #' @return list of gg objects output by ggarrange. Combo plot, data only, annotations only.
  #' @export
  
  if (testTime_v) tocs_lsv <- list()
  
  if (!is.null(data_dt)) {
    
    if (is.null(fill_v)) stop("Need to provide fill column.")
    
    ### Base plot depends on stat argument
    if (stat_v == "identity") {
      if (is.null(y_v)) stop("Must provide y_v argument if stat is identity.\n")
      plot_gg <- ggplot(data = data_dt, aes(x = !!sym(x_v), y = !!sym(y_v), fill = !!sym(fill_v)))
    } else if (stat_v == "count") {
      plot_gg <-  ggplot(data = data_dt, aes(x = !!sym(x_v), fill = !!sym(fill_v)))
    } else {
      stop("Can only handle 'identity' or 'count' for stat_v argument")
    } # fi
    
    ### Add the rest to the base plot
    plot_gg <- plot_gg +
      geom_bar(stat = stat_v, position = position_v) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + # make 0 the bottom
      theme(legend.position = "bottom", 
            plot.title = element_text(size = 26, hjust = 0.5),
            axis.text = element_text(size = 16),
            panel.background = element_rect(fill = backgroundCol_v)) +
      ylab(yLab_v)
    
    ### Add title
    if (!is.null(title_v)) plot_gg <- plot_gg + ggtitle(title_v)
    
    ### Add colors
    if (!is.null(fillColors_v)) plot_gg <- plot_gg + scale_fill_manual(values = fillColors_v, breaks = names(fillColors_v))
    
  } else if (!is.null(plot_gg)) {
    
    ### Extract data from plot
    data_dt <- as.data.table(plot_gg$data)
    
    ### Factorize x-axis to maintain order
    if (!is.factor(data_dt[[x_v]])) {
      data_dt[[x_v]] <- factor(data_dt[[x_v]], levels = unique(data_dt[[x_v]]))
    }
    
  } else {
    
    stop("Must provide either data_dt or plot_gg.\n")
    
  } # fi !is.null(data_dt)
  
  ### Begin annotations
  if (is.null(annot_lsv)) {
    warning("annot_lsv not provided...just make a regular barplot if you don't have annotations.\n")
    return(plot_gg)
  } else {
    
    if (testTime_v) tictoc::tic()
    
    # This doesn't seem to work...
    # ### Check legend size
    # tmp_gg <- ggplot_build(plot_gg)
    # legendVar_v <- tmp_gg$plot$labels$fill
    # legendVals_v <- levels(tmp_gg$plot$data[, get(legendVar_v)])
    # 
    # ### If many values in legend, make it 2 columns
    # if (length(legendVals_v) > 9) {
    #   plot_gg <- plot_gg + guides(fill = guide_legend(ncol = 2))
    # }
    
    ### Extract legend
    mainLegend_gg <- wrh.rUtils::g_legend(plot_gg + theme(legend.text = element_text(size = 18), legend.title = element_text(size = 20), 
                                                          plot.margin = unit(c(t = 0, r = 1, b = 1, l = 1), "cm")))
    plot_gg <- plot_gg + theme(legend.position = "none")
    
    ### Extract axis
    mainXAxis_gg <- ggpubr::as_ggplot(cowplot::get_x_axis(plot_gg + theme(plot.margin = unit(c(t = -0.1, r = 1, b = 1, l = 1), "cm"))))
    plot_gg <- plot_gg + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    ### Set margin
    plot_gg <- plot_gg + theme(plot.margin = unit(c(t = 1, r = 1, b = 0, l = 1), "cm"))
    #mainXAxis_gg <- mainXAxis_gg + theme(plot.margin = unit(c(-0.1, 1, -0.1, 1), "cm"))
    
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
      
      ### Thin legend margin
      ###currBar_gg <- currBar_gg + theme(legend.margin = margin(t = 0.1, r = 0.5, b = 1, l = 0.5, "cm"))
      
      ### Split legend
      currBarLegend_gg <- suppressWarnings(wrh.rUtils::g_legend(currBar_gg))
      currBar_gg <- currBar_gg + theme(legend.position = "none")
      
      ### Set margin
      currBar_gg <- currBar_gg + theme(plot.margin = unit(c(t = -0.1, r = 1, b = -0.1, l = 1), "cm"))
      
      ### Save
      bar_lsgg[[currName_v]] <- currBar_gg
      barLegend_lsgg[[currName_v]] <- currBarLegend_gg
      
      if (testTime_v) tocs_lsv[[currName_v]] <- tictoc::toc()$callback_msg
      
    } # for i
    
    ### New legend version
    tmp <- melt(data_dt[,mget(c(x_v, names(annot_lsv)))], measure.vars = names(annot_lsv))
    tmpColor <- unlist(annotColors_lsv)
    names(tmpColor) <- gsub("^.*\\.", "", names(tmpColor))
    tmpColor <- tmpColor[names(annotColors_lsv)]
    comboBarLegend_gg <- ggplot(tmp, aes(x = !!sym(x_v), y = 1, fill = variable)) + geom_boxplot() +
      scale_fill_manual(values = tmpColor, breaks = names(tmpColor)) + my_theme()
    comboBarLegend_gg <- comboBarLegend_gg + theme(legend.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, "cm"))
    comboBarLegend_gg <- get_legend(comboBarLegend_gg)
    
    ### Add axis
    if (keepXAxis_v) {
      toPlot_lsgg <- c(list("plot" = plot_gg), bar_lsgg, list("axis" = mainXAxis_gg))
      heights_v <- c(2, rep(0.1, length(bar_lsgg)), 1)
    } else {
      toPlot_lsgg <- c(list("plot" = plot_gg), bar_lsgg)
      heights_v <- c(2, rep(0.1, length(bar_lsgg)))
    }
    
    # toPlotLegend_lsgg <- c(list("plot" = mainLegend_gg), barLegend_lsgg)
    toPlotLegend_lsgg <- c(list("plot" = mainLegend_gg, "annot" = comboBarLegend_gg))
    
    if (length(bar_lsgg) > 3) heights_v[1] <- 1.9
    
    ### Get heights - old version
    # heights_v <- c(1, rep(0.03, length(bar_lsgg)))
    # if (keepXAxis_v) {
    #   heights_v <- c(heights_v, 0.08)
    #   heights_v[1] <- 0.9
    # } # add space for axis if keeping
    # if (length(bar_lsgg) > 3) heights_v[1] <- 0.95 # shorten plot if more than 3 annotation rows
    
    if (testTime_v) tictoc::tic()
    ### Combine plots
    combo_gg <- suppressWarnings(ggpubr::ggarrange(plotlist = toPlot_lsgg, ncol = 1, nrow = length(toPlot_lsgg), align = "v", heights = heights_v))
    
    if (testTime_v) tocs_lsv[["combo1"]] <- tictoc::toc()$callback_msg
    
    if (testTime_v) tictoc::tic()
    ### Combine legends
    if (length(toPlotLegend_lsgg) > 3) {nrow_v <- 2; ncol_v <- ceiling(length(toPlotLegend_lsgg)/2)} else {nrow_v <- 1; ncol_v <- length(toPlotLegend_lsgg)}
    comboLegend_gg <- ggpubr::ggarrange(plotlist = toPlotLegend_lsgg, nrow = nrow_v, ncol = ncol_v)
    
    ### Combine both
    finalCombo_gg <- ggpubr::ggarrange(plotlist = list(combo_gg, comboLegend_gg), ncol = 1, nrow = 2, heights = c(2, 1))
    #finalCombo_gg <- ggpubr::ggarrange(plotlist = list(combo_gg, comboLegend_gg), ncol = 1, nrow = 2, heights = c(1, 1))
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