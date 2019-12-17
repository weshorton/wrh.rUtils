cpmThresh <- function(raw_dt, cpm_dt, metaCols_v = "Gene", rawTest_v = c(10,20,30),
                      indPlot_v = F, histPlot_v = T, densPlot_v = T, boxPlot_v = T, plotDir_v = NA,
                      meta_dt, plotCols_lsv = list()) {
  #' Plot CPM Thresh
  #' @description Plot the cpm counts for a dataset to determine a potential filter threshold.
  #' @param raw_dt data.table of raw values. rows = genes; columns = samples. 
  #' @param cpm_dt data.table of cpm values. rows = genes; columns = samples.
  #' @param metaCols_v columns that should not be used for cpm values. Default is "Gene"
  #' @param rawTest_v vector of numbers to use as raw values to test for cpm equivalents.
  #' @param indPlot_v logical. TRUE - print raw-cpm comparison plot for each sample; FALSE - do not print plot.
  #' @param histPlot_v logical. TRUE - print summary histogram plot; FALSE - do not print
  #' @param densPlot_v logical. TRUE - print density plot; FALSE - do not print
  #' @param boxPlot_v logical. TRUE - print boxplot; FALSE - do not print
  #' @param plotDir_v character vector of where to save plots.
  #' @param meta_dt data.table containing metadata information. Should have column named "sample" that matches colnames of raw_dt/cpm_dt
  #' @param plotCols_lsv list of metadata columns to use to change plots. list element name is the plotting parameter (e.g. color or fill),
  #' while list element value is the column name (e.g. batch, treatment)
  #' @return Named vector where values are cpm and names are the corresponding raw value.
  #' @export
  
  require(ggplot2)
  
  ## Get measure columns
  measureCols_v <- setdiff(colnames(raw_dt), metaCols_v)
  
  ## Set variables
  mapCPM_mat <- matrix(nrow = length(measureCols_v), ncol = length(rawTest_v))
  mapRaw_v <- numeric(length = length(measureCols_v))
  if (is.na(plotDir_v)) plotDir_v <- getwd()
  
  ## Run for each
  for (i in 1:length(measureCols_v)) {
    
    ## Get name and data
    currName_v <- measureCols_v[i]
    currRaw_v <- raw_dt[[currName_v]]
    currCPM_v <- cpm_dt[[currName_v]]
    
    ## Get cpm values corresponding to raw
    ## What if none of them are equal??
    currVert_v <- mapCPM_mat[i,] <- findMatch(counts1_v = currRaw_v, counts2_v = currCPM_v, checks_v = rawTest_v)
    mapCPM_mat[i,] <- currVert_v
    
    ## Get raw value corresponding with cpm of 1
    currHoriz_v <- findMatch(counts1_v = currCPM_v, counts2_v = currRaw_v, checks_v = 1)
    mapRaw_v[i] <- currHoriz_v
    
    ## Add to other vars
    currRawTest_v <- c(rawTest_v, round(mapRaw_v[i]))
    currVert_v <- c(currVert_v, 1)
    
    ## Plot
    if (indPlot_v) {
      ## Make legend
      rawLeg_v <- paste0("raw", currRawTest_v)
      cpmLeg_v <- paste0("cpm", round(currVert_v, digits = 3))
      currLegend_v <- mapply(function(x,y) paste(x,y,sep = " - "), rawLeg_v, cpmLeg_v)
      
      pdf(file = file.path(plotDir_v, paste0(currName_v, ".pdf")))
      plot(x = currCPM_v, y = currRaw_v,
           xlim = c(0,2), ylim = c(0, 50),
           xlab = "CPM", ylab = "Raw",
           main = currSample_v)
      sapply(seq_along(currRawTest_v), function(x) abline(h = currRawTest_v[x], col = colors_v[x]))
      sapply(seq_along(currVert_v), function(x) abline(v = currVert_v[x], col = colors_v[x]))
      legend("topright", legend = currLegend_v, col = colors_v, lwd = 2, bty = 'n')
      graphics.off()
    } # fi
  } # for i
  
  ## Update table names
  rownames(mapCPM_mat) <- measureCols_v
  colnames(mapCPM_mat) <- rawTest_v
  
  ## Get mean raw value that corresponds to 1 and update rawtest
  meanRaw_v <- mean(mapRaw_v)
  rawTest_v <- c(rawTest_v, meanRaw_v)
  
  ## Get average cpm
  meanCPMCut_v <- c(colMeans(mapCPM_mat), 1)
  names(meanCPMCut_v)[length(meanCPMCut_v)] <- meanRaw_v
  
  ## Boxplot
  if (boxPlot_v) {
    ## Make data
    merge_dt <- merge(convertDFT(mapCPM_mat, newName_v = "sample"), meta_dt, by = "sample", sort = F)
    melt_dt <- melt(merge_dt, measure.vars = colnames(mapCPM_mat))
    ## Make plot
    box_gg <- ggplot(data = melt_dt, aes(x = variable, y = value)) + 
      geom_boxplot() +
      big_label() + labs(x = "Raw Value", y = "CPM Value") + ggtitle("Raw to CPM relationship")
    ## Add details
    if (length(plotCols_lsv) > 0) {
      for (i in 1:length(plotCols_lsv)) {
        if (names(plotCols_lsv)[i] == "fill") {
          box_gg <- box_gg + aes_string(fill = plotCols_lsv[[i]])
        } else if (names(plotCols_lsv)[i] == "color") {
          box_gg <- box_gg + aes_string(color = plotCols_lsv[[i]])
        } # fi
      } # for
    } # fi
    ggsave(box_gg, filename = file.path(plotDir_v, "rawCPM_boxplot.pdf"), width = 7, height = 7)
  } # fi
  
  ## Histogram
  if (histPlot_v) {
    logmeanCPM_v <- log(rowMeans(cpm_dt[,mget(measureCols_v)]))
    pdf(file = file.path(plotDir_v, "logCPM_histogram.pdf"))
    hist(logmeanCPM_v, breaks = 100, xlab = "log10(mean(cpm))", main = "Log10 CPM Hist")
    graphics.off()
  } # fi
  
  ## Density
  if (densPlot_v) {
    log10CPM_lsv <- lapply(measureCols_v, function(x) log10(cpm_dt[[x]]))
    pdf(file = file.path(plotDir_v, "logCPM_density.pdf"))
    plotDensity(log10CPM_lsv,
                main_v = "Log 10 Densities",
                x_v = "Log10(cpm)",
                names_v = measureCols_v,
                colors_ls = rep("black", length(log10CPM_lsv)))
    lapply(meanCPMCut_v, function(x) abline(v = x))
    graphics.off()
  } # fi
  
  ## Return val
  return(meanCPMCut_v)
  
} # cpmThresh