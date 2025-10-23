simpleSurvival <- function(data_dt, exprCol_v, timeCol_v = "OS.time", deathCol_v = "OS", method_v = "quartile", cutOff_v = NULL,
                           outputFile_v = NULL, name_v = NULL, plotType_v = "gg", return_v = F, printPlot_v = T) {
  #' Simple Survival
  #' @description
    #' Calculate Kaplan Meier survival estimate and plot. Split groups based on expression.
  #' @param data_dt data.table with combined clinical and expression data
  #' @param exprCol_v column that contains expression information to use as a group divider.
  #' @param timeCol_v column that indicates survival time. Originally was days_to_death, may now be OS.time or DSS.time
  #' @param deathCol_v column that indicates death (vital) status. "DECEASED" == 1; "LIVING" == 0
  #' @param method_v which method to use to divide expression groups. quartile (default), median, or tertile.
  #' @param cutOff_v optional pre-calculated cut-off value to use to create groups (instead of calculating within function call)
  #' @param outputFile_v optional path to an output location if saving file is desired
  #' @param name_v optional name to add to the title. Usually the title of the dataset used as input.
  #' @param plotType_v character vector. Either 'gg' or 'base' to indicate how to make the plot
  #' @param return_v logical indicating to return coxph and logrank test results (T) or not (F)
  #' @param printPlot_v logical indicating to output plot or not.
  #' @details
    #' This function is specifically made for data that has been downloaded from the TCGA hub by the UCSCXenaTools package.
    #' [here](https://xenabrowser.net/datapages/?dataset=survival%2FBRCA_survival.txt&host=https%3A%2F%2Ftcga.xenahubs.net&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443) is an example of the BRCA
    #' dataset that is accessed by this tool. There are multiple survival classifications in these datasets:
    #'   1. OS: overall survival
    #'   2. DSS: disease-specific survival
    #'   3. DFI: disease-free interval
    #'   4. PFI: progression-free interval
    #' method_v options:
    #'   1. quartile - will take the 1st and 3rd quartiles. inclusive
    #'   1. median - will split in half on the median. exclusive
    #'   1. tertile - will take the bottom 3rd and upper 3rd. inclusive
    #' plotType_v == 'gg' will use `survminer::ggsurvplot()` to make the plot
    #' plotType_v == 'base' will just use `base::plot()` and `lines()` to make the plot
  #' @return plot
  #' @export
  
  ###
  ### Wrangle ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  # Get cut-offs
  summary_v <- summary(data_dt[[exprCol_v]])
  
  if (method_v == "quartile") {
    
    if (is.null(cutOff_v)) {
      low_v <- summary_v[2]
      up_v <- summary_v[5]
    } else {
      if (length(cutOff_v) != 2) stop("quartile method_v selected, but provided cutOff_v does not have 2 values.")
      low_v <- cutOff_v[1]
      up_v <- cutOff_v[2]
    } # fi
    
    labs_v <- c("below 1st Q.", "above 3rd Q.")
  } else if (method_v == "median") {
    
    if (is.null(cutOff_v)) {
      low_v <- up_v <- summary_v[3]
    } else {
      if (length(cutOff_v) != 1) stop("median method_v selected, but provided cutOff_v does not have exactly 1 value.")
      low_v <- up_v <- cutOff_v
    } # fi
    
    labs_v <- c("below Med", "above Med")
  } else if (method_v == "tertile") {
    
    summary_v <- quantile(data_dt[[exprCol_v]], probs = c(0, 0.33, 0.67, 1))
    
    if (is.null(cutOff_v)) {
      low_v <- summary_v[2]
      up_v <- summary_v[3]
    } else {
      if (length(cutOff_v) != 2) stop("tertile method_v selected, but provided cutOff_v does not have 2 values.")
      low_v <- cutOff_v[1]
      up_v <- cutOff_v[2]
    } # fi
    
  } else {
    stop(sprintf("Bad value for method_v. Can be either 'quartile' (default), 'median', or 'tertile'. %s was provided.\n", method_v))
  }
  
  # Add as new column
  data_dt$survCol <- character()
  if (method_v == "median") {
    data_dt[get(exprCol_v) < low_v, survCol := "low"]
    data_dt[get(exprCol_v) > up_v, survCol := "up"]
  } else {
    data_dt[get(exprCol_v) <= low_v, survCol := "low"]
    data_dt[get(exprCol_v) >= up_v, survCol := "up"]
  } # fi method
  
  # Subset
  data_dt <- data_dt[!is.na(survCol),]
  
  # Check
  nGrp_v <- length(unique(data_dt$survCol))
  if (nGrp_v < 2) return("Fewer than 2 groups.")
  
  ###
  ### Survival ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Make survival object
  survObj <- survival::Surv(time = data_dt[[timeCol_v]], event = data_dt[[deathCol_v]])
  
  ### Fit model
  fit <- survival::survfit(survObj ~ data_dt$survCol)
  
  ### Run Cox
  cox <- survival::coxph(survObj ~ data_dt$survCol)
  coxP_v <- modelP(cox)
  
  ### Run log-rank
  survDiff <- survival::survdiff(survObj ~ data_dt$survCol)
  pval_v <- pchisq(survDiff$chisq, df = 1, lower = F)
  
  ### Get medians
  medians_v <- summary(fit)$table[,'median']
  names(medians_v) <- gsub("^.*=", "", names(medians_v))
  
  ###
  ### Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###

  ### Make title
  plotName_v <- ifelse(is.null(name_v), 
                       paste0("Survival Based on ", exprCol_v, " ", simpleCap(method_v), " Expression"),
                       paste0("Survival of ", name_v, " Based on\n", exprCol_v, " ", simpleCap(method_v), " Expression"))
  
  if (printPlot_v) {
    
    if (plotType_v == "gg") {
      
      plotName_v <- paste0(plotName_v, "\nMedian Surv - ", paste(paste(names(medians_v), medians_v, sep = ": "), collapse = "; "))
      
      surv_gg <- suppressWarnings(survminer::ggsurvplot(fit = survminer::surv_fit(survObj ~ survCol, data = data_dt),
                                                        data = data_dt,
                                                        pval = T,
                                                        title = plotName_v,
                                                        surv.median.line = "hv",
                                                        ggtheme = my_theme(),
                                                        risk.table = T,
                                                        xlab = timeCol_v,
                                                        legend.title = exprCol_v,
                                                        legend.labs = labs_v,
                                                        legend = c(0.8,0.8),
                                                        palette = c("blue", "red")))
      
      if (!is.null(outputFile_v)) {
        
        pdf(file = outputFile_v, onefile = F)
        print(surv_gg)
        dev.off()
        
      } else {
        
        print(surv_gg)
        
      } # fi
      
    } else if (plotType_v == "base") {
      
      if (!is.null(outputFile_v)) pdf(file = outputFile_v)
      
      ## Base plot
      plot(fit, col=c("blue", "red"), frame = F, lwd = 2, 
           main = plotName_v,
           mark.time = T, cex.main = 2, cex.lab = 1.5, cex.axis = 1.2,
           xlab = gsub("\\.", " ", timeCol_v), ylab = "Prop. Survival")
      
      ## Add median survival lines
      lines(c(0,max(medians_v)),c(0.5,0.5), lty = "dashed")
      mapply(function(x,y) lines(rep(x,2),c(0,0.5),col=y, lty="dashed"), medians_v, c("blue", "red"))
      
      ## Get number of records
      records_v <- summary(fit)$table[,'records']
      names(records_v) <- gsub("^.*=", "", names(records_v))
      
      ## Add legends
      max_x <- max(data_dt[[timeCol_v]], na.rm = T) * 0.95
      legend(max_x, .5, legend = paste0("p.val = ", round(pval_v, digits = 4)), bty = "n", xjust=1, cex = 1.2)
      legend(max_x, 1, legend = c(paste0("Low = ", medians_v[["low"]]), paste0("High = ", medians_v[["up"]])),
             bty = "n", xjust = 1, col=c("blue", "red"), lwd=2,  cex = 1.2, title = "Median Survival")
      # Not sure why I had this?
      # legend(max_x, .73, legend = c(paste0("Low = ", records_v[["low"]]), paste0("High = ", records_v[["up"]])),
      #        bty  = "n", xjust = 1, col=c("blue", "red"), lwd=2, cex=1.2, title = "N. Patients")
      
      if (!is.null(outputFile_v)) dev.off()
      
    } else {
      
      stop(sprintf("Only 'gg' and 'base' supported for plotType_v. %s provided.", plotType_v))
      
    } # fi plotType_v
  } # fi printPlot_v
  
  ### Output
  out_lsls <- list("cox" = list("pVal" = coxP_v, "result" = cox),
                   "logRank" = list("pVal" = pval_v, "result" = survDiff))
  
  if (return_v) return(out_lsls)
  
} # simpleSurvival