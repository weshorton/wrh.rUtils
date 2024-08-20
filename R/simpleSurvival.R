simpleSurvival <- function(data_dt, exprCol_v, timeCol_v = "OS.time", deathCol_v = "OS", method_v = "quartile", outputFile_v = NULL, name_v = NULL) {
  #' Simple Survival
  #' @description
    #' Calculate Kaplan Meier survival estimate and plot. Split groups based on expression.
  #' @param data_dt data.table with combined clinical and expression data
  #' @param exprCol_v column that contains expression information to use as a group divider.
  #' @param timeCol_v column that indicates survival time. Originally was days_to_death, may now be OS.time or DSS.time
  #' @param deathCol_v column that indicates death (vital) status. "DECEASED" == 1; "LIVING" == 0
  #' @param method_v which method to use to divide expression groups. quartile (default) will take the upper and lower quartiles. Median will split in half on the median.
  #' @param outputFile_v optional path to an output location if saving file is desired
  #' @param name_v optional name to add to the title. Usually the title of the dataset used as input.
  #' @details
    #' This function is specifically made for data that has been downloaded from the TCGA hub by the UCSCXenaTools package.
    #' [here](https://xenabrowser.net/datapages/?dataset=survival%2FBRCA_survival.txt&host=https%3A%2F%2Ftcga.xenahubs.net&removeHub=https%3A%2F%2Fxena.treehouse.gi.ucsc.edu%3A443) is an example of the BRCA
    #' dataset that is accessed by this tool. There are multiple survival classifications in these datasets:
    #'   1. OS: overall survival
    #'   2. DSS: disease-specific survival
    #'   3. DFI: disease-free interval
    #'   4. PFI: progression-free interval
  #' @return plot
  #' @export
  
  # Get cut-offs
  summary_v <- summary(data_dt[[exprCol_v]])
  
  if (method_v == "quartile") {
    low_v <- summary_v[2]
    up_v <- summary_v[5]
  } else if (method_v == "median") {
    low_v <- up_v <- summary_v[3]
  } else {
    stop(sprintf("Bad value for method_v. Can be either 'quartile' (default) or 'median'. %s was provided.\n", method_v))
  }
  
  # Add as new column
  data_dt$survCol <- character()
  data_dt[get(exprCol_v) < low_v, survCol := "low"]
  data_dt[get(exprCol_v) > up_v, survCol := "up"]
  
  # Subset
  data_dt <- data_dt[!is.na(survCol),]
  
  # Check
  nGrp_v <- length(unique(data_dt$survCol))
  if (nGrp_v < 2) return("Fewer than 2 groups.")
  
  # Survival
  survObj <- survival::Surv(time = data_dt[[timeCol_v]], event = data_dt[[deathCol_v]])
  survDiff <- survival::survdiff(survObj ~ data_dt$survCol)
  pval_v <- pchisq(survDiff$chisq, df = 1, lower = F)
  fit <- survival::survfit(survObj ~ data_dt$survCol)
  
  # Plot
  if (!is.null(outputFile_v)) pdf(file = outputFile_v)
  
  plotName_v <- ifelse(is.null(name_v), 
                       paste0("Survival Based on ", exprCol_v, " ", simpleCap(method_v), " Expression"),
                       paste0("Survival of ", name_v, " Based on\n", exprCol_v, " ", simpleCap(method_v), " Expression"))
  
  plot(fit, col=c("blue", "red"), frame = F, lwd = 2, 
       main = plotName_v,
       mark.time = T, cex.main = 2, cex.lab = 1.5, cex.axis = 1.2,
       xlab = gsub("\\.", " ", timeCol_v), ylab = "Prop. Survival")
  
  ## Add median survival lines
  medians_v <- summary(fit)$table[,'median']
  names(medians_v) <- gsub("^.*=", "", names(medians_v))
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
  legend(max_x, .73, legend = c(paste0("Low = ", records_v[["low"]]), paste0("High = ", records_v[["up"]])),
         bty  = "n", xjust = 1, col=c("blue", "red"), lwd=2, cex=1.2, title = "N. Patients")
  
  if (!is.null(outputFile_v)) dev.off()
  
} # simpleSurvival