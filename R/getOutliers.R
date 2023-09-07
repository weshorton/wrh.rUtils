getOutliers <- function(data_dt, col_v, type_v = c("mean", "mad", "dmad", "iqr"), by_v = NULL, mergeCol_v = NULL,
                        scale_v = NULL, verbose_v = T) {
  #' Find Outliers
  #' @description
    #' Find outliers in distributions using MAD or mean \+- 2 SD
  #' @param data_dt data.table containing distribution to search for 
  #' @param col_v column name that contains data
  #' @param type_v vector indicating which outlier method(s) to use. Can be any combination of "mean", "mad", "dmad", and "iqr".
  #' Can also be 'lt:colName' 'gt:val:colName' indicating less than or greater than val in column colName
  #' @param by_v column name indicating grouping variable. Default is no grouping.
  #' @param mergeCol_v column name(s) to merge on if doing dmad.
  #' @param scale_v vector indicating whether to scale data or not. NULL (default) is no scale. "log10", "log2", and "ln" are acceptable values.
  #' @param verbose_v logical. TRUE - print results of each type_v in format "Found %s <type_v> outliers in %s samples". FALSE - no output
  #' @export
  
  ### Scale data
  if (!is.null(scale_v)) {
    data_dt[,Scale := get(col_v)]
    col_v <- "Scale"
    if (scale_v == "log10") {
      data_dt[,(col_v) := log10(get(col_v))]
    } else if (scale_v == "log2") {
      data_dt[,(col_v) := log2(get(col_v))]
    } else if (scale_v == "ln") {
      data_dt[,(col_v) := log(get(col_v))]
    } else {
      stop("Acceptable values for scale_v are 'log10', 'log2', and 'ln'.")
    }
  }
  
  
  ### Calculate MAD version
  if ("mad" %in% type_v) {
    
    ### Calculate with or without grouping variable
    if (is.null(by_v)) {
      data_dt[,Median := median(get(col_v))]
      data_dt[,Dev := abs(get(col_v) - Median)]
      data_dt[,MAD := median(Dev)]
      data_dt[,Mscore := (0.6745*(get(col_v) - Median))/MAD]
    } else {
      data_dt[,Median := median(get(col_v)), by = mget(by_v)]
      data_dt[,Dev := abs(get(col_v) - Median)]
      data_dt[,MAD := median(Dev), by = mget(by_v)]
      data_dt[,Mscore := (0.6745*(get(col_v) - Median))/MAD]
    } # fi by_v
    
    ### Label outliers
    data_dt$madOutlier <- "no"
    # data_dt[Mscore < -3.5 | Mscore > 3.5, madOutlier := "yes"]
    data_dt[Mscore < -3.5, madOutlier := "yes"]
    data_dt[Mscore > 3.5, madOutlier := "yes"]
    
    ### Notify
    if (verbose_v) cat(sprintf("Found %s MAD outliers in %s samples.\n", data_dt[madOutlier == "yes",.N], data_dt[,.N]))
    
  } # fi mad
  
  ### Calculate mean version
  if ("mean" %in% type_v) {
    
    ### Calculate with or without grouping variable
    if (is.null(by_v)) {
      data_dt[,Mean := mean(get(col_v))]
      data_dt[,SD := sd(get(col_v))]
    } else {
      data_dt[,Mean := mean(get(col_v)), by = mget(by_v)]
      data_dt[,SD := sd(get(col_v)), by = mget(by_v)]
    }
    
    ### Finish calculations
    data_dt[,lowerBound := Mean - 2*SD]
    data_dt[,upperBound := Mean + 2*SD]
    
    ### Label outliers
    data_dt$meanOutlier <- "no"
    # data_dt[get(col_v) < lowerBound | get(col_v) > upperBound, meanOutlier := "yes"]
    data_dt[get(col_v) < lowerBound, meanOutlier := "yes"]
    data_dt[get(col_v) > upperBound, meanOutlier := "yes"]
    
    ### Notify
    if (verbose_v) cat(sprintf("Found %s mean outliers in %s samples.\n", data_dt[meanOutlier == "yes",.N], data_dt[,.N]))
    
  } # fi mean
  
  ### Calculate double MAD version
  if ("dmad" %in% type_v) {
    
    ### Calculate median
    if (is.null(by_v)) {
      data_dt[,Median := median(get(col_v))]
    } else {
      data_dt[,Median := median(get(col_v)), by = mget(by_v)]
    } # fi by_v
    
    ### Get deviation
    data_dt[,Dev := abs(get(col_v) - Median)]
    
    ### Split data into above and below median
    lower_dt <- data_dt[get(col_v) <= Median,]
    upper_dt <- data_dt[get(col_v) > Median,]
    
    ### Finish calculation
    if (is.null(by_v)) {
      lower_dt[,dMAD := median(Dev)]
      upper_dt[,dMAD := median(Dev)]
    } else {
      lower_dt[,dMAD := median(Dev), by = mget(by_v)]
      upper_dt[,dMAD := median(Dev), by = mget(by_v)]
    } # fi by_v
    
    ### Convert to score
    lower_dt[,DMscore := (0.6745*(get(col_v) - Median))/dMAD]
    upper_dt[,DMscore := (0.6745*(get(col_v) - Median))/dMAD]
    
    ### Combine
    final_dt <- rbind(lower_dt, upper_dt)
    
    ### Label outliers
    final_dt$dmadOutlier <- "no"
    # final_dt[DMscore < -3.5 | DMscore > 3.5, dmadOutlier := "yes"]
    final_dt[DMscore < -3.5, dmadOutlier := "yes"]
    final_dt[DMscore > 3.5, dmadOutlier := "yes"]
    
    ### Notify
    if (verbose_v) cat(sprintf("Found %s dMAD outliers in %s samples.\n", final_dt[dmadOutlier == "yes",.N], final_dt[,.N]))
    
    ### Merge with original
    if (is.null(mergeCol_v)) {
      stop("Must specify mergeCol_v in dmad method is to be used")
    } else {
      data_dt <- merge(data_dt, final_dt[,mget(c(mergeCol_v, "dMAD", "DMscore", "dmadOutlier"))], by = mergeCol_v)
    }
    
  } # fi dmad
  
  ### Calculate IQR version
  if ("iqr" %in% type_v) {
    
    ### Calculate with or without grouping variable
    if (is.null(by_v)) {
      data_dt[,Q1 := fivenum(get(col_v))[2]]
      data_dt[,Q3 := fivenum(get(col_v))[4]]
    } else {
      data_dt[,Q1 := fivenum(get(col_v))[2], by = mget(by_v)]
      data_dt[,Q3 := fivenum(get(col_v))[4], by = mget(by_v)]
    }
    
    ### Finish calculations
    data_dt[,IQR := Q3 - Q1]
    data_dt[,iqrLower := Q1 - 1.5*IQR]
    data_dt[,iqrUpper := Q3 + 1.5*IQR]
    
    ### Label outliers
    data_dt$iqrOutlier <- "no"
    # data_dt[get(col_v) < iqrLower | get(col_v) > iqrUpper, iqrOutlier := "yes"]
    data_dt[get(col_v) < iqrLower, iqrOutlier := "yes"]
    data_dt[get(col_v) > iqrUpper, iqrOutlier := "yes"]
    
    ### Notify
    if (verbose_v) cat(sprintf("Found %s IQR outliers in %s samples.\n", data_dt[iqrOutlier == "yes",.N], data_dt[,.N]))
    
  } # fi iqr
  
  ### Calculate any less/greater than
  lgt_v <- grep(":", type_v, value = T)
  if (length(lgt_v) > 0) {
    
    for (i in 1:length(lgt_v)) {
      
      ### Get info
      currDir_v <- strsplit(lgt_v[i], split = ":")[[1]][1]
      currVal_v <- as.numeric(strsplit(lgt_v[i], split = ":")[[1]][2])
      currCol_v <- strsplit(lgt_v[i], split = ":")[[1]][3]
      currOutlierCol_v <- paste(currCol_v, currVal_v, "Outlier", sep = ".")
      
      ### Start outlier column
      data_dt[[currOutlierCol_v]] <- "no"
      
      ### Update data
      if (currDir_v == "lt") data_dt[get(currCol_v) < currVal_v, (currOutlierCol_v) := "yes"]
      if (currDir_v == "gt") data_dt[get(currCol_v) > currVal_v, (currOutlierCol_v) := "yes"]
      
      ### Notify
      if (verbose_v) cat(sprintf("Found %s %s outliers in %s samples.\n", data_dt[get(currOutlierCol_v) == "yes",.N], lgt_v[i], data_dt[,.N]))
      
    } # for i
      
  } # fi lgt_v
  
  ### Return
  return(data_dt)
}
