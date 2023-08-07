getOutliers <- function(data_dt, col_v, type_v = c("mean", "mad", "dmad"), by_v = NULL, mergeCol_v = NULL) {
  #' Find Outliers
  #' @description find outliers in distributions using MAD or mean +- 2 SD
  #' @param data_dt data.table containing distribution to search for 
  #' @param col_v column name that contains data
  #' @param type_v vector indicating which outlier method(s) to use. Can be any combination of "mean", "mad", and "dmad"
  #' @param by_v column name indicating grouping variable. Default is no grouping.
  #' @param mergeCol_v column name to merge on if doing dmad.
  #' @export
  
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
    data_dt[Mscore < -3.5 | Mscore > 3.5, madOutlier := "yes"]
    
    ### Notify
    cat(sprintf("Found %s MAD outliers in %s samples.\n", data_dt[madOutlier == "yes",.N], data_dt[,.N]))
    
  } # fi mad
  
  ### Calculate mean version
  if ("mean" %in% type_v) {
    
    ### Calculate with or without grouping variable
    if (is.null(by_v)) {
      data_dt[,Mean := mean(get(col_v))]
      data_dt[,SD := sd(get(col_v))]
    } else {
      data_dt[,Mean := mean(get(col_v)), by = get(by_v)]
      data_dt[,SD := sd(get(col_v)), by = get(by_v)]
    }
    
    ### Finish calculations
    data_dt[,lowerBound := Mean - 2*SD]
    data_dt[,upperBound := Mean + 2*SD]
    
    ### Label outliers
    data_dt$meanOutlier <- "no"
    data_dt[get(col_v) < lowerBound | get(col_v) > upperBound, meanOutlier := "yes"]
    
    ### Notify
    cat(sprintf("Found %s mean outliers in %s samples.\n", data_dt[meanOutlier == "yes",.N], data_dt[,.N]))
    
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
    
    ### Label outliers
    data_dt$madOutlier <- "no"
    data_dt[Mscore < -3.5 | Mscore > 3.5, madOutlier := "yes"]
    
    ### Combine
    final_dt <- rbind(lower_dt, upper_dt)
    
    ### Label outliers
    final_dt$dmadOutlier <- "no"
    final_dt[DMscore < -3.5 | DMscore > 3.5, dmadOutlier := "yes"]
    
    ### Notify
    cat(sprintf("Found %s MAD outliers in %s samples.\n", final_dt[dmadOutlier == "yes",.N], final_dt[,.N]))
    
    ### Merge with original
    data_dt <- merge(data_dt, final_dt[,mget(c(mergeCol_v, "dMAD", "DMscore", "dmadOutlier"))])
    
  } # fi dmad
  
  ### Return
  return(data_dt)
}