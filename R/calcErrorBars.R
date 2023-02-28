calcErrorBars <- function(data_dt, kind_v = "MSEM", col_v = "value", groupCol_v, fCol_v = NA, fVal_v = NA) {
  #' Calculate Error Bars
  #' @description Calculate error bars for given measurement column in data_dt.
  #' @param data_dt input data.table (most likely melted and ready for ggplot)
  #' @param kind_v which bars to make. MSEM = mean +/- SEM; MSD = mean +/- sd; MQ = median and lower/upper quartiles
  #' @param col_v which column in data_dt is the measurement that is used.
  #' @param groupCol_v which column to group calculations by. Treatment is common
  #' @param fCol_v which column to further group calcs by. Used if faceting the output.
  #' @param fVal_v values of fCol_v to group by.
  #' @details Will calculate center and upper/lower bounds of disribution by one of two methods 
  #' and add columns to data_dt labeled "Lower", "Middle", and "Upper". Will group calculated
  #' error bars by a grouping column and can further subdivide by a second column.
  #' @export
  
  ### Calculate if no facet provided
  if (is.na(fCol_v)) {
    
    if (kind_v == "MQ") {
      
      data_dt[, Lower := summary(get(col_v), na.rm = T)[2], by = get(groupCol_v)]
      data_dt[, Middle := summary(get(col_v), na.rm = T)[3], by = get(groupCol_v)]
      data_dt[, Upper := summary(get(col_v), na.rm = T)[5], by = get(groupCol_v)]
      
    } else if (kind_v %in% c("MSD", "MSEM")) {
      
      data_dt[, Middle := mean(get(col_v), na.rm = T), by = get(groupCol_v)]
      data_dt[, SD := sd(get(col_v), na.rm = T), by = get(groupCol_v)]
      data_dt[, n := length(get(col_v)), by = get(groupCol_v)]
      data_dt$SEM <- data_dt$SD / sqrt(data_dt$n)
      
      if (kind_v == "MSD") {
        data_dt$Lower <- data_dt$Middle - data_dt$SD
        data_dt$Upper <- data_dt$Middle + data_dt$SD
      } else {
        data_dt$Lower <- data_dt$Middle - data_dt$SEM
        data_dt$Upper <- data_dt$Middle + data_dt$SEM
      }
      
      data_dt[, (c("SD", "n", "SEM")) := NULL]
      
    } else {
      
      stop("kind_v must be 'MSEM', 'MSD', or 'MQ'")
      
    } # fi kind_v == "MQ"
    
  } else {
    
    if (kind_v == "MQ") {
      
      for (f_v in fVal_v) {
        data_dt[get(fCol_v) == f_v, Lower := summary(get(col_v), na.rm = T)[2], by = get(groupCol_v)]
        data_dt[get(fCol_v) == f_v, Middle := summary(get(col_v), na.rm = T)[3], by = get(groupCol_v)]
        data_dt[get(fCol_v) == f_v, Upper := summary(get(col_v), na.rm = T)[5], by = get(groupCol_v)]
      } # for
      
    } else if (kind_v %in% c("MSD","MSEM")) {
      
      for (f_v in fVal_v) {
        data_dt[get(fCol_v) == f_v, Middle := mean(get(col_v), na.rm = T), by = get(groupCol_v)]
        data_dt[get(fCol_v) == f_v, SD := sd(get(col_v), na.rm = T), by = get(groupCol_v)]
        data_dt[get(fCol_v) == f_v, n := length(get(col_v)), by = get(groupCol_v)]
      } # for
      
      data_dt$SEM <- data_dt$SD / sqrt(data_dt$n)
      
      if (kind_v == "MSD") {
        data_dt$Lower <- data_dt$Middle - data_dt$SD
        data_dt$Upper <- data_dt$Middle + data_dt$SD
      } else {
        data_dt$Lower <- data_dt$Middle - data_dt$SEM
        data_dt$Upper <- data_dt$Middle + data_dt$SEM
      }
    
      data_dt[, (c("SD", "n", "SEM")) := NULL]
      
    } else {
      
      stop("kind_v must be 'MSEM' or 'MQ'")
      
    } # fi kind_v == "MQ"
    
  } # fi is.na(fCol_v)
  
  return(data_dt)
  
} # calcErrorBars

