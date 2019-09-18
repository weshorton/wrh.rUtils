checkSamples <- function(dt1_dt, dt2_dt, col1_v = "Sample", col2_v = col1_v) {
  #' Check Samples
  #' @description Check two data.tables to see if they have different samples.
  #' Could be any column in theory.
  #' @param dt1_dt first data.table
  #' @param dt2_dt second data.table
  #' @param col1_v column in first data.table to check. Default is 'Sample'
  #' @param col2_v column in second data.table to check. Default is to be same as col1_v.
  #' @export
  
  ## Check both directions
  missingIn2_v <- setdiff(dt1_dt[[col1_v]], dt2_dt[[col2_v]])
  missingIn1_v <- setdiff(dt2_dt[[col2_v]], dt1_dt[[col1_v]])
  
  ## Notify
  if (length(missingIn1_v) > 0) {
    warning("The following were found in dt2, but not dt1: %s\n", paste(missingIn1_v, collapse = "; "))
  } # fi
  
  if (length(missingIn2_v) > 0) {
    warning("The following were found in dt1, but not dt2: %s\n", paste(missingIn2_v, collapse = "; "))
  } # fi
} # checkSamples