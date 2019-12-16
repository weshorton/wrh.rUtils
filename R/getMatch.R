getMatch <- function(counts1_v, counts2_v, checks_v) {
  #' Find match
  #' @description Given two matching vectors of different scales (e.g. raw and cpm),
  #' find out which values map with each other, e.g. which cpm corresponds to a raw count of 10?
  #' @param counts1_v same scale as the values in checks_v
  #' @param counts2_v scale that will find equivalent
  #' @param checks_v values to test
  
  idxOut_v <- sapply(checks_v, function(x) {
    idx_v <- NA
    one_v <- 0.98; two_v <- 1.02
    while (is.na(idx_v[1])) {
      idx_v <- which(counts1_v >= one_v*x & counts1_v <= two_v*x)
      one_v <- one_v - 0.03; two_v <- two_v + 0.03
    }
    diff_v <- abs(x - counts1_v[idx_v])
    finalIdx_v <- idx_v[which.min(diff_v)]
  })
  
  return(counts2_v[idxOut_v])
} # findMatch
