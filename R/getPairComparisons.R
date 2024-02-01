getPairwiseComparisons <- function(toCompare_v, unique_v = T) {
  #' Get Pairwise Comparisons
  #' @description
  #' Create a list of pairwise comparisons from an input vector
  #' @param toCompare_v vector to run comparisons for
  #' @param unique_v logical indicating if only unique comparisons should be run
  #' @description unique_v = T will return a non-repeating list of pairs, so order of toCompare_v matters. unique_v = F will return all pairs
  #' @return list. 
  #' @export
  
  pairComparisons_lsv <- list()
  
  if (unique_v) {
    for (i in 1:length(toCompare_v)) {
      if (i == length(toCompare_v)) next
      pairComparisons_lsv[[toCompare_v[i]]] <- toCompare_v[(i+1):length(toCompare_v)]
    } # for i
  } else {
    for (i in 1:length(toCompare_v)) {
      pairComparisons_lsv[[toCompare_v[i]]] <- toCompare_v[-i]
    } # for i
  }
  
  return(pairComparisons_lsv)
  
} # getPairwiseComparisons
