getPairwiseComparisons <- function(toCompare_v) {
  #' Get Pairwise Comparisons
  #' @description
  #' Create a list of pairwise comparisons from an input vector
  #' @param toCompare_v vector to run comparisons for
  #' @description Returns a non-repeating list of pairs, so order of toCompare_v matters.
  #' @return list. 
  #' @export
  
  pairComparisons_lsv <- list()
  for (i in 1:length(toCompare_v)) {
    if (i == length(toCompare_v)) next
    pairComparisons_lsv[[toCompare_v[i]]] <- toCompare_v[(i+1):length(toCompare_v)]
  } # for i
  
  return(pairComparisons_lsv)
  
} # getPairwiseComparisons
