calcOverlap <- function(set1_v, set2_v, total_v = NA, digits_v = 4) {
  #' Calculate Overlap
  #' @description Calculate the Jaccard Index of two different sets. Can also calculate BUB.
  #' @param set1_v Character vector of values to compare
  #' @param set2_v Character vector of values to compare
  #' @param total_v Optional global set to compare set1 and set2 against. Only used for BUB. 
  #' If 'NA' (default), will just calculate Jaccard index.
  #' @param digits_v how many digits to round output to. Default is 4.
  #' @details This will calculate overlap between two sets by comparing the identities of each element within
  #' set1 and set2. Jaccard Index is the Intersection of the two sets divided by the union of the two sets. BUB
  #' (Baroni Urbani Binary Index) is similar, but compares against a global set (rather than just the union of 1 and 2).
  #' This allows for weighting two samples as more similar if a they also cover a greater amount of the global set.
  #' @examples 
  #' A <- 1:100
  #' B <- 25:75
  #' C <- 60:150
  #' TOT <- union(union(A, B), C)
  #' calcOverlap(A, B)
  #' calcOverlap(A, C)
  #' calcOverlap(A, B, TOT)
  #' calcOverlap(A, C, TOT)
  #' @export
  
  ### Calculate intersection and union
  intersect_v <- intersect(set1_v, set2_v)
  union_v <- union(set1_v, set2_v)
  
  ### Calculate jaccard
  jaccard_v <- round( length(intersect_v) / length(union_v), digits = digits_v)
  
  ### If total is NA, then becomes union
  if (is.na(total_v[1])) { total_v <- union_v }
  
  ### Calculate number absent (0 if total_v is NA)
  absent_v <- setdiff(total_v, union_v)
    
  ## BUB numerator and denominator
  numerator_v <- length(intersect_v) + sqrt(length(intersect_v) * length(absent_v))
  denominator_v <- length(union_v) + sqrt(length(intersect_v) * length(absent_v))
    
  ## Calculate (same as jaccard if total_v was NA)
  bub_v <- round(numerator_v / denominator_v, digits = digits_v)
    
  ## Make output
  out_v <- c("Jaccard" = jaccard_v, "BUB" = bub_v)
  
  ## Return output
  return(out_v)
} # calcOverlap
