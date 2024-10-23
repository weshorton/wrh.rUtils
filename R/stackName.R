stackName <- function(x_v, delim_v = "_") {
  #' Stack Name for Plot
  #' @description
  #' Given a string with a lot of a certain delimiter, replace that delimiter with \n 
  #' in certain spots. Made for MsigDB GO pathway names, but use where applicable.
  #' @param x_v long string with delimiter
  #' @param delim_v delimiter, usually _ for msigdb names
  #' @return same string, but a few of delim_v replaced with newline
  #' @export
  
  ### Split on delimiter
  split_v <- strsplit(x_v, split = delim_v)[[1]]
  
  ### Cut points depend on how many delims.
  len_v <- length(split_v)
  cut2_v <- ceiling(len_v/2); cut3_v <- ceiling(len_v/3)
  
  ### Apply cuts
  out_v <- ifelse(len_v < 3, paste(split_v, collapse = "_"),
                  ifelse(len_v < 8, paste0(paste(split_v[1:cut2_v], collapse = "_"), "\n", 
                                           paste(split_v[(cut2_v+1):length(split_v)], collapse = "_")),
                         paste0(paste(split_v[1:cut3_v], collapse = "_"), "\n", 
                                paste(split_v[(cut3_v+1):(2*cut3_v)], collapse = "_"), "\n", 
                                paste(split_v[((2*cut3_v)+1):length(split_v)], collapse = "_"))))
  
  return(out_v)
} # stackName_v
