getDims <- function(n_v) {
  #' Get Dimensions
  #' @description
    #' Get row/col dimensions for ggarrange
  #' @param n_v number of plots to get dimensions from
  #' @return vector of length two - c(row, col)
  #' @export
  
  ### 30 is max
  if (n_v > 30) stop("Only supports up to 30.")
  ### Make table
  ref_dt <- data.table("N" = 1:30,
                       "nRow" = c(rep(1, 2), rep(2, 4), rep(3, 6), rep(4, 8), rep(5, 10)),
                       "nCol" = c(1, rep(2, 3), rep(3, 5), rep(4, 7), rep(5, 9), rep(6, 5)))
  
  ### Get cols_v
  out_v <- c(ref_dt[N == n_v, nRow], ref_dt[N == n_v, nCol])
  
  ### Output
  return(out_v)
  
}