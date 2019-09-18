fixVSegments <- function(v_v) {
  #' Fix V Segments
  #' @description V segments are simplified during pipeline. Some tools need them converted back to
  #' 'standard' format. Add 'TRB' to beginning and add '-' when needed. (E.g. V132 -> TRBV13-2)
  #' @param v_v character vector of segments to fix
  #' @export
  
  ## Add TRB
  v_v <- paste0("TRB", v_v)
  
  ## Fix others
  v_v <- gsub("TRBV121", "TRBV12-1", v_v)
  v_v <- gsub("TRBV122", "TRBV12-2", v_v)
  v_v <- gsub("TRBV131", "TRBV13-1", v_v)
  v_v <- gsub("TRBV132", "TRBV13-2", v_v)
  v_v <- gsub("TRBV133", "TRBV13-3", v_v)
  
  ## Return
  return(v_v)
}