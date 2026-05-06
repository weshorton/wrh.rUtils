multiSave <- function(obj_ls, file_v, height_v = 7, width_v = 7, res_v = 600, which_v = c("pdf", "png", "svg")) {
  #' Multi Save
  #' @description Save image(s) in multiple file types
  #' @param obj_ls list of objects to print
  #' @param file_v file path to output. Can have any file suffix - suffix is controlled by which_v
  #' @param height_v height of file
  #' @param width_v width of file
  #' @param res_v resolution (for png)
  #' @param which_v vector containing any combo of 'pdf', 'png', and 'svg', indicating what types to save.
  #' @return NULL
  #' @export
  
  # Remove extension
  baseFile_v <- tools::file_path_sans_ext(file_v)
  
  # Check which_v
  if (!length(intersect(which_v, c("pdf", "png", "svg")))) stop("None of 'pdf', 'png', or 'svg' are in which_v argument.\n")
  
  # Save pdf
  if ("pdf" %in% which_v) {
    
    pdf(file = paste0(baseFile_v, ".pdf"), width = width_v, height = height_v, onefile = T)
    invisible(sapply(obj_ls, print))
    dev.off()
    
  } # fi pdf
  
  # Save pdf
  if ("png" %in% which_v) {
    
    png(file = paste0(baseFile_v, ".png"), res = res_v, width = width_v, height = height_v, units = "in")
    invisible(sapply(obj_ls, print))
    dev.off()
    
  } # fi png
  
  # Save svg
  if ("svg" %in% which_v) {
    
    svglite::svglite(filename = paste0(baseFile_v, ".svg"), width = width_v, height = height_v)
    invisible(sapply(obj_ls, print))
    dev.off()
    
  } # fi svg
} # multiSave