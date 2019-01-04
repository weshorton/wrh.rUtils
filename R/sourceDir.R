sourceDir <- function(dirPath_v, recurse_v=TRUE, verbose_v = F){
  #' Allows users to source all .R files in a given directory
  #' @description Enables convenient sourcing of multiple .R files simultaneously. 
  #' Optionally, sourcing can occur recursively in subdirectories.
  #' @param dirPath_v a vector of strings, each corresponding to a directory containing .R files you would like sourced
  #' @param recurse_v a boolean allowing the user to specify whether the sourcing should dive into subdirectories. 
  #' Unlike list.files(), the default is true
  #' @param verbose_v a boolean determining if sourced files should be printed.
  #' @return The list of full paths to .R scripts being sourced
  #' @examples
  #' sourceDir(dirPath_v=c("/path/to/UtilityFunctions", "/path/to/AbundanceFunctions"))
  #' @export
  
  ### Get list of files
  file_v = list.files(dirPath_v,full.names = TRUE, pattern="*.R$", ignore.case=T, recursive=recurse_v)
  
  ### Get names
  names_v <- basename(file_v)
  
  ### Attempt to source
  tryCatch({
    lapply(file_v, source)
  }, warning = function(w){
    print("Warning associated with sourcing at least one of the .R files")
  }, error = function(e){
    stop("Error: at least one of the .R files couldn't be sourced")
  }, finally = {
    print("Sourcing completed")
  })
  
  ### Print
  if (verbose_v) return(file_v)

} # sourceDir