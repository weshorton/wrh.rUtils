mkdir <- function(baseDir_v = NULL, 
                  newDir_v,
                  wd_v = F,
                  recursive_v = T){
  #' Creates new directory in which to write files
  #' @description
  #' Given a base directory and string, will check if specified directory exits, and make it if not. 
  #' Can also choose to set as working directory.
  #' @param baseDir_v Character string. Relative or absolute path to directory that will be parent of newly created directory. 
  #' If set to NULL (default), then will use working directory.
  #' @param newDir_v Character string. Name of new directory.
  #' @param wd_v boolean values determining if new directory should be set as working directory (T) or not (F).
  #' @param recursive_v boolean value determining if all elements of path other than last should be created. Passed to dir.create
  #' @return Character string of path to new directory. 
  #' Makes directory in file system. 
  #' If wd_v == TRUE, also sets as working directory.
  #' @examples 
  #' mkdir("~/", "mkdir_test")
  #' setwd("~/test/")
  #' mkdir(newDir_v = "anotherTest")
  #' mkdir("~/", "wdTest", wd_v = T)
  #' myDir_v <- mkdir("~/", "finalTest")
  #' @export
  
  ## Handle NULL baseDir_v
  if (is.null(baseDir_v)) baseDir_v <- getwd()
  
  ## Construct full path
  tempDir_v <- file.path(baseDir_v, newDir_v)
  
  ## Add trailing slash, if absent
  if (substring(tempDir_v, nchar(tempDir_v)) != "/") tempDir_v <- paste0(tempDir_v, "/")
  
  ## If it doesn't already exist, make it
  if (!dir.exists(tempDir_v)) dir.create(tempDir_v, recursive = recursive_v)
  
  ## Set as working directory, if specified
  if (wd_v) setwd(tempDir_v)
  
  ## Return
  invisible(tempDir_v)
  
} # mkdir
