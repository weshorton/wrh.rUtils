mvFiles <- function(origDir_v, newDir_v, pattern_v = "*\\.log"){
  #' Move a batch of files to a new directory
  #' @description Originally developed to move venn diagram log output. Can be used to move any unique set of files to new directory.
  #' @param origDir_v path to original directory containing the files
  #' @param newDir_v one of two. (1) path to new directory where files should be moved to. (2) character string of new sub-directory of origDir_v to put files into.
  #' @param pattern_v some sort of regex to uniquely identify files to move.
  #' @return moves files at system level
  #' @export
  
  ## Make new directory as sub-directory, if specified
  if (length(strsplit(newDir_v, split = "/")[[1]]) == 1){
    newDir_v <- mkdir(origDir_v, newDir_v)
  } # fi
  
  ## Get files to move
  files_v <- list.files(origDir_v, pattern = pattern_v)
  
  ## Move them
  for (file_v in files_v) file.rename(from = file.path(origDir_v, file_v),
                                      to = file.path(newDir_v, file_v))
} # mvFiles