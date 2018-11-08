returnSessionInfo <- function(outDir_v = NULL, loadDirs_v = NULL, args_lsv = NULL){
  #' Output session information
  #' @description Print general R session info, current date/time, and loaded git repo versions to stdout or file. Note
  #' this uses sessionInfo() from base R, but an argument could be made to use session_info() from devtools.
  #' @param outDir_v optional directory to write information to. Default is to print to stdout
  #' @param loadDirs_v vector of directory paths that have been sourced in script. Note, if using the utility function
  #' sourceDir, these will be the same paths.
  #' @param args_lsv optional list of arguments used as input. Could be from optparse (easiest), or can hand-make a list of arguments.
  #' @return multiple lines of text printed to stdout or written to file
  #' @examples 
  #' returnSessionInfo()
  #' returnSessionInfo("~/Desktop/")
  #' @export
  
  ## Close all connections
  closeAllConnections()
  
  ## Get information
  info_v <- sessionInfo()
  date_v <- date()
  
  if (!is.null(loadDirs_v)){
    
    hashes_v <- sapply(loadDirs_v, function(x){
      setwd(x)
      system("git rev-parse --short HEAD", intern = T)
    })
    
    git_hashes_df <- as.data.frame(hashes_v)
    
  } else {
    
    git_hashes_df <- "No sourced repos"
    
  } # fi
  
  if (is.null(args_lsv)) args_lsv <- "No arguments given."
  
  
  ## Default: print to stdout
  if (is.null(outDir_v)){
    
    cat("Date of Run:\n"); print(date_v)
    cat("\nSession Info:\n\n"); print(info_v)
    cat("\nGit repo commits:\n"); print(git_hashes_df)
    cat("\nArguments Passed:\n"); print(args_lsv)
    
  ## Option: write to file  
  } else {
    
    ## Prepare file connection
    outData_v <- paste(strsplit(as.character(Sys.time()), split = ' ')[[1]][1:2], collapse = "_")
    file_v <- file.path(outDir_v, paste0(outData_v, "_session_info.txt"))
    file_conn <- file(file_v)
    
    ## Write date
    writeLines(c("Date of Run:\n", date_v), file_conn)
    
    ## Write session info
    file_conn <- file(file_v, "a")
    write(c("\nSession Info:\n", capture.output(info_v)), file_conn, append = T)
    file_conn <- file(file_v, "a")
    
    ## Write git repos
    write(c("\nGit repo commits:\n", capture.output(git_hashes_df)), file_conn, append = T)
    file_conn <- file(file_v, "a")
    
    ## Write arguments
    write(c("\nArguments Passed:\n", capture.output(args_lsv)), file_conn, append = T)
    close(file_conn)
  } # fi
  
  ## Close connections
  closeAllConnections()
  
} # returnSessionInfo