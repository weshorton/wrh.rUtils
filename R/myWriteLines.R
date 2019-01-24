myWriteLines <- function(file_v, data_lsv, sep_v = " ") {
  #' My Write Lines
  #' @description Write a list of vectors to an output file. Each line is one element of list.
  #' @param file_v path to file
  #' @param data_lsv list of vectors
  #' @param sep_v Character vector used to separate elements of a vector. If NULL, each element of each vector will have its own line
  #' @return no output to stdout, just writes to file.
  #' @examples
  #' testOut_lsv <- list(rep(0, 5), LETTERS[1:10], "This is a test")
  #' myWriteLines("~/Desktop/multiLineTest.txt", testOut_lsv, sep = NULL)
  #' myWriteLines("~/Desktop/singleLineTest.txt", testOut_lsv)
  #' @export
  
  ## Open connection
  file_conn <- file(file_v)
  
  ## Write each line
  for (i in 1:length(data_lsv)) {
    
    ## Prepare output
    if (is.null(sep_v)) {
      out <- as.character(data_lsv[[i]])
    } else {
      out <- paste(data_lsv[[i]], collapse = sep_v)
    } # fi
    
    ## Write
    if (i == 1) {
      writeLines(out, file_conn)
    } else {
      file_conn <- file(file_v, "a")
      write(out, file_conn, append = T)
    } # fi
    
  } # for i
  
  ## Close Connections
  closeAllConnections()
  
} # myWriteLines
