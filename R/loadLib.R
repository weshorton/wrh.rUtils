loadLib <- function(lib_v, silent_v = T) {
  #' Load Libraries
  #' @description Load vector of libraries
  #' @param lib_v character vector containing names of libraries to load
  #' @param silent_v boolean determining if library load messages should be printed or not.
  #' @return load libraries and (optionally) print any messages to console
  #' @examples
  #' loadLib(c("data.table", "ggplot2", "dplyr"))
  #' @export
  
  if (silent_v) {
    invisible(lapply(lib_v, function(x) suppressMessages(suppressWarnings(require(x, character.only = T)))))
  } else {
    invisible(lapply(lib_v, function(x) require(x, character.only = T)))
  } # fi
}
