notice <- function(statement_v) {
  #' Print easy-to-find statement
  #' @description Print a specified statement after a large ASCII "LOOK" statement.
  #' @param statement_v Some sort of character vector. Can be a paste() call, an sprintf message, etc.
  #' @return print to console
  #' @examples 
  #' notice("This is a test")
  #' notice(paste("Another", "test", sep = "_"))
  #' @export
  
  cat("#   ### ### # #\n#   # # # # ##\n### ### ### # #\n\n")
  cat(statement_v)
  cat("\n\n\n")
} # notice