myRGB <- function(color_lsv, maxColorValue_v = 255) {
  #' my RGB
  #' @description
  #' rgb() function with a vector of length 3 instead of 3 separate args
  #' @param color_lsv list of vectors of length 3 containing rgb values
  #' @param maxColorValue_v passed to rgb's maxColorValue
  #' @details
  #' Additional details...
  #' @return list of hex colors. if only 1, will just be a vector
  #' @export
  
  ### Assign
  if (class(color_lsv) == "list") {
    out_lsv <- sapply(color_lsv, function(x) {
      grDevices::rgb(red = x[1], green = x[2], blue = x[3], maxColorValue = maxColorValue_v)
    }, simplify = F)
  } else if (class(color_lsv) == "numeric") {
    out_lsv <- grDevices::rgb(red = color_lsv[1], green = color_lsv[2], blue = color_lsv[3], maxColorValue = maxColorValue_v)
  } else {
    stop("Object must be list or numeric.\n%s provided.\n", paste(class(color_lsv), collapse = "; "))
  } # fi
  
  ### Output
  return(out_lsv)
}