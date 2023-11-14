writeCSVorExcel <- function(obj, file_v, quote_v = F, rowNames_v = F) {
  #' Write CSV or Excel
  #' @description
  #' Will write out object according to file_v's extension
  #' @param obj either a data.table, data.frame, or matrix, or a list of one of those
  #' @param file_v path to file to write out. Can be either .csv or .xlsx
  #' @param quote_v logical. passed to quote argument of write.csv
  #' @param rowNames_v logical. passed to row.names argument of write.csv
  #' @details
  #' Will write .csv or .xlsx depending on the extension found in file_v
  #' Currently can only write a single table to csv. Additional functionality
  #' Would be to write a list of tables to a directory of csv's instead of an excel file.
  #' @return write out file.
  #' @export
  
  ### Get file extension and object class
  ext_v <- tools::file_ext(file_v)
  class_v <- class(obj)
  
  ### Simplify multi-classes
  class_v <- ifelse(is.logical(all.equal(class_v, c("matrix", "array"))), "matrix",
                    ifelse(is.logical(all.equal(class_v, c("data.table", "data.frame"))), "data.table", class_v))
  
  ### Handle edge case of providing a list with a csv file
  if (ext_v == "csv" & class_v == "list") {
    warning("File extension is .csv, but obj is a list.\n")
    if (length(obj) == 0) {
      stop("obj is an empty list...\n")
    } else if (length(obj) == 1) {
      warning("obj is a list of length one with csv file extension. Why not provide the table that's in the list?\n")
      obj <- obj[[1]]
    } else {
      warning("obj is a list greater than length one, but file extension is csv. Will write this as an excel file.
              if you want to write this as a directory of CSV's, make sure obj is a named list, and file_v is the directory path with no file extension.\n")
      ext_v <- "xlsx"
      file_v <- gsub("csv$", "xlsx", file_v)
    } # fi length(obj)
  } # fi ext_v == csv & class_v == "list"
  
  ### Handle CSV directory
  if (ext_v == "") {
    
    if (class_v != "list") {
      
      file_v <- file.path(file_v, "unNamedFile.csv")
      ext_v <- "csv"
      warning("No file extension in file_v, so assuming it's directory path. 
              List of tables not provided, will write this singular table to %s.\n", file_v)
      
    } # fi not list
    
    ### Get names
    names_v <- names(obj)
    
    ### Add extension and path
    if (is.null(names_v)) {
      
      warning("Trying to write obj as a directory of CSV's, but obj has no names, so nothing to call the files.
              Going to write each one as sheetN.csv as if they were in a workbook.\n")
      names_v <- paste0("sheet", 1:length(obj), ".csv")
      names_v <- file.path(file_v, names_v)
      
    } else {
      
      names_v <- file.path(file_v, paste0(names_v, ".csv"))
      
    } # handle names
    
    ### Write each
    sapply(1:length(names_v), function(i) {
      write.csv(x = obj[[i]],
                file = names_v[[i]],
                quote = quote_v,
                row.names = rowNames_v)
    })
    
  } else if (ext_v == "csv") {
    
    write.csv(x = obj,
              file = file_v,
              quote = quote_v, 
              row.names = rowNames_v)
    
  } else if (ext_v %in% c("xlsx", "xls")) {
    
    if (class_v == "list") {
      wrh.rUtils::myOpenXWriteWkbk(data_ls = obj,
                                   file_v = file_v)
    } else {
      openxlsx::write.xlsx(x = obj,
                           file = file_v)
    }
    
  } else {
    
    stop("File extension must be '', .csv, .xlsx, or .xls\n")
    
  } # fi ext_v
  
} # writeCSVorExcel