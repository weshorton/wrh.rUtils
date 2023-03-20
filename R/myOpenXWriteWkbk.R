myOpenXWriteWkbk <- function(data_ls, file_v, overwrite_v = T) {
  #' Write list to excel workbook
  #' @description Write a named list to a workbook using openxlsx. Sheet names are list names.
  #' @param data_ls list of tables to write
  #' @param file_v path to output file.
  #' @param overwrite_v logical to overwrite workbook.
  #' @return writes to file
  #' @export
  
  ### Create workbook
  wb <- openxlsx::createWorkbook()
  
  ### Make names if not present
  if (is.null(names(data_ls))) {
    names(data_ls) <- paste0("Sheet ", 1:length(data_ls))
    cat(sprintf("No sheet names given. Will be named Sheet 1, Sheet 2, etc."))
  } # fi
  
  ### Load sheets
  lapply(seq_along(data_ls), function(x) {
    openxlsx::addWorksheet(wb = wb, sheetName = names(data_ls)[x])
    openxlsx::writeData(wb = wb, sheet = x, x = data_ls[[x]])
  })
  
  ### Save
  openxlsx::saveWorkbook(wb = wb, file = file_v, overwrite = overwrite_v)
} # myOpenXWriteWkbk