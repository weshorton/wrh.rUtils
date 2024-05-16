readAllExcel <- function(file_v, ...) {
  #' Read All Excel
  #' @description Read in all of the sheets in an excel workbook as a list of data.tables
  #' @param file_v path to excel file. Can be '.xlsx' or '.xls'
  #' @param ... extra options to pass to readxl::read_excel
  #' @return list of data.tables
  #' @export
  
  ## Get sheets
  sheets_v <- readxl::excel_sheets(file_v)
  
  ## Read in
  data_lsdt <- lapply(sheets_v, function(x) {
    # as.data.table(readxl::read_excel(path = file_v, sheet = x))
    setDT(readxl::read_excel(path = file_v, sheet = x, ...))
  })
  
  ## Add names
  names(data_lsdt) <- sheets_v
  
  ## Return
  return(data_lsdt)
}