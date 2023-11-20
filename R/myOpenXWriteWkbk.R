myOpenXWriteWkbk <- function(data_ls, file_v, append_v = T) {
  #' Write list to excel workbook
  #' @description Write a named list to a workbook using openxlsx. Sheet names are list names.
  #' @param data_ls list of tables to write
  #' @param file_v path to output file.
  #' @param append_v logical ind
  #' @param overwrite_v logical. Only matters if file_v already exists. If true, will append to existing workbook.
  #' If false, will overwrite existing workbook.
  #' @return writes to file
  #' @export
  
  ### Turn to list if not
  if (!is.logical(all.equal(class(data_ls), "list"))) {
    data_ls <- list(data_ls)
  }
  
  ### Make names if not present
  if (is.null(names(data_ls))) {
    names(data_ls) <- paste0("Sheet ", 1:length(data_ls))
    cat(sprintf("No sheet names given. Will be named Sheet 1, Sheet 2, etc."))
  } # fi is.null
  
  ### Check if file exists
  fileExists_v <- file.exists(file_v)
  
  ### Read in existing data and overwrite or not
  if (fileExists_v) {
    
    ### Read in existing data
    origData_lsdt <- wrh.rUtils::readAllExcel(file_v)
    
    ### Either add or overwrite it
    if (append_v) {
      origData_lsdt <- c(origData_lsdt, data_ls)
    } else {
      origData_lsdt <- data_ls
    } # fi append
    
  } else {
    origData_lsdt <- data_ls
  } # fi fileExists_v
  
  ### Check names of the added data
  emptyNames_v <- which(names(origData_lsdt) == "")
  if (length(emptyNames_v) > 0) {
    for (i in 1:length(emptyNames_v)) {
      cat(sprintf("Sheet %s missing a name. Naming: NoName%s", emptyNames_v[i], i))
      names(origData_lsdt)[emptyNames_v[i]] <- paste0("NoName", i)
    } # for i
  } # fi length > 0
  
  ### Check duplicate names
  dupNames_dt <- as.data.table(table(names(origData_lsdt)))[N>1]
  if (dupNames_dt[,.N] > 0) {
    for (i in 1:nrow(dupNames_dt)) {
      whichNames_v <- which(names(origData_lsdt) == dupNames_dt[i,V1])[-1]                         # get indices of the duplicates
      newNames_v <- names(origData_lsdt)[whichNames_v]                                             # extract them
      newNames_v <- sapply(1:length(newNames_v), function(x) paste0(newNames_v[x], LETTERS[x+1]))  # Modify them
      names(origData_lsdt)[whichNames_v] <- newNames_v                                             # Add back
    } # for i
  } # fi dupNames_v
  
  ### Create workbook
  wb <- openxlsx::createWorkbook()
  
  ### Load sheets
  invisible(lapply(seq_along(origData_lsdt), function(x) {
    openxlsx::addWorksheet(wb = wb, sheetName = names(origData_lsdt)[x])
    openxlsx::writeData(wb = wb, sheet = x, x = origData_lsdt[[x]])
  }))
  
  ### Save
  openxlsx::saveWorkbook(wb = wb, file = file_v, overwrite = T)
  
} # myOpenXWriteWkbk