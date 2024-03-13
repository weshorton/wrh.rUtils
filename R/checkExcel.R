# Check excel
checkExcel <- function(dir_v = NULL, wkbk1_v, wkbk2_v, outDir_v = NULL) {
  #' Check Excel
  #' @description
    #' Load two excel workbooks and check them for equivalence
  #' @param dir_v optional path to the directory that wkbk1 and wkbk2 are in
  #' @param wkbk1_v If dir_v == NULL, path to first workbook. If dir_v is set, then file name within dir_v that points to first workbook
  #' @param wkbk2_v If dir_v == NULL, path to second workbook. If dir_v is set, then file name within dir_v that points to second workbook
  #' @param outDir_v optional directory to write output if any is created. Default, NULL, will write to working directory.
  #' @details Read in all worksheets in each workbook and check them for equivalence
  #' @return Returns any different worksheets
  #' @export
  
  # Check names
  file1_v <- basename(wkbk1_v); name1_v <- tools::file_path_sans_ext(file1_v)
  file2_v <- basename(wkbk2_v); name2_v <- tools::file_path_sans_ext(file2_v)
  if (!is.logical(all.equal(file1_v, file2_v))) message(sprintf("File names differ. Not a problem, but check you're comparing what you want!\n\t%s\n\t%s\n",
                                                                file1_v, file2_v))
  
  # Check Files
  if (!is.null(dir_v)) {
    wkbk1_v <- file.path(dir_v, wkbk1_v)
    wkbk2_v <- file.path(dir_v, wkbk2_v)
  } # fi is.null(dir_v)
  
  # Read
  data1_lsdt <- wrh.rUtils::readAllExcel(wkbk1_v)
  data2_lsdt <- wrh.rUtils::readAllExcel(wkbk2_v)
  
  # Check lengths
  if (!is.logical(all.equal(length(data1_lsdt), length(data2_lsdt)))) warning(sprintf("Lengths differ.\n\twkbk1: %s\n\twkbk2: %s\n", length(data1_lsdt), length(data2_lsdt)))
  
  # Check names
  if (!is.logical(all.equal(names(data1_lsdt), names(data2_lsdt)))) {
    oneOnly_v <- setdiff(names(data1_lsdt), names(data2_lsdt))
    twoOnly_v <- setdiff(names(data2_lsdt), names(data1_lsdt))
    shared_v <- intersect(names(data1_lsdt), names(data2_lsdt))
    warning(sprintf("Worksheets differ.\n\tIn wkbk1 only (%s): %s\n\tIn wkbk2 only (%s): %s\n\tIn both workbooks (%s): %s\n",
                    length(oneOnly_v), paste0(oneOnly_v, collapse = "; "), 
                    length(twoOnly_v), paste0(twoOnly_v, collapse = "; "),
                    length(shared_v), paste0(shared_v, collapse = "; ")))
    oneOnly_lsdt <- data1_lsdt[oneOnly_v]
    twoOnly_lsdt <- data2_lsdt[twoOnly_v]
    shared_lslsdt <- list("wkbk1" = data1_lsdt[shared_v], "wkbk2" = data2_lsdt[shared_v])
  } else {
    oneOnly_lsdt <- list()
    twoOnly_lsdt <- list()
    shared_lslsdt <- list("wkbk1" = data1_lsdt, "wkbk2" = data2_lsdt)
  } # name difference
  
  # Check equivalence of shared sheets
  mismatchShared_lslsdt <- list("wkbk1" = list(), "wkbk2" = list())
  for (i in 1:length(shared_lslsdt$wkbk1)) {
    
    currSheet_v <- names(shared_lslsdt$wkbk1)[i]
    currOne_dt <- shared_lslsdt$wkbk1[[currSheet_v]]
    currTwo_dt <- shared_lslsdt$wkbk2[[currSheet_v]]
    
    if (!is.logical(all.equal(currOne_dt, currTwo_dt))) {
      
      mismatchShared_lslsdt$wkbk1[[currSheet_v]] <- currOne_dt
      mismatchShared_lslsdt$wkbk2[[currSheet_v]] <- currTwo_dt
      warning(sprintf("Sheet %s is different between wkbk1 and wkbk2.\n", currSheet_v))
      print(all.equal(currOne_dt, currTwo_dt))
      
    } # if one_dt != two_dt
    
  } # for each shared sheet
  
  # Construct output
  out_lslsdt <- list("wkbk1Only" = oneOnly_lsdt, "wkbk2Only" = twoOnly_lsdt, "wkbk1_sharedButNotEqual" = mismatchShared_lslsdt$wkbk1, "wkbk2_sharedButNotEqual" = mismatchShared_lslsdt$wkbk2)
  
  if (length(out_lslsdt$wkbk1Only) == 0 & length(out_lslsdt$wkbk2Only) == 0 &
      length(out_lslsdt$wkbk1_sharedButNotEqual) == 0 & length(out_lslsdt$wkbk2_sharedButNotEqual) == 0) {
    
    message(sprintf("Workbooks %s and %s match!\n", file1_v, file2_v))
    
  } else {
    
    message(sprintf("Workbooks %s and %s do not match.\n", file1_v, file2_v))
    
    if (is.null(outDir_v)) {
      
      message(sprintf("outDir_v argument is NULL. Writing mismatched files to working dir: %s\n", getwd()))
      outDir_v <- getwd()
      
    } # fi is.null(outDir_v)
    
    wrh.rUtils::myOpenXWriteWkbk(data_ls = out_lslsdt, file_v = file.path(outDir_v, paste0("compare_", name1_v, "-_-", name2_v, ".xlsx")))
    
  } # fi equal
  
} # checkExcel