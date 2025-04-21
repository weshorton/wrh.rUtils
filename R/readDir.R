readDir <- function(dir_v, files_v = NULL, pattern_v = NULL, names_v = NULL, which_v = "old") {
  #' Read Directory
  #' @description Read in all of the files in a given directory
  #' @param dir_v Path to directory containing files that should be read in
  #' @param files_v Vector containing the files within dir_v to read. See details for more info.
  #' @param pattern_v Regex pattern to search for files within dir_v to read. See details for more info.
  #' @param names_v Vector containing names to assign to each element. If blank, will use file name minus extension.
  #' @param which_v I added new functionality, but don't want to break anything because haven't tested. Change this to use new stuff.
  #' @details There are multiple ways to determine which files will be read. First, files_v will be checked. If
  #' this argument has been specified, then these are the files that will be read in. If it is blank (NA), then
  #' pattern_v will be checked. If this has been specified, then it will be used to find matching files. If both
  #' files_v and pattern_v are blank, then all of the files in the directory will be read.
  #' @return List of data.tables
  #' @examples
  #' # Make directory called "~/Desktop/readDirTest"
  #' write.table(matrix(1:100, nrow = 2), file = "~/Desktop/readDirTest/A.txt", row.names = F, quote = F, sep = '\t')
  #' write.table(matrix(1:100, nrow = 10), file = "~/Desktop/readDirTest/B.txt", row.names = F, quote = F, sep = '\t')
  #' write.table(matrix(1:100, nrow = 20), file = "~/Desktop/readDirTest/A.csv", row.names = F, quote = F, sep = ',')
  #' readDir(dir_v = "~/Desktop/readDirTest)
  #' readDir(dir_v = "~/Desktop/readDirTest, names_v = c("one", "two", "three))
  #' readDir(dir_v = "~/Desktop/readDirTest, files_v = c("B.txt", "A.csv"))
  #' readDir(dir_v = "~/Desktop/readDirTest, pattern_v = "*.txt")
  #' readDir(dir_v = "~/Desktop/readDirTest, pattern_v = "A.*")
  #' @export
  
  ### File determination
  if (is.null(files_v)) files_v <- list.files(dir_v, pattern = pattern_v)
  
  ### Read in
  if (which_v == "old") {
  data_lsdt <- lapply(files_v, function(x) fread(file.path(dir_v, x)))
  } else {
    data_lsdt <- lapply(files_v, function(x) {
      ext_v <- tools::file_ext(x)
      if (ext_v %in% c("xls", "xlsx")) {
        readAllExcel(file.path(dir_v, x))
      } else {
        fread(file.path(dir_v, x))
      } # fi
    })
  } # fi
  
  ### Add names
  if (is.null(names_v)) names_v <- tools::file_path_sans_ext(files_v)
  names(data_lsdt) <- names_v
  
  ### Return
  return(data_lsdt)
} # readDir