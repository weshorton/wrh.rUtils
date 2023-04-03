% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myOpenXWriteWkbk.R
\name{myOpenXWriteWkbk}
\alias{myOpenXWriteWkbk}
\title{Write list to excel workbook}
\usage{
myOpenXWriteWkbk(data_ls, file_v, overwrite_v = T)
}
\arguments{
\item{data_ls}{list of tables to write}

\item{file_v}{path to output file.}

\item{overwrite_v}{logical to overwrite workbook.}
}
\value{
writes to file
}
\description{
Write a named list to a workbook using openxlsx. Sheet names are list names.
}