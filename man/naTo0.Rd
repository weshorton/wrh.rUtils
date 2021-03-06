% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naTo0.R
\name{naTo0}
\alias{naTo0}
\title{NA to 0}
\usage{
naTo0(data_dt, cols_v = NA, verbose_v = F)
}
\arguments{
\item{data_dt}{data.table with NAs in some columns. Data.frames and matrices are accepted.}

\item{cols_v}{vector of either column indices (numeric) or column names (character) that have NAs to be replaced}

\item{verbose_v}{boolean values determining if verbose printing should be output.}
}
\value{
data.table with same dim(), but NAs are now 0 for all cols_v
}
\description{
Given a set of columns in a data.table, convert all NA values in those columns to 0
}
\examples{
data_df <- data.frame(A = 1:5, B = c(5,4,NA,2,1), C = c(3,NA,2,NA,NA), D = c(NA, NA, 1:3), stringsAsFactors = F)
data_mat <- as.matrix(data_df)
data_dt <- as.data.table(data_df)
naTo0(data_df, cols_v = "D")
naTo0(data_dt, cols_v = colnames(data_dt))
naTo0(data_mat, cols_v = c(1,3))
naTo0(data_df)
}
