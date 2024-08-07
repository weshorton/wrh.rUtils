% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOutliers.R
\name{getOutliers}
\alias{getOutliers}
\title{Find Outliers}
\usage{
getOutliers(
  data_dt,
  col_v,
  type_v = c("mean", "mad", "dmad", "iqr"),
  by_v = NULL,
  mergeCol_v = NULL,
  scale_v = NULL,
  verbose_v = T
)
}
\arguments{
\item{data_dt}{data.table containing distribution to search for}

\item{col_v}{column name that contains data}

\item{type_v}{vector indicating which outlier method(s) to use. Can be any combination of "mean", "mad", "dmad", and "iqr".
Can also be 'lt:colName' 'gt:val:colName' indicating less than or greater than val in column colName}

\item{by_v}{column name indicating grouping variable. Default is no grouping.}

\item{mergeCol_v}{column name(s) to merge on if doing dmad.}

\item{scale_v}{vector indicating whether to scale data or not. NULL (default) is no scale. "log10", "log2", and "ln" are acceptable values.}

\item{verbose_v}{logical. TRUE - print results of each type_v in format "Found %s <type_v> outliers in %s samples". FALSE - no output}
}
\description{
Get outliers from data.
}
