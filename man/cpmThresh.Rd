% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpmThresh.R
\name{cpmThresh}
\alias{cpmThresh}
\title{Plot CPM Thresh}
\usage{
cpmThresh(
  raw_dt,
  cpm_dt,
  metaCols_v = "Gene",
  rawTest_v = c(10, 20, 30),
  indPlot_v = F,
  histPlot_v = T,
  densPlot_v = T,
  boxPlot_v = T,
  plotDir_v = NA,
  meta_dt,
  plotCols_lsv = list()
)
}
\arguments{
\item{raw_dt}{data.table of raw values. rows = genes; columns = samples.}

\item{cpm_dt}{data.table of cpm values. rows = genes; columns = samples.}

\item{metaCols_v}{columns that should not be used for cpm values. Default is "Gene"}

\item{rawTest_v}{vector of numbers to use as raw values to test for cpm equivalents.}

\item{indPlot_v}{logical. TRUE - print raw-cpm comparison plot for each sample; FALSE - do not print plot.}

\item{histPlot_v}{logical. TRUE - print summary histogram plot; FALSE - do not print}

\item{densPlot_v}{logical. TRUE - print density plot; FALSE - do not print}

\item{boxPlot_v}{logical. TRUE - print boxplot; FALSE - do not print}

\item{plotDir_v}{character vector of where to save plots.}

\item{meta_dt}{data.table containing metadata information. Should have column named "sample" that matches colnames of raw_dt/cpm_dt}

\item{plotCols_lsv}{list of metadata columns to use to change plots. list element name is the plotting parameter (e.g. color or fill),
while list element value is the column name (e.g. batch, treatment)}
}
\value{
Named vector where values are cpm and names are the corresponding raw value.
}
\description{
Plot the cpm counts for a dataset to determine a potential filter threshold.
}
