% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myRGB.R
\name{myRGB}
\alias{myRGB}
\title{my RGB}
\usage{
myRGB(color_lsv, maxColorValue_v = 255)
}
\arguments{
\item{color_lsv}{list of vectors of length 3 containing rgb values}

\item{maxColorValue_v}{passed to rgb's maxColorValue}
}
\value{
list of hex colors. if only 1, will just be a vector
}
\description{
rgb() function with a vector of length 3 instead of 3 separate args
}
\details{
Additional details...
}
