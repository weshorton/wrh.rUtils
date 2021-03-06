% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcOverlap.R
\name{calcOverlap}
\alias{calcOverlap}
\title{Calculate Overlap}
\usage{
calcOverlap(set1_v, set2_v, total_v = NA, digits_v = 4, fullOut_v = T)
}
\arguments{
\item{set1_v}{Character vector of values to compare}

\item{set2_v}{Character vector of values to compare}

\item{total_v}{Optional global set to compare set1 and set2 against. Only used for BUB. 
If 'NA' (default), will just calculate Jaccard index.}

\item{digits_v}{how many digits to round output to. Default is 4.}

\item{fullOut_v}{logical. TRUE - output number of elements in 'set1', 'set2', their intersection, their union, 
and number of elements in 'total', in addition to jaccard and BUB values.
FALSE - just output jaccard and BUB values}
}
\description{
Calculate the Jaccard Index of two different sets. Can also calculate BUB.
}
\details{
This will calculate overlap between two sets by comparing the identities of each element within
set1 and set2. Jaccard Index is the Intersection of the two sets divided by the union of the two sets. BUB
(Baroni Urbani Binary Index) is similar, but compares against a global set (rather than just the union of 1 and 2).
This allows for weighting two samples as more similar if a they also cover a greater amount of the global set.
}
\examples{
A <- 1:100
B <- 25:75
C <- 60:150
TOT <- union(union(A, B), C)
calcOverlap(A, B)
calcOverlap(A, C)
calcOverlap(A, B, TOT)
calcOverlap(A, C, TOT)
}
