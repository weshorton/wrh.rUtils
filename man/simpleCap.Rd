% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpleCap.R
\name{simpleCap}
\alias{simpleCap}
\title{Capitalize first letter of each word}
\usage{
simpleCap(x, lowerCaps_v = T)
}
\arguments{
\item{x}{vector}

\item{lowerCaps_v}{boolean indicating if capitalized letters (that are not the first letter in each word)
should be left capitalized or changed to lower case.}
}
\value{
same as X, but each first letter is capitalized.
}
\description{
given multi-word vector, capitalize the first letter of each word.
Taken from 'Examples' section of ?toupper
}
\examples{
a <- "capitalize the first letter"
b <- "optionally RETURN other CAPS to lower"
c <- c("works with multiple", "strings at once")
simpleCap(a)
simpleCap(b)
simpleCap(b, F)
simpleCap(c)
}
