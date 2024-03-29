% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findSimilarColor.R
\name{findSimilarColor}
\alias{findSimilarColor}
\title{Find Similar Color}
\usage{
findSimilarColor(color_v, adjustment_v = "tint", factor_v = 0.4)
}
\arguments{
\item{color_v}{vector of colors}

\item{adjustment_v}{Type of adjustment - shade, tint, tone, layer, average}

\item{factor_v}{value from 0 to 1 indicating how closely to white or black to pull color.}
}
\value{
vector of hexcodes of length n_v
}
\description{
Find a similar hex code to a given input using
one of the available methods
}
\details{
if length(color_v) is greater than one, will average the hex code before adjusting
shade - darken by a factor of black
tint - lighten by a factor of white
average - no adjustment, just take average of values in color_v
To do - can I output more than one color?
To do - can i do tone and layer?
}
