% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params.R
\name{ss.options.extra}
\alias{ss.options.extra}
\title{Add lines to the current SaTScan parameter list}
\usage{
ss.options.extra(invals = NULL)
}
\arguments{
\item{invals}{A character vector, which will be added to the end of the 
current paramter list.}
}
\value{
Nothing.
}
\description{
Allows you to add arbitrary lines to the current set
of SaTScan parameters
}
\details{
For certain SaTScan models or inputs (multiple data sets, 
Polygon),
SaTScan allows a variable number of parameters; these 
parameters are not used/allowed for other models or inputs.
This function allows the user to add 
arbitray lines to the current list of 
parameters.  In addition to the options mentioned, it could also be
used to add comments to the parameter file.
}
