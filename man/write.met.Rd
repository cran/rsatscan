% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{write.met}
\alias{write.met}
\title{Write a SaTScan met file}
\usage{
write.met(x, location, filename, userownames = FALSE)
}
\arguments{
\item{x}{Your data frame.}

\item{location}{Directory location where the file should be written}

\item{filename}{Name for the output file in the OS; .met will be added.}

\item{userownames}{If TRUE, will write the row names into the file.}
}
\description{
Write a SaTScan met file
}
\details{
Writes the input data frame to the OS, using the .met extension.  Contents of the data 
frame should be only what you want SaTScan to see.  
This is a simple function that calls write.table, since SaTScan just needs ASCII files.
}
