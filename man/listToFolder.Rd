% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listToFolder.R
\name{listToFolder}
\alias{listToFolder}
\title{listToFolder}
\usage{
listToFolder(tblList = NULL, dataPath = NULL)
}
\arguments{
\item{tblList}{default is \code{NULL}. This is a list full of named dataframes that should be 
saved to the location identified as \code{dataPath}.  These will all be saved as individual *.rds
files, and each will have "_raw" appended to the name.}

\item{dataPath}{default is \code{NULL}. This is the path to the folder where the rds files should
be saved.}
}
\description{
This function
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
