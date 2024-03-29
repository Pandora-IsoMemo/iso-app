% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-plotExport.R
\name{getFileExt}
\alias{getFileExt}
\title{Get File Ext}
\usage{
getFileExt(exportType, isTimeSeries, typeOfSeries, isCollection = FALSE)
}
\arguments{
\item{exportType}{(character) file type of exported plot}

\item{isTimeSeries}{(logical) if TRUE, set file names for a series of plots}

\item{typeOfSeries}{one of "gifAndZip", "onlyZip", "onlyGif"}

\item{isCollection}{(logical) TRUE if this is the container file, FALSE if this is an element file}
}
\description{
Get file extension
}
