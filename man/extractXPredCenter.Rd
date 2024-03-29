% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-modules-modelResultHelpers.R
\name{extractXPredCenter}
\alias{extractXPredCenter}
\title{Extract XPred Center}
\usage{
extractXPredCenter(
  XPred,
  centerX,
  centerY,
  Radius,
  batch = FALSE,
  isThreeD = FALSE,
  data = NULL,
  time = NULL
)
}
\arguments{
\item{XPred}{(data.frame) data.frame containing all estimates}

\item{centerX}{(numeric) center for longitude}

\item{centerY}{(numeric) center for latitude}

\item{Radius}{(numeric) radius of the area around the center}

\item{batch}{(logical) set TRUE if used for batch estimates}

\item{isThreeD}{(logical) set TRUE if used in plotMap3D}

\item{data}{(data.frame) data.frame with input data for the model}

\item{time}{(numeric) center of time}
}
\description{
Extract XPred Center
}
