% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-estimateMap.R
\name{estimateMapSpread}
\alias{estimateMapSpread}
\title{Estimates spatial spread model (first or latest occurence of event)}
\usage{
estimateMapSpread(
  data,
  Longitude,
  Latitude,
  DateOne,
  DateTwo,
  center = c("Europe", "Pacific"),
  burnin = 500,
  iter = 2000,
  nChains = 1,
  K = 50,
  MinMax = "Max",
  DateType = "Interval",
  dateUnc = "mid point",
  CoordType = "decimal degrees",
  smoothConst = 1,
  penalty = 1,
  splineType = 2,
  shinyApp = FALSE,
  outlier = FALSE,
  outlierValue = 4,
  outlierD = FALSE,
  outlierValueD = 4,
  restriction = c(-90, 90, -180, 180),
  correctionPac = FALSE,
  thinning = 2,
  spreadQ = 0.01,
  minValue = -Inf
)
}
\arguments{
\item{data}{data.frame: data}

\item{Longitude}{character: name of longitude variable}

\item{Latitude}{character: name of latitude variable}

\item{DateOne}{character: name of date variable 1 (lower interval point / mean / single point)}

\item{DateTwo}{character: name of date variable 2 (upper interval point / sd / )}

\item{center}{(character) center to shift data to, either "Europe" or "Pacific"}

\item{burnin}{integer: number of burn-in iterations for Bayesian model (default = 500)}

\item{iter}{integer: number of iterations for Bayesian model (default = 2000)}

\item{nChains}{integer: number of chains for Bayesian model (default = 1)}

\item{K}{integer: number of basis functions for tprs (thin plate regression spline)}

\item{MinMax}{character: estimate minimum or maximum of distribution. choices: "Max", "Min"}

\item{DateType}{character: one of "Interval", "Mean + 1 SD uncertainty" and "Single Point"}

\item{dateUnc}{character: one of "uniform", "normal", "point"}

\item{CoordType}{character: type of longitude/latitude coordinates.
One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"}

\item{smoothConst}{numeric: adjust smoothing parameter for Bayesian model (optional)}

\item{penalty}{numeric: 1 for constant extrapolation, 2 for linear extrapolation}

\item{splineType}{numeric: 1 for classical tprs, 2 for spherical spline}

\item{shinyApp}{boolean: If called inside shinyApp: Set to true}

\item{outlier}{boolean: outlier removal TRUE/FALSE}

\item{outlierValue}{numeric: if outlier removal is TRUE, threshold for removals in sd}

\item{outlierD}{boolean: data outlier removal TRUE/FALSE}

\item{outlierValueD}{numeric: if outlierD removal is TRUE, threshold for removals in sd}

\item{restriction}{numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)}

\item{correctionPac}{boolean: correction (data augmentation) for pacific centering}

\item{thinning}{numeric: mcmc thinning for bayesian models}

\item{spreadQ}{numeric: exceedance quantile as buffer}

\item{minValue}{numeric: minValue restriction}
}
\description{
Estimates spatial spread model (first or latest occurence of event)
}
\examples{
\dontrun{
# load data
data <- readRDS(system.file("extData", "exampleData.Rds", package = "DSSM"))
# estimate model-map
map <- estimateMapSpread(data = data, Longitude = "longitude",
Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", iter = 200)
# Plot the map
plotMap(model = map)
}

}
