% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Resting.R
\name{mbites_restingSpot}
\alias{mbites_restingSpot}
\title{MBITES: Land After Flight \code{MosquitoFemale}}
\usage{
mbites_restingSpot()
}
\description{
Mosquito lands after a flight (choose a landing spot), which may cause various events.
This function always calls \code{\link{mbites_newSpot}} and may call \code{\link{mbites_enterHouse}}
Landing spots include:
\itemize{
\item i: 1 rest on the inside wall of a structure
\item w: 2 rest on the outside wall of a structure
\item v: 3 rest on vegetation
\item r: 4 reattempt without resting
\item l: 5 leave the area
}
}
\details{
\itemize{
\item This method is bound to \code{Mosquito$restingSpot}
}
}
