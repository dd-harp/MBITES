% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Bionomics.R
\name{Bionomics_UniqueBites}
\alias{Bionomics_UniqueBites}
\title{Bionomics: Compute Unique Secondary Bites (derived from human-centric VC)}
\usage{
Bionomics_UniqueBites(mosquitos, humans, EIP)
}
\arguments{
\item{mosquitos}{a data.frame of parsed JSON mosquito output}

\item{humans}{a data.frame of parsed JSON human output}

\item{EIP}{the length of EIP}

\item{spatial}{compute spatial dispersion of bites or not}
}
\value{
a list where each element corresponds to a human host.
Each host has \code{VC}, which is the total number of secondary bites arising from him or her, and
\code{spatialVC} which is a list of origin/destination pairs tracking dispersion of each initial bite.
}
\description{
Takes in JSON output parsed into a data.frame object from
an MBITES simulation run.
For each human, compute VC as in \code{\link{Bionomics_vectorialCapacity}} but only consider
unique secondary hosts to contribute to a focal hosts's VC.
}
