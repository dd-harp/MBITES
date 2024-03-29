% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Bionomics.R
\name{Bionomics_lifespan}
\alias{Bionomics_lifespan}
\title{Bionomics: Compute Mosquito Lifespans}
\usage{
Bionomics_lifespan(mosquitos)
}
\arguments{
\item{mosquitos}{a data.frame of parsed JSON mosquito output}
}
\description{
Takes in JSON output parsed into a data.frame object from
an MBITES simulation run and returns a data.frame of mosquito lifespans.
Mosquitoes that were still alive at the end of simulation are filtered out.
}
