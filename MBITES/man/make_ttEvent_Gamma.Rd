% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Parameters.R
\name{make_ttEvent_Gamma}
\alias{make_ttEvent_Gamma}
\title{MBITES Parameters: Make a Shifted Gamma Time-to-Event Closure}
\usage{
make_ttEvent_Gamma(mean, cv, tmin)
}
\description{
Generates a closure that contains the following fields:
\itemize{
\item mean: numeric value of mean
\item cv: numeric value of coefficient of variation (should be between 0 and 1 for reasonable sampling)
\item tmin: numeric value of minimum waiting time
}
}
\details{
The closure also contains the following functions:
\itemize{
\item tteShiftGamma(t): samples a time to event following shifted exponential distribution
}
}
