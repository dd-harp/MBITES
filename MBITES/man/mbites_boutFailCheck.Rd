% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Resting.R
\name{mbites_boutFailCheck}
\alias{mbites_boutFailCheck}
\title{MBITES: Bout Failure Check}
\usage{
mbites_boutFailCheck()
}
\description{
Before choosing a resting spot, check to see how many times it has failed its bout and potentially initiate
a search bout. Probability of abandoning the current \code{\link{Site}} even if necessary resources
are present is given by a geometric distribution over the number of consecutive failures, with parameter \code{boutFail_p} (1/mean number of failed bouts until mosquito gives up and searches).
The \code{boutFail} counter is incremented whenever the mosquito probabilistically fails P(Bout_succeed) and is reset to 0 in the following methods:
\itemize{
\item \code{\link{mbites_BloodMeal}}
\item \code{\link{mbites_layEggs_Emerge}}
\item \code{\link{mbites_layEggs_EL4P}}
\item \code{\link{mbites_sugarMeal}}
}
}
\details{
\itemize{
\item this method is bound to \code{Mosquito$boutFailCheck}
}
}
