% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBDETES-Model.R
\name{MBDETES_cohort_solve}
\alias{MBDETES_cohort_solve}
\title{MBDETES: Cohort ODE Model Trajectory}
\usage{
MBDETES_cohort_solve(PAR, pF = 1, mxT = 100, dt = 0.001)
}
\arguments{
\item{PAR}{output of \code{\link{MBDETES_Parameters}}}

\item{pF}{initial fraction of cohort in blood feeding search state}

\item{mxT}{maximum time of numerical solution}

\item{dt}{time-step at which solutions are requested}
}
\description{
Calculate sample trajectory for cohort model.
}
