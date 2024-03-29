% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBDETES-Parameters.R
\name{ELSB_PAR}
\alias{ELSB_PAR}
\title{MBDETES: Parameters for Transitions from L}
\usage{
ELSB_PAR(A = 0.94, D1 = 0.98, F1 = 0.8)
}
\arguments{
\item{A}{probability of successful launch (maps to \code{Os_succeed})}

\item{D1}{probability to survive resting (\code{\link{upstateState}}) (calculate with \code{MBDETES_PrSurvive(site,"L")}, uses parameters \code{Os_surv}, and the local \code{\link{Site}} object's hazard)}

\item{F1}{probability to leave the current \code{\link{Site}} and go on a oviposition search (calculate with \code{1 - MBDETES_PrLeave(site,"aqua",FALSE)}, uses parameters \code{boutFail_p}, \code{disperse})}
}
\description{
Produce named list of parameters for \code{\link{ELSB_L2X}}.
This function is useful for finding parameter sets that map between
MBITES and MBDETES parameter spaces.
}
