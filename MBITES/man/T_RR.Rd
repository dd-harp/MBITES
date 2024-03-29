% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBDETES-Model.R
\name{T_RR}
\alias{T_RR}
\title{MBDETES: Holding Time of R -> R Transition}
\usage{
T_RR(PAR)
}
\arguments{
\item{PAR}{output of \code{\link{MBDETES_Parameters}}}
}
\description{
The holding time in R, conditional on transitioning to R (one surccessful feeding cycle).
\deqn{T_{R,R} = \sum_{X \neq \left\{R,D\right\}} P_{R,X} \Psi_{X,R} T_{X,R}}
}
