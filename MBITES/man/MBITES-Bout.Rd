% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Bout.R
\name{MBITES-Bout}
\alias{MBITES-Bout}
\title{MBITES: Bout}
\description{
MBITES: Bout
}
\section{Mosquito Behavioral Bouts}{


In MBITES, mosquito behavioral bouts consist of a cycle of events: launch -> try -> land -> rest. This generic
bout process is simulated through \code{\link{mbites_oneBout}}.
}

\section{Specific Behaviors}{


There are four discrete behavioral states that can be simulated in MBITES, and an additional search bout, where a mosquito may move between \code{\link{Site}} objects to find resources:
\itemize{
\item \code{\link{mbites_boutB}}: blood feeding attempt bout
\item \code{\link{mbites_boutO}}: oviposition attempt bout
\item \code{\link{mbites_boutM}}: mating attempt bout
\item \code{\link{mbites_boutS}}: sugar feeding attempt bout
\item \code{\link{mbites_attempt_search}}: search attempt bout
}

Additionally, there are a variety of supporting modules to simulate specific results or effects from the behavioral states.
}

