% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PATHOGEN-Generic-Class.R
\name{PathogenGeneric}
\alias{PathogenGeneric}
\title{PATHOGEN: Generic Models}
\description{
All pathogen objects need to be able to communicate with a pathogen pedigree,
which links all clonal variants together through a \code{id} and
\code{parentID} field. Pathogen models link human and mosquito contacts which
occur during feeding, see \code{\link{MBITES-HostEncounter}}
for more details on the "host encounter" process.
}
\details{
All pathogen modules need to implement the following methods in humans:
\itemize{
\item add_pathogen(pathogen): during successful mosquito -> human transmission
}

All pathogen modules need to implement the following methods in mosquitoes:
\itemize{
\item pathogenDynamics
\item probeHost - human-to-mosquito transmission
\item feedHost - mosquito-to-human transmission
}

All pathogen modules need to implement the following methods in pathogens:
\itemize{
\item oneDay_human
}
}
