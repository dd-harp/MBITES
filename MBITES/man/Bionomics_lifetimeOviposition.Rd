% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Bionomics.R
\name{Bionomics_lifetimeOviposition}
\alias{Bionomics_lifetimeOviposition}
\title{Bionomics: Compute Lifetime Egg Production and Spatial Dispersion}
\usage{
Bionomics_lifetimeOviposition(mosquitos, spatial = FALSE)
}
\arguments{
\item{mosquitos}{a data.frame of parsed JSON mosquito output}

\item{spatial}{compute spatial dispersion of eggs or not}
}
\description{
Takes in JSON output parsed into a data.frame object from
an MBITES simulation run.
Computes lifetime egg production, as well as its spatial dispersion.
Mosquitoes that were still alive at the end of simulation are filtered out.
Please note that in order to reconstruct kernels for egg dispersion, the distance matrix between sites
must be preserved somewhere, as the mosquito only records the index of the site it visited, not the xy coordinates.
}
