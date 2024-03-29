% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MBITES-Survival.R
\name{mbites_pSenesce}
\alias{mbites_pSenesce}
\title{MBITES: Probability of Death due to Senescence}
\usage{
mbites_pSenesce()
}
\description{
Probability of death due to senescence given by \deqn{ \frac{2+sns.b}{1+sns.b} - \frac{e^{sns.a\times age}}{sns.b+e^{sns.a\times age}} }
\itemize{
\item This method is bound to \code{Mosquito$pSenesce}.
}
}
