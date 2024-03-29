% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LANDSCAPE-Aqua.R
\docType{class}
\name{Aqua_Resource}
\alias{Aqua_Resource}
\title{Aquatic Habitat Resource Base Class}
\format{
An \code{\link{R6Class}} generator object
}
\description{
A \code{Aqua_Resource} is a type of resource at a \code{\link{Site}} where
mosquitoes travel for oviposition of eggs and from which new imagos (adult
mosquitoes) emerge from. This abstract base class defines an interface which
all models of aquatic ecology must inherit from to generate concrete
implementations of the interface methods.
}
\section{\strong{Constructor}}{

\itemize{
\item w: a numeric search weight
\item site: a reference to a \code{\link{Site}} object
}
}

\section{\strong{Methods}}{

\itemize{
\item one_day: function that updates daily aquatic population dynamics
\item push_imago: function that takes emerging imagos from the \code{ImagoQ} and
pushes them to the adult mosquito population
\item reset: function that resets the aquatic habitat between simulation runs
}
}

\section{\strong{Fields}}{

\itemize{
\item EggQ: a closure of egg batches (see \code{\link{make_EggQ}})
\item ImagoQ: a closure of imago cohorts (see \code{\link{make_ImagoQ}})
}
}

\keyword{R6}
\keyword{class}
\section{Super class}{
\code{\link[MBITES:Resource]{MBITES::Resource}} -> \code{Aqua_Resource}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Aqua_Resource$new()}}
\item \href{#method-finalize}{\code{Aqua_Resource$finalize()}}
\item \href{#method-one_day}{\code{Aqua_Resource$one_day()}}
\item \href{#method-push_imago}{\code{Aqua_Resource$push_imago()}}
\item \href{#method-get_habitatID}{\code{Aqua_Resource$get_habitatID()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="MBITES" data-topic="Resource" data-id="get_site">}\href{../../MBITES/html/Resource.html#method-get_site}{\code{MBITES::Resource$get_site()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="MBITES" data-topic="Resource" data-id="get_w">}\href{../../MBITES/html/Resource.html#method-get_w}{\code{MBITES::Resource$get_w()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="MBITES" data-topic="Resource" data-id="reset">}\href{../../MBITES/html/Resource.html#method-reset}{\code{MBITES::Resource$reset()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Aqua_Resource$new(w, site)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Aqua_Resource$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-one_day"></a>}}
\if{latex}{\out{\hypertarget{method-one_day}{}}}
\subsection{Method \code{one_day()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Aqua_Resource$one_day()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-push_imago"></a>}}
\if{latex}{\out{\hypertarget{method-push_imago}{}}}
\subsection{Method \code{push_imago()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Aqua_Resource$push_imago()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_habitatID"></a>}}
\if{latex}{\out{\hypertarget{method-get_habitatID}{}}}
\subsection{Method \code{get_habitatID()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Aqua_Resource$get_habitatID()}\if{html}{\out{</div>}}
}

}
}
