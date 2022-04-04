% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fHMM_sdds.R
\name{fHMM_sdds}
\alias{fHMM_sdds}
\title{Defining state-dependent distributions}
\usage{
fHMM_sdds(sdds)
}
\arguments{
\item{sdds}{A character or a character vector of length two that can be specified for
\code{"sdds"} in \code{\link{set_controls}}.}
}
\value{
A list of length \code{length(sdds)}. Each element is a list, containing
\itemize{
\item the \code{"name"} of the distribution
\item and a list \code{"pars"} of its parameters.
}
Unknown parameters are set to \code{NULL}.
}
\description{
This function defines state-dependent distributions for the {fHMM} package.
}
\examples{
sdds <- c("t(sigma = 0.1, df = Inf)", "gamma")
fHMM:::fHMM_sdds(sdds)

}
\keyword{internal}