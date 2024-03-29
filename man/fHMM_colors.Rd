% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fHMM_colors.R
\name{fHMM_colors}
\alias{fHMM_colors}
\title{Set color scheme for visualizations}
\usage{
fHMM_colors(controls, colors = NULL)
}
\arguments{
\item{controls}{An object of class \code{fHMM_controls}.
It can be created with \code{\link{set_controls}}.}

\item{colors}{Either \code{NULL} (default) or a \code{character} vector of color names or
hexadecimal RGB triplets.}
}
\value{
An object of class \code{fHMM_colors}, which is:
\itemize{
\item for \code{controls$hierarchy == FALSE} a \code{character} vector of
length \code{controls$states} of color codes,
\item for \code{controls$hierarchy == TRUE} a \code{list} of
\itemize{
\item a \code{character} vector of length  \code{controls$states[1]} and
\item a \code{character} matrix of dimensions \code{controls$states}
}
with color codes.
}
}
\description{
This helper function defines a color scheme for visualizations in the
\{fHMM\} package.
}
\examples{
\dontrun{
controls <- set_controls()
fHMM_colors(controls, colors = c("red", "blue"))
}

}
\keyword{internal}
