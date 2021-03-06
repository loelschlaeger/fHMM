% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter_transformations.R
\name{Gamma2gammasCon}
\alias{Gamma2gammasCon}
\title{Constrains non-diagonal matrix elements of transition probability matrix.}
\usage{
Gamma2gammasCon(Gamma, shift = FALSE)
}
\arguments{
\item{Gamma}{Transition probability matrix.}

\item{shift}{Boolean, determining wheter to shift boundary probabilities.}
}
\value{
Vector of constrained non-diagonal matrix elements (column-wise).
}
\description{
Constrains non-diagonal matrix elements of transition probability matrix.
}
\details{
Function may shift 0 and 1 non-diagonal elements by \code{1e-3}.
}
\keyword{internal}
