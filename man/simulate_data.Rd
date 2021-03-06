% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data.R
\name{simulate_data}
\alias{simulate_data}
\title{Data simulation}
\usage{
simulate_data(controls, sim_par)
}
\arguments{
\item{controls}{A list of controls.}

\item{sim_par}{A list of model parameters for simulation in \code{thetaList} format.}
}
\value{
A list containing the following elements:
\item{data}{A matrix of simulated data.}
\item{states0}{A matrix of simulated hidden states.}
\item{thetaUncon0}{True parameters in format \code{thetaUncon}.}
\item{thetaCon0}{True parameters in format \code{thetaCon}.}
\item{thetaList0}{True parameters in format \code{thetaList}.}
\item{T_star}{A vector of fine-scale chunk sizes.}
}
\description{
Simulates data from a (hierarchical) hidden Markov model.
}
