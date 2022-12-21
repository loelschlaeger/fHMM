#' DAX data
#' 
#' @description 
#' DAX data from ... to ... scraped from ...
#' TODO
#' 
#' @details 
#' The data was obtained via ... 
#' TODO
#' 
#' @format A \code{data.frame} with the columns:
#' TODO
#' 
#' @keywords data
"dax"

#' VW data
#' 
#' @description 
#' VW data from ... to ... scraped from ...
#' TODO
#' 
#' @details 
#' The data was obtained via ...
#' TODO
#' 
#' @format A \code{data.frame} with the columns:
#' TODO
#' 
#' @keywords data
"vw"

#' DAX 2-state HMM with normal distributions
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2021
#' with two hidden states and normal state-dependent distributions for
#' demonstration purpose.
#'
#' @usage data("dax_model_2n")
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   states = 2,
#'   sdds   = "t(df = Inf)",
#'   data   = list(
#'     file        = system.file("extdata", "dax.csv", package = "fHMM"),
#'     date_column = "Date",
#'     data_column = "Close",
#'     logreturns  = TRUE,
#'     from        = "2000-01-03",
#'     to          = "2021-12-31"
#'    ),
#'   fit    = list("runs" = 100)
#' )
#' TODO
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_model_2n"

#' DAX 3-state HMM with t-distributions
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2021
#' with three hidden states and state-dependent t-distributions for
#' demonstration purpose.
#'
#' @usage data("dax_model_3t")
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   states = 3,
#'   sdds   = "t",
#'   data   = list(
#'     file        = system.file("extdata", "dax.csv", package = "fHMM"),
#'     date_column = "Date",
#'     data_column = "Close",
#'     logreturns  = TRUE,
#'     from        = "2000-01-03",
#'     to          = "2021-12-31"
#'   ),
#'   fit    = list("runs" = 100)
#' )
#' TODO
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_model_3t"

#' DAX/VW hierarchical HMM with t(1)-distributions 
#'
#' @description
#' A pre-computed HHMM with monthly averaged closing prices of the
#' DAX from 2000 to 2021 on the coarse scale, VW stock data on the fine scale,
#' two hidden fine-scale and coarse-scale states, respectively, and
#' state-dependent t-distributions with degrees of freedom fixed to 1 for
#' demonstration purpose.
#'
#' @usage data("dax_vw_model")
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   hierarchy = TRUE,
#'   states    = c(2,2),
#'   sdds      = c("t(df = 1)", "t(df = 1)"),
#'   period    = "m",
#'   data      = list(
#'     file = c(system.file("extdata", "dax.csv", package = "fHMM"),
#'              system.file("extdata", "vw.csv", package = "fHMM")),
#'     from = "2015-01-01",
#'     to = "2020-01-01",
#'     logreturns = c(TRUE,TRUE))
#' )
#' TODO
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_vw_model"

#' Simulated 2-state HMM with gamma distributions
#'
#' @description
#' A pre-computed 2-state HMM with state-dependent gamma distributions with means
#' fixed to \code{0.5} and \code{2} on \code{500} simulated observations.
#'
#' @usage data("sim_model_2gamma")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- list(
#'   states  = 2,
#'   sdds    = "gamma(mu = 1|2)",
#'   horizon = 200,
#'   fit     = list(runs = 50)
#' )
#' controls <- set_controls(controls)
#' pars <- fHMM_parameters(
#'   controls = controls, Gamma = matrix(c(0.9,0.2,0.1,0.8), nrow = 2),
#'   sigmas = c(0.5,1)
#' )
#' data <- prepare_data(controls, true_parameters = pars, seed = 1)
#' sim_model_2gamma <- fit_model(data, seed = 1, verbose = TRUE)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"sim_model_2gamma"

#' Simulated 4-state HMM with log-normal distributions
#'
#' @description
#' A pre-computed 4-state HMM with state-dependent log-normal distributions
#' on \code{1000} simulated observations.
#'
#' @usage data("sim_model_4lnorm")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- list(
#'   states  = 4,
#'   sdds    = "lnorm",
#'   horizon = 1000,
#'   fit     = list(runs = 50)
#' )
#' controls <- set_controls(controls)
#' data <- prepare_data(controls, seed = 1)
#' sim_model_4lnorm <- fit_model(data, seed = 1, verbose = TRUE)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"sim_model_4lnorm"
