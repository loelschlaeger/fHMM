#' DAX 2-state HMM
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2021
#' with two hidden states and normal state-dependent distributions for
#' demonstration purpose.
#'
#' @usage data(dax_model_2n)
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   states = 2,
#'   sdds   = "t(df = Inf)",
#'   data   = list(file        = system.file("extdata", "dax.csv", package = "fHMM"),
#'                 date_column = "Date",
#'                 data_column = "Close",
#'                 logreturns  = TRUE,
#'                 from        = "2000-01-03",
#'                 to          = "2021-12-31"),
#'   fit    = list("runs" = 100)
#'   )
#' }
#'
#' @format An object of class \code{fHMM_model}.
#'
#' @keywords
#' model
"dax_model_2n"

#' DAX 3-state HMM
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2021
#' with three hidden states and state-dependent t-distributions for
#' demonstration purpose.
#'
#' @usage data(dax_model_3t)
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   states = 3,
#'   sdds   = "t",
#'   data   = list(file        = system.file("extdata", "dax.csv", package = "fHMM"),
#'                 date_column = "Date",
#'                 data_column = "Close",
#'                 logreturns  = TRUE,
#'                 from        = "2000-01-03",
#'                 to          = "2021-12-31"),
#'   fit    = list("runs" = 100)
#'   )
#' }
#'
#' @format An object of class \code{fHMM_model}.
#'
#' @keywords
#' model
"dax_model_3t"

#' DAX/VW hierarchical HMM
#'
#' @description
#' A pre-computed HHMM with monthly averaged closing prices of the
#' DAX from 2000 to 2021 on the coarse scale, VW stock data on the fine scale,
#' two hidden fine-scale and coarse-scale states, respectively, and
#' state-dependent t-distributions with degrees of freedom fixed to 1 for
#' demonstration purpose.
#'
#' @usage data(dax_vw_model)
#'
#' @details
#' The model was derived via specifying
#' \preformatted{
#' controls <- list(
#'   hierarchy = TRUE,
#'   states    = c(2,2),
#'   sdds      = c("t(df = 1)", "t(df = 1)"),
#'   period    = "m",
#'   data      = list(file = c(system.file("extdata", "dax.csv", package = "fHMM"),
#'                             system.file("extdata", "vw.csv", package = "fHMM")),
#'                    from = "2015-01-01",
#'                    to = "2020-01-01",
#'                    logreturns = c(TRUE,TRUE)),
#'   fit       = list("runs" = 100)
#' )
#' }
#'
#' @format An object of class \code{fHMM_model}.
#'
#' @keywords
#' model
"dax_vw_model"
