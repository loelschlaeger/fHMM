#' DAX model
#'
#' @description 
#' A pre-computed \code{fHMM_model} on closing prices of the DAX from 2000 to 2020
#' for demonstration purpose.
#' 
#' @usage data(dax_data)
#' 
#' @details
#' The model was derived via specifying 
#' \preformatted{
#' controls = list(
#'   states = 3,
#'   sdds   = "t",
#'   data   = list(file        = "inst/extdata/dax.csv",
#'                 date_column = "Date",
#'                 data_column = "Close",
#'                 logreturns  = TRUE,
#'                 from        = "2000-01-03",
#'                 to          = "2021-12-31"),
#'   fit    = list("runs" = 100)
#'   )
#' }
#' Set `file = "extdata/dax.csv"` to reproduce this code.
#'
#' @format An object of class \code{fHMM_model}.
"dax_model"