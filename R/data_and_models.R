#' Standard & Poor’s 500 (S&P 500) index data
#' 
#' @description 
#' Standard & Poor’s 500 (S&P 500) index data from 1928 to 2022 from Yahoo 
#' Finance.
#' 
#' @details 
#' The data was obtained via: 
#' \preformatted{
#' spx <- download_data(
#'   symbol = "^GSPC",    # S&P 500 identifier on Yahoo Finance
#'   from = "1928-01-01", # first observation
#'   to = "2022-12-31"    # last observation
#' )
#' }
#' 
#' @format A \code{data.frame} with 23864 rows and the following 7 columns:
#' \itemize{
#'   \item \code{Date}: The date.
#'   \item \code{Open}: Opening price.
#'   \item \code{High}: Highest price.
#'   \item \code{Low}: Lowest price.
#'   \item \code{Close}: Close price adjusted for splits.
#'   \item \code{Adj.Close}: Close price adjusted for dividends and splits.
#'   \item \code{Volume}: Trade volume.
#' }
#' 
#' @keywords data
"spx"

#' Deutscher Aktienindex (DAX) index data
#' 
#' @description 
#' Deutscher Aktienindex (DAX) index data from 1988 to 2022 from Yahoo Finance.
#' 
#' @details 
#' The data was obtained via: 
#' \preformatted{
#' dax <- download_data(
#'   symbol = "^GDAXI",   # DAX identifier on Yahoo Finance
#'   from = "1988-01-01", # first observation
#'   to = "2022-12-31"    # last observation
#' )
#' }
#' 
#' @format A \code{data.frame} with 9012 rows and the following 7 columns:
#' \itemize{
#'   \item \code{Date}: The date.
#'   \item \code{Open}: Opening price.
#'   \item \code{High}: Highest price.
#'   \item \code{Low}: Lowest price.
#'   \item \code{Close}: Close price adjusted for splits.
#'   \item \code{Adj.Close}: Close price adjusted for dividends and splits.
#'   \item \code{Volume}: Trade volume.
#' }
#' 
#' @keywords data
"dax"

#' Volkswagen AG (VW) stock data
#' 
#' @description 
#' Volkswagen AG (VW) stock data from 1998 to 2022 from Yahoo Finance.
#' 
#' @details 
#' The data was obtained via: 
#' \preformatted{
#' vw <- download_data(
#'   symbol = "VOW3.DE",  # Volkswagen AG identifier on Yahoo Finance
#'   from = "1988-07-22", # first observation
#'   to = "2022-12-31"    # last observation
#' )
#' }
#' 
#' @format A \code{data.frame} with 6260 rows and the following 7 columns:
#' \itemize{
#'   \item \code{Date}: The date.
#'   \item \code{Open}: Opening price.
#'   \item \code{High}: Highest price.
#'   \item \code{Low}: Lowest price.
#'   \item \code{Close}: Close price adjusted for splits.
#'   \item \code{Adj.Close}: Close price adjusted for dividends and splits.
#'   \item \code{Volume}: Trade volume.
#' }
#' 
#' @keywords data
"vw"

#' Unemployment rate data USA
#' 
#' @description
#' The monthly unemployment rate in the USA from 1955 to 2022 on a daily
#' observation basis. 
#' 
#' @format 
#' A \code{data.frame} with 24806 rows and the following 3 columns:
#' \itemize{
#'   \item \code{date}: The date.
#'   \item \code{rate}: The unemployment rate.
#'   \item \code{rate_diff}: The difference rate to previous month.
#' }
#' 
#' @source 
#' OECD (2023), Unemployment rate (indicator). 
#' doi: 10.1787/52570002-en (Accessed on 18 January 2023)
#' <https://www.oecd.org/en/data/indicators/unemployment-rate.html>
#' 
#' @keywords data
"unemp"

#' DAX 2-state HMM with normal distributions
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2022
#' with two hidden states and normal state-dependent distributions for
#' demonstration purpose.
#'
#' @usage data("dax_model_2n")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- set_controls(
#'   states = 2,
#'   sdds   = "normal",
#'   data   = list(
#'     file        = dax,
#'     date_column = "Date",
#'     data_column = "Close",
#'     logreturns  = TRUE,
#'     from        = "2000-01-03",
#'     to          = "2022-12-31"
#'   ),
#'   fit    = list("runs" = 10, "gradtol" = 1e-6, "steptol" = 1e-6)
#' )
#' dax_data <- prepare_data(controls)
#' dax_model_2n <- fit_model(dax_data, seed = 1)
#' dax_model_2n <- decode_states(dax_model_2n)
#' dax_model_2n <- compute_residuals(dax_model_2n)
#' summary(dax_model_2n)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_model_2n"

#' DAX 3-state HMM with t-distributions
#'
#' @description
#' A pre-computed HMM on closing prices of the DAX from 2000 to 2022
#' with three hidden states and state-dependent t-distributions for
#' demonstration purpose.
#'
#' @usage data("dax_model_3t")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- set_controls(
#'   states = 3,
#'   sdds   = "t",
#'   data   = list(
#'     file        = dax,
#'     date_column = "Date",
#'     data_column = "Close",
#'     logreturns  = TRUE,
#'     from        = "2000-01-03",
#'     to          = "2022-12-31"
#'   ),
#'   fit    = list(
#'     runs        = 100, 
#'     iterlim     = 300,
#'     gradtol     = 1e-6,
#'     steptol     = 1e-6
#'   )
#' )
#' dax_data <- prepare_data(controls)
#' dax_model_3t <- fit_model(dax_data, seed = 1, ncluster = 10)
#' dax_model_3t <- decode_states(dax_model_3t)
#' dax_model_3t <- compute_residuals(dax_model_3t)
#' summary(dax_model_3t)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_model_3t"

#' DAX/VW hierarchical HMM with t-distributions 
#'
#' @description
#' A pre-computed HHMM with monthly averaged closing prices of the
#' DAX from 2010 to 2022 on the coarse scale, Volkswagen AG stock data on the 
#' fine scale, two hidden fine-scale and coarse-scale states, respectively, and
#' state-dependent t-distributions for demonstration purpose.
#'
#' @usage data("dax_vw_model")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- set_controls(
#'   hierarchy = TRUE,
#'   states    = c(2, 2),
#'   sdds      = c("t", "t"),
#'   period    = "m",
#'   data      = list(
#'     file       = list(dax, vw),
#'     from       = "2010-01-01",
#'     to         = "2022-12-31",
#'     logreturns = c(TRUE, TRUE)
#'   ),
#'   fit       = list(
#'     runs       = 200, 
#'     iterlim    = 300,
#'     gradtol    = 1e-6,
#'     steptol    = 1e-6
#'   )
#' )
#' dax_vw_data <- prepare_data(controls)
#' dax_vw_model <- fit_model(dax_vw_data, seed = 1, ncluster = 10)
#' dax_vw_model <- decode_states(dax_vw_model)
#' dax_vw_model <- compute_residuals(dax_vw_model)
#' summary(dax_vw_model)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"dax_vw_model"

#' Unemployment rate and S&P 500 hierarchical HMM
#'
#' @description
#' A pre-computed HHMM with monthly unemployment rate in the US on the coarse
#' scale using 3 states and S&P 500 index data on the fine scale using 2 states
#' from 1970 to 2020 for demonstration purpose.
#'
#' @usage data("unemp_spx_model_3_2")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- list(
#'   hierarchy = TRUE, states = c(3, 2), 
#'   sdds = c("t", "t"), period = "m",
#'   data = list(
#'     file = list(unemp, spx), 
#'     data_column = c("rate_diff", "Close"), 
#'     date_column = c("date", "Date"), 
#'     from = "1970-01-01", to = "2020-01-01", 
#'     logreturns = c(FALSE, TRUE)
#'   ),
#'   fit = list(runs = 50, iterlim = 1000, gradtol = 1e-6, steptol = 1e-6)
#' )
#' controls <- set_controls(controls)
#' unemp_spx_data <- prepare_data(controls)
#' unemp_spx_model_3_2 <- fit_model(unemp_spx_data, seed = 1, ncluster = 10)
#' unemp_spx_model_3_2 <- decode_states(unemp_spx_model_3_2)
#' unemp_spx_model_3_2 <- compute_residuals(unemp_spx_model_3_2)
#' summary(unemp_spx_model_3_2)
#' state_order <- matrix(c(3, 2, 1, 2, 2, 2, 1, 1, 1), 3, 3)
#' unemp_spx_model_3_2 <- reorder_states(unemp_spx_model_3_2, state_order)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"unemp_spx_model_3_2"

#' Simulated 2-state HMM with gamma distributions
#'
#' @description
#' A pre-computed 2-state HMM with state-dependent gamma distributions with 
#' means fixed to \code{0.5} and \code{2} on \code{500} simulated observations.
#'
#' @usage data("sim_model_2gamma")
#'
#' @details
#' The model was estimated via:
#' \preformatted{
#' controls <- set_controls(
#'   states  = 2,
#'   sdds    = "gamma(mu = 1|2)",
#'   horizon = 200,
#'   runs    = 10
#' )
#' pars <- fHMM_parameters(
#'   controls = controls,
#'   Gamma = matrix(c(0.9, 0.2, 0.1, 0.8), nrow = 2),
#'   sigma = c(0.5, 1),
#'   seed = 1
#' )
#' data_sim <- prepare_data(controls, true_parameters = pars, seed = 1)
#' sim_model_2gamma <- fit_model(data_sim, seed = 1)
#' sim_model_2gamma <- decode_states(sim_model_2gamma)
#' sim_model_2gamma <- compute_residuals(sim_model_2gamma)
#' summary(sim_model_2gamma)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"sim_model_2gamma"

