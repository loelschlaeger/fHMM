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
#' The data is also available as \code{.csv} file via:
#' \preformatted{
#' system.file("extdata", "spx.csv", package = "fHMM")
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
#' @source 
#' <https://finance.yahoo.com/quote/%5EGSPC>
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
#' The data is also available as \code{.csv} file via:
#' \preformatted{
#' system.file("extdata", "dax.csv", package = "fHMM")
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
#' @source 
#' <https://finance.yahoo.com/quote/%5EGDAXI>
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
#' The data is also available as \code{.csv} file via:
#' \preformatted{
#' system.file("extdata", "vw.csv", package = "fHMM")
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
#' @source 
#' <https://finance.yahoo.com/quote/vow3.de>
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
#' <https://data.oecd.org/unemp/unemployment-rate.htm>
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
#' controls <- list(
#'   states = 2,
#'   sdds   = "t(df = Inf)",
#'   data   = list(
#'     file        = dax,
#'     date_column = "Date",
#'     data_column = "Close",
#'     logreturns  = TRUE,
#'     from        = "2000-01-03",
#'     to          = "2022-12-31"
#'   ),
#'   fit    = list(runs = 100)
#' )
#' controls <- set_controls(controls)
#' dax_data <- prepare_data(controls)
#' dax_model_2n <- fit_model(dax_data)
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
#' controls <- list(
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
#'   fit    = list(runs = 200)
#' )
#' controls <- set_controls(controls)
#' dax_data <- prepare_data(controls)
#' dax_model_3t <- fit_model(dax_data)
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
#' controls <- list(
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
#'     runs = 200
#'   )
#' )
#' controls <- set_controls(controls)
#' dax_vw_data <- prepare_data(controls)
#' dax_vw_model <- fit_model(dax_vw_data)
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
#'   hierarchy = TRUE,
#'   states    = c(3, 2),
#'   sdds      = c("t", "t"),
#'   period    = "m",
#'   data      = list(
#'     file       = list(unemp, spx),
#'     date_column = c("date", "Date"),
#'     data_column = c("rate_diff", "Close"),
#'     from       = "1970-01-01",
#'     to         = "2020-01-01",
#'     logreturns = c(FALSE, TRUE)
#'   ),
#'   fit       = list(
#'     runs    = 200,
#'     iterlim = 300
#'   )
#' )
#' controls <- set_controls(controls)
#' unemp_spx_data <- prepare_data(controls)
#' unemp_spx_model_3_2 <- fit_model(unemp_spx_data)
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
#' controls <- list(
#'   states  = 2,
#'   sdds    = "gamma(mu = 1|2)",
#'   horizon = 200,
#'   fit     = list(runs = 50)
#' )
#' controls <- set_controls(controls)
#' pars <- fHMM_parameters(
#'   controls = controls, Gamma = matrix(c(0.9, 0.2, 0.1, 0.8), nrow = 2),
#'   sigmas = c(0.5, 1)
#' )
#' data_sim <- prepare_data(controls, true_parameters = pars, seed = 1)
#' sim_model_2gamma <- fit_model(data_sim, seed = 1)
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
#' data_sim <- prepare_data(controls, seed = 1)
#' sim_model_4lnorm <- fit_model(data_sim, seed = 1)
#' }
#'
#' @format An object of class \code{\link{fHMM_model}}.
#'
#' @keywords model
"sim_model_4lnorm"
