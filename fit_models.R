#' try({
#' controls <- set_controls(
#' states = 2,
#' sdds   = "normal",
#' data   = list(
#'   file        = dax,
#'   date_column = "Date",
#'   data_column = "Close",
#'   logreturns  = TRUE,
#'   from        = "2000-01-03",
#'   to          = "2022-12-31"
#' ),
#' fit    = list("runs" = 10, "gradtol" = 1e-6, "steptol" = 1e-6)
#' )
#' dax_data <- prepare_data(controls)
#' dax_model_2n <- fit_model(dax_data, seed = 1)
#' dax_model_2n <- decode_states(dax_model_2n)
#' dax_model_2n <- compute_residuals(dax_model_2n)
#' summary(dax_model_2n)
#' })

#' try({
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
#' })


#' try({
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
#' })



try({
controls <- set_controls(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file        = list(unemp, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from        = "1970-01-01",
    to          = "2020-01-01",
    logreturns  = c(FALSE, TRUE)
  ),
  fit       = list(
    runs        = 20, 
    iterlim     = 300,
    gradtol     = 1e-6,
    steptol     = 1e-6
  )
)
unemp_spx_data <- prepare_data(controls)
unemp_spx_model_3_2 <- fit_model(unemp_spx_data, seed = 1, ncluster = 20)
unemp_spx_model_3_2 <- decode_states(unemp_spx_model_3_2)
unemp_spx_model_3_2 <- compute_residuals(unemp_spx_model_3_2)
summary(unemp_spx_model_3_2)
})


#' try({
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
#' })



