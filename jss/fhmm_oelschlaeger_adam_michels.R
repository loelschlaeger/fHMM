### R code from vignette source 'fhmm_oelschlaeger_adam_michels.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 60, useFancyQuotes = FALSE)
library("fHMM")


###################################################
### code chunk number 2: set controls emp hmm
###################################################
controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = "dax.csv",
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE)
)
controls <- set_controls(controls)
class(controls)


###################################################
### code chunk number 3: set controls sim hmm
###################################################
controls <- list(
  states  = 2,
  sdds    = "gamma(mu = 0.5|2)",
  horizon = 500,
  fit     = list(runs = 50)
)
set_controls(controls)


###################################################
### code chunk number 4: set controls hhmm
###################################################
controls <- list(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t(df = 1)", "t(df = Inf)"),
  horizon   = c(100, 10)
)
set_controls(controls)


###################################################
### code chunk number 5: download dax example
###################################################
download_data(symbol = "^GDAXI", from = "2000-01-01", to = Sys.Date())


###################################################
### code chunk number 6: get data
###################################################
system.file("extdata", "dax.csv", package = "fHMM")
system.file("extdata", "vw.csv", package = "fHMM")


###################################################
### code chunk number 7: prepare_data example
###################################################
controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = system.file("extdata", "dax.csv", package = "fHMM"),
                date_column = "Date",
                data_column = "Close",
                from        = "2000-01-01",
                to          = "2021-12-31",
                logreturns  = TRUE),
  fit    = list(runs        = 100)
)
controls <- set_controls(controls)
data <- prepare_data(controls)
class(data)


###################################################
### code chunk number 8: fHMM_data summary
###################################################
summary(data)


###################################################
### code chunk number 9: ts
###################################################
events <- fHMM_events(
  list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c("9/11 terrorist attack", "Bankruptcy of Lehman Brothers", 
               "First COVID-19 case in Germany")
    )
  )
plot(data, events = events)


###################################################
### code chunk number 10: revisited controls example
###################################################
controls <- list(
  states  = 2,
  sdds    = "gamma(mu = 0.5|2)",
  horizon = 500,
  fit     = list(runs = 50)
)
controls <- set_controls(controls)


###################################################
### code chunk number 11: fHMM_parameters example
###################################################
pars <- fHMM_parameters(
  controls = controls, Gamma = matrix(c(0.9,0.2,0.1,0.8), nrow = 2), 
  sigmas = c(0.1,0.5)
)
class(pars)


###################################################
### code chunk number 12: simdata
###################################################
data <- prepare_data(controls, true_parameters = pars, seed = 1)
plot(data)


###################################################
### code chunk number 13: data preparation
###################################################
controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = system.file("extdata", "dax.csv", package = "fHMM"),
                date_column = "Date",
                data_column = "Close",
                from        = "2000-01-01",
                to          = "2021-12-31",
                logreturns  = TRUE),
  fit    = list("runs" = 100)
)
controls <- set_controls(controls)
data <- prepare_data(controls)


###################################################
### code chunk number 14: model estimation (eval = FALSE)
###################################################
## dax_model_3t <- fit_model(data, seed = 1, verbose = FALSE)


###################################################
### code chunk number 15: access model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 16: model coefficients
###################################################
coef(dax_model_3t)


###################################################
### code chunk number 17: sdds
###################################################
plot(dax_model_3t, plot_type = "sdds")


###################################################
### code chunk number 18: ll
###################################################
plot(dax_model_3t, plot_type = "ll")


###################################################
### code chunk number 19: hhmm_model
###################################################
data(dax_vw_model)
plot(dax_vw_model, plot_type = "sdds")


###################################################
### code chunk number 20: load dax model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 21: dax decode states
###################################################
dax_model_3t <- decode_states(dax_model_3t)


###################################################
### code chunk number 22: dec_ts
###################################################
plot(dax_model_3t)


###################################################
### code chunk number 23: reorder states
###################################################
dax_model_3t <- reorder_states(dax_model_3t, 3:1)


###################################################
### code chunk number 24: predict
###################################################
predict(dax_model_3t, ahead = 10)


###################################################
### code chunk number 25: load dax data
###################################################
data(dax_model_3t)


###################################################
### code chunk number 26: compute residuals
###################################################
dax_model_3t <- compute_residuals(dax_model_3t)


###################################################
### code chunk number 27: residuals
###################################################
plot(dax_model_3t, plot_type = "pr")


###################################################
### code chunk number 28: jb test
###################################################
res <- dax_model_3t$residuals
tseries::jarque.bera.test(res)


###################################################
### code chunk number 29: compare models
###################################################
data(dax_model_2n)
data(dax_model_3t)
compare_models(dax_model_2n, dax_model_3t)


