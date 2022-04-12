### R code from vignette source 'fhmm_oelschlaeger_adam_michels.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 60, useFancyQuotes = FALSE)
library("fHMM")


###################################################
### code chunk number 2: example 1 dax define controls
###################################################
contr_dax <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = "dax.csv",
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE)
)


###################################################
### code chunk number 3: example 1 dax set controls
###################################################
contr_dax <- set_controls(contr_dax)
class(contr_dax)


###################################################
### code chunk number 4: example 2 simulation define controls
###################################################
contr_sim <- list(
  states  = 2,
  sdds    = "gamma(mu = 1|2)",
  horizon = 200,
  fit     = list(runs = 50)
)


###################################################
### code chunk number 5: example 2 simulation print controls
###################################################
(contr_sim <- set_controls(contr_sim))


###################################################
### code chunk number 6: example 3 hhmm set controls
###################################################
contr_hhmm <- list(
  hierarchy = TRUE,
  states    = c(2,2),
  sdds      = c("t(df = 1)", "t(df = 1)"),
  period    = "m",
  data      = list(file = c(system.file("extdata", "dax.csv", package = "fHMM"),
                            system.file("extdata", "vw.csv", package = "fHMM")),
                   date_column = c("Date","Date"),
                   data_column = c("Close","Close"),
                   from = "2015-01-01",
                   to = "2020-01-01",
                   logreturns = c(TRUE,TRUE),
                   merge = function(x) mean(x))
)
contr_hhmm <- set_controls(contr_hhmm)


###################################################
### code chunk number 7: download dax example (eval = FALSE)
###################################################
## download_data(symbol = "^GDAXI", from = "2000-01-01", to = Sys.Date())


###################################################
### code chunk number 8: example 1 dax prepare data
###################################################
data_dax <- prepare_data(contr_dax)
summary(data_dax)


###################################################
### code chunk number 9: dax-ts
###################################################
events <- fHMM_events(
  list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c("9/11 terrorist attack", "Bankruptcy of Lehman Brothers", 
               "First COVID-19 case in Germany")
    )
  )
plot(data_dax, events = events)


###################################################
### code chunk number 10: example 2 sim prepare data
###################################################
pars <- fHMM_parameters(
  controls = contr_sim, 
  Gamma = matrix(c(0.9,0.2,0.1,0.8), nrow = 2), 
  sigmas = c(0.1,0.5)
)
data_sim <- prepare_data(contr_sim, true_parameters = pars, seed = 1)


###################################################
### code chunk number 11: example 3 hhmm prepare data
###################################################
data_hhmm <- prepare_data(contr_hhmm)


###################################################
### code chunk number 12: data preparation
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
### code chunk number 13: model estimation (eval = FALSE)
###################################################
## dax_model_3t <- fit_model(data, seed = 1, verbose = FALSE)


###################################################
### code chunk number 14: access model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 15: model coefficients
###################################################
coef(dax_model_3t)


###################################################
### code chunk number 16: sdds
###################################################
plot(dax_model_3t, plot_type = "sdds")


###################################################
### code chunk number 17: ll
###################################################
plot(dax_model_3t, plot_type = "ll")


###################################################
### code chunk number 18: hhmm_model
###################################################
data(dax_vw_model)
plot(dax_vw_model, plot_type = "sdds")


###################################################
### code chunk number 19: load dax model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 20: dax decode states
###################################################
dax_model_3t <- decode_states(dax_model_3t)


###################################################
### code chunk number 21: dec_ts
###################################################
plot(dax_model_3t)


###################################################
### code chunk number 22: reorder states
###################################################
dax_model_3t <- reorder_states(dax_model_3t, 3:1)


###################################################
### code chunk number 23: predict
###################################################
predict(dax_model_3t, ahead = 10)


###################################################
### code chunk number 24: load dax data
###################################################
data(dax_model_3t)


###################################################
### code chunk number 25: compute residuals
###################################################
dax_model_3t <- compute_residuals(dax_model_3t)


###################################################
### code chunk number 26: residuals
###################################################
plot(dax_model_3t, plot_type = "pr")


###################################################
### code chunk number 27: jb test
###################################################
res <- dax_model_3t$residuals
tseries::jarque.bera.test(res)


###################################################
### code chunk number 28: compare models
###################################################
data(dax_model_2n)
data(dax_model_3t)
compare_models(dax_model_2n, dax_model_3t)


