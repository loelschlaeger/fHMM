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
  sdds    = "gamma(mu = -1|1)",
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
### code chunk number 5: get data
###################################################
system.file("extdata", "dax.csv", package = "fHMM")
system.file("extdata", "vw.csv", package = "fHMM")


###################################################
### code chunk number 6: prepare_data example
###################################################
controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = system.file("extdata", "dax.csv", package = "fHMM"),
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE)
)
controls <- set_controls(controls)
data <- prepare_data(controls)
summary(data)


###################################################
### code chunk number 7: download dax example
###################################################
download_data(symbol = "^GDAXI", from = "2000-01-01", to = Sys.Date())


###################################################
### code chunk number 8: ts
###################################################
events <- fHMM_events(
  list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c("9/11 terrorist attack", "Bankruptcy of Lehman Brothers", 
               "First COVID-19 case in Germany")
    )
  )
print(events)
plot(data, events = events)


###################################################
### code chunk number 9: data preparation
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
### code chunk number 10: access model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 11: summarize model
###################################################
summary(dax_model_3t)


###################################################
### code chunk number 12: sdds
###################################################
plot(dax_model_3t, plot_type = "sdds")


###################################################
### code chunk number 13: ll
###################################################
plot(dax_model_3t, plot_type = "ll")


###################################################
### code chunk number 14: load dax model
###################################################
data(dax_model_3t)


###################################################
### code chunk number 15: dax decode states
###################################################
dax_model_3t <- decode_states(dax_model_3t)


###################################################
### code chunk number 16: dec_ts
###################################################
plot(dax_model_3t)


###################################################
### code chunk number 17: reorder states
###################################################
dax_model_3t <- reorder_states(dax_model_3t, 3:1)


###################################################
### code chunk number 18: predict
###################################################
predict(dax_model_3t, ahead = 10)


###################################################
### code chunk number 19: load dax data
###################################################
data(dax_model_3t)


###################################################
### code chunk number 20: compute residuals
###################################################
dax_model_3t <- compute_residuals(dax_model_3t)


###################################################
### code chunk number 21: residuals
###################################################
plot(dax_model_3t, plot_type = "pr")


###################################################
### code chunk number 22: jb test
###################################################
res <- dax_model_3t$residuals
tseries::jarque.bera.test(res)


###################################################
### code chunk number 23: compare models
###################################################
data(dax_model_2n)
data(dax_model_3t)
compare_models(dax_model_2n, dax_model_3t)


