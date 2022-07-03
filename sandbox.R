### load code ---------------------------------------------------------------
rm(list = ls())
library("magrittr")
devtools::load_all()
#install.packages("../fHMM_1.0.2.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))

### download data -----------------------------------------------------------
download_data(symbol = "^GDAXI", from = "2000-01-01", to = Sys.Date(),
              file = tempfile())

### simulated HMM -----------------------------------------------------------
seed <- 1
controls = list(
  states  = 2,
  sdds    = "lnorm(mu = 0|3)",
  horizon = 1000,
  fit     = list("runs" = 100)
)
controls %<>% set_controls
data <- prepare_data(controls, seed = seed)
data %>% summary
data %>% plot
model <- fit_model(data, ncluster = 7, seed = seed) %>%
  decode_states %>%
  compute_residuals
summary(model)
model %<>% reorder_states(state_order = 1:2)
compare_models(model)
model %>% plot("ll")
model %>% plot("sdds")
model %>% plot("pr")
model %>% plot("ts")
model %>% predict(ahead = 10)

### empirical HMM -----------------------------------------------------------
seed <- 1
controls = list(
  states = 3,
  sdds   = "t",
  data   = list(file        = "inst/extdata/dax.csv",
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE,
                from        = "2015-01-01"),
  fit    = list("runs" = 14)
)
controls %<>% set_controls
data <- prepare_data(controls)
summary(data)
events <- list(dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
               labels = c("9/11 terrorist attack", "Bankruptcy Lehman Brothers",
                          "First COVID-19 case Germany")) %>% fHMM_events
plot(data, events)
model <- fit_model(data, ncluster = 7, seed = seed) %>%
  decode_states %>%
  compute_residuals
summary(model)
model %>% plot("ll")
model %>% plot("sdds")
model %<>% reorder_states(state_order = c(3,1,2))
model %>% plot("pr")
model %>% plot("ts", events = events)
model %>% predict(ahead = 10)

### simulated HHMM ----------------------------------------------------------
seed <- 1
controls <- list(
  hierarchy = TRUE,
  states    = c(2,2),
  sdds      = c("t(sigma = 0.1, df = Inf)", 
                "gamma(sigma = 0.1)"),
  horizon   = c(50,10),
  fit       = list("runs" = 7)
)
controls %<>% set_controls
data <- prepare_data(controls, seed = seed)
summary(data)
plot(data)
model <- fit_model(data, ncluster = 7) %>% 
  decode_states %>%
  compute_residuals 
summary(model)
compare_models(model)
model %>% plot("ll")
model %>% plot("sdds")
model %<>% reorder_states(state_order = matrix(c(1,2,1,2,2,1),2,3))
model %>% plot("pr")
model %>% plot("ts")
model %>% predict(ahead = 10)

### empirical HHMM ----------------------------------------------------------
seed <- 1
controls <- list(
  hierarchy = TRUE,
  states    = c(2,2),
  sdds      = c("t(df = 1)", "t(df = 1)"),
  period    = "m",
  data      = list(file = c("inst/extdata/dax.csv", "inst/extdata/vw.csv"),
                   from = "2010-01-01",
                   to = "2020-01-01",
                   logreturns = c(TRUE,TRUE)),
  fit       = list("runs" = 100)
)
controls <- set_controls(controls)
data <- prepare_data(controls)
plot(data)
model <- fit_model(data, ncluster = 7, seed = seed) %>% 
  decode_states %>%
  compute_residuals
summary(model)
compare_models(model)
model %>% plot("ll")
model %>% plot("sdds")
model %>% plot("pr")
model %>% plot("ts", events = events)
model %>% predict(ahead = 10)

