### load code ---------------------------------------------------------------
rm(list = ls())
library(magrittr)
devtools::load_all()
#install.packages("../fHMM_0.3.0.9000.tar.gz", repos = NULL, type = "source", INSTALL_opts = c('--no-lock'))

### download data -----------------------------------------------------------
download_data(symbol = "^GDAXI", file = "dax.csv", verbose = FALSE)
download_data(symbol = "VOW3.DE", file = "vw.csv", verbose = FALSE)

### simulated HMM -----------------------------------------------------------
controls = list(
  states  = 2,
  sdds    = "t(mu = -1|1, df = Inf)",
  horizon = 100,
  fit     = list("runs" = 100)
)
controls %<>% set_controls
data = prepare_data(controls)
summary(data)
plot(data)
model = fit_model(data, ncluster = 7) %>%
  decode_states %>%
  compute_residuals
summary(model)
model %<>% reorder_states(state_order = 2:1)
compare(model)
model %>% plot("ll")
model %>% plot("sdds")
model %>% plot("pr")
model %>% plot("ts")
predict(model, ahead = 10)

### empirical HMM -----------------------------------------------------------
controls = list(
  states = 2,
  sdds   = "t",
  data   = list(file        = "dax.csv",
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE)
)
controls %<>% set_controls
data = prepare_data(controls)
summary(data)
events = list(dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
              labels = c("9/11 terrorist attack", "Bankruptcy Lehman Brothers",
                         "First COVID-19 case Germany")) %>% fHMM_events
plot(data, events)
model = fit_model(data, ncluster = 7) %>%
  decode_states %>%
  compute_residuals
summary(model)
model %>% plot("ll")
model %>% plot("sdds")
model %>% plot("pr")
model %>% plot("ts", events)

### simulated HHMM ----------------------------------------------------------
controls = list(
  hierarchy = TRUE,
  states    = c(2,2),
  sdds      = c("t(mu = -1|1, sigma = 0.1, df = Inf)", 
                "t(mu = -1|1, sigma = 0.1, df = Inf)"),
  horizon   = c(100,30),
  fit       = list("runs" = 10)
)
controls %<>% set_controls
data = prepare_data(controls)
plot(data)
model = fit_model(data, ncluster = 7) %>% 
  decode_states %>%
  compute_residuals 
summary(model)
compare(model)
model %>% plot("ll")
#TODO: model %>% plot("sdds")
#TODO: model %>% plot("pr")
#TODO: model %>% plot("ts", events)

### empirical HHMM ----------------------------------------------------------
controls = list(
  hierarchy = TRUE,
  horizon = c(100, NA),
  sdds = c("t(df = 1)", "t(df = 1)"),
  period = "m",
  data = list(file = c("dax.csv","vw.csv"),
              logreturns = c(TRUE,TRUE))
)
controls = set_controls(controls)
data = prepare_data(controls)
model = fit_model(data)
summary(model)


