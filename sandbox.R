### load code
devtools::load_all()

### download data
download_data(symbol = "^GDAXI", file = "dax.csv")

### set controls
controls = list(
  path    = ".",
  states  = 2,
  sdds    = "t",
  horizon = 400,
  fit     = list("runs" = 50)
)
controls = set_controls(controls)

### set true parameters
Gamma = matrix(c(0.8,0.1,0.2,0.9),2,2)
mus = c(-1,1)
sigmas = c(0.5,2)
dfs = c(1,Inf)
true_parameter = set_parameters(controls, Gamma = Gamma, mus = mus, 
                                sigmas = sigmas, dfs = dfs)

### prepare data
data = prepare_data(controls, true_parameter)

### fit model
fit = fit_model(data, controls)

### decode data
decoding = perform_decoding(data, model, controls)

### plot results
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
plot(type = "", controls, data, model, decoding, events)


