### load code
devtools::load_all()

### download data (optional)
download_data(name = "dax", symbol = "^GDAXI", path = ".")

### set controls
controls = list(
  path    = ".",
  states  = 2,
  sdds    = "t",
  horizon = 400,
  fit     = list("runs" = 50)
)

### fit (H)HMM
controls = set_controls(controls)
parameter = set_parameter(controls)
data = prepare_data(controls, true_parameter = NULL)
fit = fit_model(data, controls)
decoding = perform_decoding(data, model ,controls)

### visualize model results
colors = set_colors(controls)
plot_sdd(controls, data, model, decoding, colors)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
plot_ts(controls, data, decoding, colors, events)
pseudo_residuals(controls, data, model, decoding)

