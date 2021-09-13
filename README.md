# fHMM <img src="man/figures/sticker.png" align="right" height=136 />

[![R-CMD-check](https://github.com/loelschlaeger/fHMM/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/fHMM/actions)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)

👉 Fitting (hierarchical) hidden Markov models to financial data.

💬 Found a bug? Request a feature? Please [tell us](https://github.com/loelschlaeger/fHMM/issues)!

📝 In R, type `citation("fHMM")` for citing this package in publications.

## How to get started?

1. Run `install.packages("fHMM")` and `library("fHMM")` in your R console. to install **fHMM**.
2. Specify your model by defining the named list `controls`. 
3. Execute `fit_hmm(controls)`.

We provide vignettes that give answers to the following FAQs:
- How to specify a model in **fHMM**?
- How to process financial data in **fHMM**?
- How to interpret **fHMM** outputs?
- How to debug **fHMM**?
- How does **fHMM** process model parameters?

## Examples

### Fitting a 2-state HMM to simulated data using gamma-distributions

Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_2_sim_gamma) for the results.

```R
### set controls
controls = list(
  path    = ".",
  id      = "HMM_2_sim_gamma",
  model   = "hmm",
  states  = 2,
  sdds    = "gamma",
  horizon = 5000,
  fit     = list("seed" = 1)
)

### fit (H)HMM
fit_hmm(controls)
```

### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020 using t-distributions

Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_3_DAX) for the results.

```R
### download data (optional)
download_data(name = "dax", symbol = "^GDAXI", path = ".")

### set controls
controls = list(
  path    = ".",
  id      = "HMM_3_DAX",
  model   = "hmm",
  states  = 3,
  sdds    = "t",
  data    = list("source" = "dax", 
                 "column" = "Close", 
                 "truncate" = c("2000-01-03","2020-12-30")
                 )
)

### define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### fit (H)HMM
fit_hmm(controls,events)
```

### Fitting a (2,2)-state HHMM jointly to the DAX and the Deutsche Bank stock

Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HHMM_2_2_DAX_DBK_t_t) for the results.

```R
### download data (optional)
download_data(name = "dax", symbol = "^GDAXI", path = ".")
download_data(name = "dbk", symbol = "DBK.DE", path = ".")

### set controls
controls = list(
  path    = ".",
  id      = "HHMM_2_2_DAX_DBK_t_t",
  model   = "hhmm",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","dbk"), 
                 "column"       = c("Close","Close"), 
                 "cs_transform" = function(x) (tail(x,1)-head(x,1))/head(x,1), 
                 "log_returns"  = c(FALSE,TRUE),
                 "truncate"     = c("2000-01-01",NA)),
  fit     = list("runs" = 100)
)

### define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### fit (H)HMM
fit_hmm(controls,events)

```
