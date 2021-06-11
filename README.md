# fHMM <img src='sticker/StickerShadesOfBlue.png' align="right" height="136" />
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)

👉 Fitting (hierarchical) hidden Markov models to financial data.

💬 Found a bug? Request a feature? Please [tell us](https://github.com/loelschlaeger/fHMM/issues)!

## How to get started?
1. Run `install.packages("fHMM")` and `library("fHMM")` in your R console. to install fHMM.
2. Specify your model by setting the [controls](#specifying-controls). See below for [examples](#examples).
3. Execute `fit_hmm(controls)`.

## Vignettes
We provide several vignettes that give answers to the following questions:
- How to specify a model in fHMM?
- How to process financial data in fHMM?
- How to interprete fHMM outputs?
- How to debug fHMM?
- How does fHMM process model parameters?

## Examples
### Fitting a 2-state HMM to simulated data using gamma-distributions
Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_2_sim_gamma) for the results.
```R
### set controls
controls = list(
  path    = ".",
  id      = "HMM_2_sim_gamma",
  sdds    = c("gamma",NA),
  states  = c(2,0),
  horizon = c(5000,NA),
  fit     = list("seed" = 1)
)

### fit (H)HMM
fit_hmm(controls)
```
### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020 using t-distributions
Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_3_DAX) for the results.
```R
### download data (optional)
download_data("dax","^GDAXI",path=".")

### set controls
controls = list(
  path    = ".",
  id      = "HMM_3_DAX",
  states  = c(3,0),
  sdds    = c("t",NA),
  data    = list("source" = c("dax",NA), "col" = c("Close",NA), "truncate" = c("2000-01-03","2020-12-30"))
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
download_data("dax","^GDAXI",path=".")
download_data("dbk","DBK.DE",path=".")

### set controls
controls = list(
  path    = ".",
  id      = "HHMM_2_2_DAX_DBK_t_t",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","dbk"), 
                 "column"       = c("Close","Close"), 
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
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
