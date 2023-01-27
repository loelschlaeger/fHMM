# fHMM 1.0.3.9000

* The `plot.fHMM_model()` function now has the additional argument `ll_relative` (default is `TRUE`) to plot the relative log-likelihood values when `plot_type = "ll"`.

# fHMM 1.0.3

* We added the following methods for an `fHMM_model` object: `AIC()`, `BIC()`, `logLik()`, `nobs()`, `npar()`, `residuals()`.
* The log-normal distribution can now be estimated by setting `sdds = "lnorm"` in the `controls` object.

# fHMM 1.0.2

* Fixed bug in `reorder_states()` that did not order the fine-scale parameter sets when the coarse-scale order was changed.
* Fixed bug in `parameter_labels()` that returned the wrong order of parameter labels.
* Changed plot type of simulated data to lines.

# fHMM 1.0.1

* In the vignette 'Controls', in the section about example specifications for `controls`, corrected `sdds = "gamma(mu = -1|1)"` to `sdds = "gamma(mu = 0.5|2)"` because mean of the Gamma distribution must be positive.
* Added `digits` argument to `print.fHMM_predict()`.
* Fixed bug in `reorder_states()` that allowed for misspecification of `state_order`.
* Added option to `fit_model()` to initialize at the estimates of another model (#73).

# fHMM 1.0.0

* Enhanced the package by S3 classes.
* Added more `controls` specifications.
* Included a prediction function.
* Improved documentations.

# fHMM 0.3.0

* Added vignettes.
* Improved specification of `controls`. 
* Created a package start-up message.
* Fixed minor bugs.

# fHMM 0.2.0

* Improved documentation of functions and README.
* Improved specification of `controls`. (#37 and #38)

# fHMM 0.1.0

* Initial version.
