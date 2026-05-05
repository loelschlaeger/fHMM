# Changelog

## fHMM 1.4.3

- Improved documentation of input types and input checks.

- Small documentation fixes.

- Removed renv.

## fHMM 1.4.2

CRAN release: 2025-03-24

- Let
  [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  fail gracefully with an informative message if the Yahoo Finance
  resource is not available or has changed.

- Restored test coverage via codecov.io.

- Fixed invalid URLs.

## fHMM 1.4.1

CRAN release: 2024-09-16

- Removed stale import of `oeli::check_date()`.

- Updated
  [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  to work with the new Yahoo Finance API.

## fHMM 1.4.0

CRAN release: 2024-08-26

- Fixed a bug around the `period` control
  ([\#93](https://github.com/loelschlaeger/fHMM/issues/93), thanks to
  [@dongsen86](https://github.com/dongsen86)).

- Fixed date conversion to
  [`character()`](https://rdrr.io/r/base/character.html) (thanks to
  Hee-Young Kim).

## fHMM 1.3.1

CRAN release: 2024-05-31

- Added citation to JSS paper in DESCRIPTION.

## fHMM 1.3.0

CRAN release: 2024-04-30

- Improved initialization of the numerical likelihood optimization.

- Now the states after model estimation are automatically ordered
  according to the estimated mean of the state-dependent distributions,
  see
  [`reorder_states()`](https://loelschlaeger.de/fHMM/reference/reorder_states.md)
  with the new (default) option `state_order = "mean"`.

- Re-fitted the example models contained in the package.

## fHMM 1.2.2

CRAN release: 2024-02-24

- Added examples to
  [`fit_model()`](https://loelschlaeger.de/fHMM/reference/fit_model.md).

- Small code improvements in file `ll.cpp`.

## fHMM 1.2.1

CRAN release: 2024-01-15

- Small bug fix when computing the stationary distribution.

## fHMM 1.2.0

CRAN release: 2023-12-13

- Controls can now be provided separately for the
  [`set_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  function.

- The arguments in
  [`fHMM_parameters()`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md)
  for model parameters were slightly renamed as follows:

  - `mus` -\> `mu`

  - `sigmas` -\> `sigma`

  - `dfs` -\> `df`

  - `Gammas_star` -\> `Gamma_star`

  - `mus_star` -\> `mu_star`

  - `sigmas_star` -\> `sigma_star`

  - `dfs_star` -\> `df_star`

- The log-normal state-dependent distribution is renamed: `lnorm` -\>
  `lognormal`.

- Two more state-dependent distributions were added: `normal` and
  `poisson`.

- The Viterbi algorithm can be directly accessed via
  [`viterbi()`](https://loelschlaeger.de/fHMM/reference/decode_states.md).

- Renamed `simulate_data()` -\>
  [`simulate_hmm()`](https://loelschlaeger.de/fHMM/reference/simulate_hmm.md)
  to make the functionality clearer. Furthermore, this function is now
  exported and can be used outside of the package to simulate HMM data.

- [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  no longer saves a .csv-file but returns the data as a `data.frame`.
  Its `verbose` argument is removed because the function no longer
  prints any messages.

- The utilities (i.e., all functions with roxygen tag `@keywords utils`)
  were moved to the [`{oeli}`](https://loelschlaeger.de/oeli/) package.

## fHMM 1.1.1

CRAN release: 2023-10-12

- Fixed documenting the new special sentinel “\_PACKAGE” for the package
  help file, see <https://github.com/r-lib/roxygen2/issues/1491>.

## fHMM 1.1.0

CRAN release: 2023-02-14

- Extended the time horizon of saved data and updated models for
  demonstration.

- The
  [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  function now returns the data as a `data.frame` by default. However,
  specifying argument `file` still allows for saving the data as a .csv
  file.

- The
  [`plot.fHMM_model()`](https://loelschlaeger.de/fHMM/reference/plot.fHMM_model.md)
  function now has the additional argument `ll_relative` (default is
  `TRUE`) to plot the relative log-likelihood values when
  `plot_type = "ll"`.

- Significantly increased the test coverage and fixed minor bugs.

- Changed color of time series plot from `"lightgray"` to `"black"` for
  better readability.

- Added a title to the time series plot when calling
  `plot.fHMM_model(plot_type = "ts")`. Additionally, a time interval
  with arguments `from` and `to` can be selected to zoom into the data.

## fHMM 1.0.3

CRAN release: 2022-07-07

- Added the following methods for an `fHMM_model` object:
  [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html),
  [`npar()`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html).

- The log-normal distribution can now be estimated by setting
  `sdds = "lnorm"` in the `controls` object.

## fHMM 1.0.2

CRAN release: 2022-05-02

- Fixed bug in
  [`reorder_states()`](https://loelschlaeger.de/fHMM/reference/reorder_states.md)
  that did not order the fine-scale parameter sets when the coarse-scale
  order was changed.

- Fixed bug in
  [`parameter_labels()`](https://loelschlaeger.de/fHMM/reference/parameter_labels.md)
  that returned the wrong order of parameter labels.

- Changed plot type of simulated data to lines.

## fHMM 1.0.1

CRAN release: 2022-04-07

- In the vignette on controls, in the section about example
  specifications for `controls`, corrected `sdds = "gamma(mu = -1|1)"`
  to `sdds = "gamma(mu = 0.5|2)"` because mean of the Gamma distribution
  must be positive.

- Added `digits` argument to `print.fHMM_predict()`.

- Fixed bug in
  [`reorder_states()`](https://loelschlaeger.de/fHMM/reference/reorder_states.md)
  that allowed for misspecification of `state_order`.

- Added option to
  [`fit_model()`](https://loelschlaeger.de/fHMM/reference/fit_model.md)
  to initialize at the estimates of another model
  ([\#73](https://github.com/loelschlaeger/fHMM/issues/73)).

## fHMM 1.0.0

CRAN release: 2022-03-14

- Enhanced the package by S3 classes.

- Added more `controls` specifications.

- Included a prediction function.

- Improved documentations.

## fHMM 0.3.0

CRAN release: 2021-06-16

- Added vignettes.

- Improved specification of `controls`.

- Fixed minor bugs.

## fHMM 0.2.0

CRAN release: 2021-03-13

- Improved documentation of functions and README.

- Improved specification of `controls`.
  ([\#37](https://github.com/loelschlaeger/fHMM/issues/37) and
  [\#38](https://github.com/loelschlaeger/fHMM/issues/38))

## fHMM 0.1.0

CRAN release: 2021-02-22

- Initial version.
