# Define and validate model specifications

This function defines and validates specifications for model estimation.

## Usage

``` r
set_controls(
  controls = list(),
  hierarchy = FALSE,
  states = if (!hierarchy) 2 else c(2, 2),
  sdds = if (!hierarchy) "normal" else c("normal", "normal"),
  horizon = if (!hierarchy) 100 else c(100, 30),
  period = NA,
  data = NA,
  file = NA,
  date_column = if (!hierarchy) "Date" else c("Date", "Date"),
  data_column = if (!hierarchy) "Close" else c("Close", "Close"),
  from = NA,
  to = NA,
  logreturns = if (!hierarchy) FALSE else c(FALSE, FALSE),
  merge = function(x) mean(x),
  fit = list(),
  runs = 10,
  origin = FALSE,
  accept = 1:3,
  gradtol = 0.01,
  iterlim = 100,
  print.level = 0,
  steptol = 0.01
)

validate_controls(controls)

# S3 method for class 'fHMM_controls'
print(x, ...)

# S3 method for class 'fHMM_controls'
summary(object, ...)
```

## Arguments

- controls:

  \[[`list()`](https://rdrr.io/r/base/list.html) \| `fHMM_controls`\]  
  Either a `list` or an object of class `fHMM_controls`.

  The `list` can contain the following elements, which are described in
  more detail below:

  - `hierarchy`, defines a hierarchical HMM,

  - `states`, defines the number of states,

  - `sdds`, defines the state-dependent distributions,

  - `horizon`, defines the time horizon,

  - `period`, defines a flexible, periodic fine-scale time horizon,

  - `data`, a `list` of controls that define the data,

  - `fit`, a `list` of controls that define the model fitting

  Either none, all, or selected elements can be specified.

  Unspecified parameters are set to their default values.

  Important: Specifications in `controls` always override individual
  specifications.

- hierarchy:

  \[`logical(1)`\]  
  A `logical`, set to `TRUE` for a hierarchical HMM.

  If `hierarchy = TRUE`, some of the other controls must be specified
  for the coarse-scale and the fine-scale layer.

  By default, `hierarchy = FALSE`.

- states:

  \[`integer(1)` \| `integer(2)`\]  
  An `integer`, the number of states of the underlying Markov chain.

  If `hierarchy = TRUE`, `states` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `states = 2` if `hierarchy = FALSE` and `states = c(2, 2)`
  if `hierarchy = TRUE`.

- sdds:

  \[`character(1)` \| `character(2)`\]  
  A `character`, specifying the state-dependent distribution. One of

  - `"normal"` (the normal distribution),

  - `"lognormal"` (the log-normal distribution),

  - `"t"` (the t-distribution),

  - `"gamma"` (the gamma distribution),

  - `"poisson"` (the Poisson distribution).

  The distribution parameters, i.e. the

  - mean `mu`,

  - standard deviation `sigma` (not for the Poisson distribution),

  - degrees of freedom `df` (only for the t-distribution),

  can be fixed via, e.g., `"t(df = 1)"` or `"gamma(mu = 0, sigma = 1)"`.
  To fix different values of a parameter for different states, separate
  by "\|", e.g. `"poisson(mu = 1|2|3)"`.

  If `hierarchy = TRUE`, `sdds` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `sdds = "normal"` if `hierarchy = FALSE` and
  `sdds = c("normal", "normal")` if `hierarchy = TRUE`.

- horizon:

  \[`integer(1)` \| `integer(2)`\]  
  A `numeric`, specifying the length of the time horizon.

  If `hierarchy = TRUE`, `horizon` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `horizon = 100` if `hierarchy = FALSE` and
  `horizon = c(100, 30)` if `hierarchy = TRUE`.

  If `data` is specified (i.e., not `NA`), the first entry of `horizon`
  is ignored and the (coarse-scale) time horizon is defined by available
  data.

- period:

  \[`NA` \| `character(1)`\]  
  Only relevant if `hierarchy = TRUE`.

  In this case, a `character` which specifies a flexible, periodic
  fine-scale time horizon and can be one of

  - `"w"` for a week,

  - `"m"` for a month,

  - `"q"` for a quarter,

  - `"y"` for a year.

  By default, `period = NA`. If `period` is not `NA`, it overrules
  `horizon[2]`.

- data:

  \[`NA` \| [`list()`](https://rdrr.io/r/base/list.html)\]  
  Either `NA`, in which case data is simulated (the default), or a
  `list` of controls specifying the empirical data set.

  The `list` can contain the following elements, which are described in
  more detail below:

  - `file`, defines the data set,

  - `date_column`, defines the date column,

  - `data_column`, defines the data column,

  - `from`, defines a lower date limit,

  - `to`, defines an upper date limit,

  - `logreturns`, defines a data transformation to log-returns,

  - `merge`, defines the merging for coarse-scale observations.

  Either none, all, or selected elements can be specified.

  Unspecified parameters are set to their default values, see below.

  Specifications in `data` override individual specifications.

- file:

  \[`data.frame` \|
  [`character()`](https://rdrr.io/r/base/character.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A `data.frame` with data and dates for modeling.

  If `hierarchy = TRUE`, `file` can be a `list` of length 2. The first
  entry is a `data.frame` and provides the data for the coarse-scale
  layer, while the second entry corresponds to the fine-scale layer. If
  `file` is a single `data.frame`, then the same `data.frame` is used
  for both layers.

  Alternatively, it can be a `character` (of length two), the path to a
  .csv-file with financial data.

- date_column:

  \[`character(1)` \| `character(2)`\]  
  A `character`, the name of the column in `file` with dates.

  If `hierarchy = TRUE` and `file` is a `list` of two `data.frame`s,
  `data_column` must be a `vector` of length 2. The first entry
  corresponds to the coarse-scale layer, while the second entry
  corresponds to the fine-scale layer.

  By default, `date_column = "Date"`.

- data_column:

  \[`character(1)` \| `character(2)`\]  
  A `character`, the name of the column in `file` with observations.

  If `hierarchy = TRUE`, `data_column` must be a `vector` of length 2.
  The first entry corresponds to the coarse-scale layer, while the
  second entry corresponds to the fine-scale layer.

  By default, `data_column = "Close"` if `hierarchy = FALSE` and
  `data_column = c("Close", "Close")` if `hierarchy = TRUE`.

- from:

  \[`NA` \| `character(1)`\]  
  A `character` of the format `"YYYY-MM-DD"`, setting a lower date
  limit. No lower limit if `from = NA` (default).

- to:

  \[`NA` \| `character(1)`\]  
  A `character` of the format `"YYYY-MM-DD"`, setting an upper date
  limit. No lower limit if `to = NA` (default).

- logreturns:

  \[`logical(1)` \| `logical(2)`\]  
  A `logical`, if `TRUE` the data is transformed to log-returns.

  If `hierarchy = TRUE`, `logreturns` must be a `vector` of length 2.
  The first entry corresponds to the coarse-scale layer, while the
  second entry corresponds to the fine-scale layer.

  By default, `logreturns = FALSE` if `hierarchy = FALSE` and
  `logreturns = c(FALSE, FALSE)` if `hierarchy = TRUE`.

- merge:

  \[`function`\]  
  Only relevant if `hierarchy = TRUE`.

  In this case, a `function` which merges an input numeric vector of
  fine-scale data `x` into one coarse-scale observation. For example,

  - `merge = function(x) mean(x)` (default) defines the mean of the
    fine-scale data as the coarse-scale observation,

  - `merge = function(x) mean(abs(x))` for the mean of the absolute
    values,

  - `merge = function(x) sum(abs(x))` for the sum of the absolute
    values,

  - `merge = function(x) (tail(x, 1) - head(x, 1)) / head(x, 1)` for the
    relative change of the first to the last fine-scale observation.

- fit:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  A `list` of controls specifying the model fitting.

  The `list` can contain the following elements, which are described in
  more detail below:

  - `runs`, defines the number of numerical optimization runs,

  - `origin`, defines initialization at the true parameters,

  - `accept`, defines the set of accepted optimization runs,

  - `gradtol`, defines the gradient tolerance,

  - `iterlim`, defines the iteration limit,

  - `print.level`, defines the level of printing,

  - `steptol`, defines the minimum allowable relative step length.

  Either none, all, or selected elements can be specified.

  Unspecified parameters are set to their default values, see below.

  Specifications in `fit` override individual specifications.

- runs:

  \[`integer(1)`\]  
  An `integer`, setting the number of randomly initialized optimization
  runs of the model likelihood from which the best one is selected as
  the final model.

  By default, `runs = 10`.

- origin:

  \[`logical(1)`\]  
  Only relevant for simulated data, i.e., if the `data` control is `NA`.

  In this case, a `logical`. If `origin = TRUE` the optimization is
  initialized at the true parameter values. This sets `run = 1` and
  `accept = 1:5`.

  By default, `origin = FALSE`.

- accept:

  \[[`integer()`](https://rdrr.io/r/base/integer.html) \| `"all"`\]  
  An `integer` (vector), specifying which optimization runs are accepted
  based on the output code of [`nlm`](https://rdrr.io/r/stats/nlm.html).

  By default, `accept = 1:3`.

- gradtol:

  \[`numeric(1)`\]  
  A positive `numeric` value, specifying the gradient tolerance, passed
  on to [`nlm`](https://rdrr.io/r/stats/nlm.html).

  By default, `gradtol = 0.01`.

- iterlim:

  \[`integer(1)`\]  
  A positive `integer` value, specifying the iteration limit, passed on
  to [`nlm`](https://rdrr.io/r/stats/nlm.html).

  By default, `iterlim = 100`.

- print.level:

  \[`0` \| `1` \| `2`\]  
  One of `0`, `1`, and `2` to control the verbosity of the numerical
  likelihood optimization, passed on to
  [`nlm`](https://rdrr.io/r/stats/nlm.html).

  By default, `print.level = 0`.

- steptol:

  \[`numeric(1)`\]  
  A positive `numeric` value, specifying the step tolerance, passed on
  to [`nlm`](https://rdrr.io/r/stats/nlm.html).

  By default, `steptol = 0.01`.

- x, object:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

- ...:

  Currently not used.

## Value

An object of class `fHMM_controls`, which is a `list` that contains
model and estimation specifications.

## Details

See the [vignette on controls](https://loelschlaeger.de/fHMM/articles/)
for more details.

## Examples

``` r
# 2-state HMM with t-distributions for simulated data
set_controls(
  states = 2,   # the number of states
  sdds   = "t", # the state-dependent distribution
  runs   = 50   # the number of optimization runs
)
#> fHMM controls:
#> * hierarchy: FALSE 
#> * data type: simulated 
#> * number of states: 2 
#> * sdds: t() 
#> * number of runs: 50  

# 3-state HMM with normal distributions for the DAX closing prices
set_controls(
  states      = 3,
  sdds        = "normal",
  file        = download_data("^GDAXI"), # the data set
  date_column = "Date",                   # the column with the dates
  data_column = "Close"                   # the column with the data
)
#> fHMM controls:
#> * hierarchy: FALSE 
#> * data type: empirical 
#> * number of states: 3 
#> * sdds: normal() 
#> * number of runs: 10  

# hierarchical HMM with Gamma and Poisson state distributions
set_controls(
  hierarchy = TRUE,                  # defines a hierarchy
  states    = c(3, 2),               # coarse scale and fine scale states
  sdds      = c("gamma", "poisson"), # distributions for both layers
  horizon   = c(100, NA),            # 100 coarse-scale data points
  period    = "m"                    # monthly simulated fine-scale data
)
#> fHMM controls:
#> * hierarchy: TRUE 
#> * data type: simulated 
#> * number of states: 3 2 
#> * sdds: gamma() poisson() 
#> * number of runs: 10  

# hierarchical HMM with data from .csv-file
set_controls(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t", "t"),
  file      = c(               
    system.file("extdata", "dax.csv", package = "fHMM"),
    system.file("extdata", "dax.csv", package = "fHMM")
  ),
  date_column = c("Date", "Date"), 
  data_column = c("Close", "Close"),
  logreturns  = c(TRUE, TRUE)
)
#> fHMM controls:
#> * hierarchy: TRUE 
#> * data type: empirical 
#> * number of states: 3 2 
#> * sdds: t() t() 
#> * number of runs: 10  
```
