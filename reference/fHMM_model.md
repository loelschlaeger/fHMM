# Constructor of a model object

This function constructs an object of class `fHMM_model`, which contains
details about the fitted (hierarchical) Hidden Markov model.

## Usage

``` r
fHMM_model(
  data,
  estimate,
  nlm_output,
  estimation_time,
  ll,
  lls,
  gradient,
  inverse_fisher,
  decoding
)

# S3 method for class 'fHMM_model'
print(x, ...)

# S3 method for class 'fHMM_model'
residuals(object, ...)

# S3 method for class 'fHMM_model'
summary(object, alpha = 0.05, ...)

# S3 method for class 'fHMM_model'
coef(object, alpha = 0.05, digits = 2, ...)

# S3 method for class 'fHMM_model'
AIC(object, ..., k = 2)

# S3 method for class 'fHMM_model'
BIC(object, ...)

# S3 method for class 'fHMM_model'
nobs(object, ...)

# S3 method for class 'fHMM_model'
logLik(object, ...)

npar(object, ...)

# S3 method for class 'fHMM_model'
npar(object, ...)

# S3 method for class 'fHMM_model'
predict(object, ahead = 5, alpha = 0.05, ...)
```

## Arguments

- data:

  \[`fHMM_data`\]  
  An object of class
  [`fHMM_data`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md).

- estimate:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector of unconstrained model estimates.

- nlm_output:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  The output of [`nlm`](https://rdrr.io/r/stats/nlm.html) for the
  selected optimization run.

- estimation_time:

  \[`difftime`\]  
  A `diff.time` object, the total estimation time.

- ll:

  \[`numeric(1)`\]  
  A `numeric`, the model log-likelihood.

- lls:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector, the model log-likelihoods in all optimization
  runs.

- gradient:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector, the gradient at the optimum.

- inverse_fisher:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector, the inverse Fisher information for each parameter.

- decoding:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A `numeric` vector, the decoded time series.

- x, object:

  \[`fHMM_model`\]  
  An object of class `fHMM_model`.

- ...:

  Currently not used.

- alpha:

  \[`numeric(1)`\]  
  A `numeric` between 0 and 1, the confidence level.

- digits:

  \[`integer(1)`\]  
  The number of decimal places.

- k:

  \[`numeric(1)`\]  
  Passed on to [`AIC`](https://rdrr.io/r/stats/AIC.html).

- ahead:

  \[`integer(1)`\]  
  The number of time points to predict ahead.

## Value

An object of class `fHMM_model`.
