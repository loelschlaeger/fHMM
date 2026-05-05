# Model selection

Model selection involves the choice of a family for the state-dependent
distribution and the selection of the number of states. This
vignette[^1] introduces model selection in
[fHMM](https://loelschlaeger.de/fHMM/).

## Information criteria

Common model selection tools are information criteria, such as the
Akaike information criterion (AIC) or the Bayesian information criterion
(BIC), both of which aim at finding a compromise between model fit and
model complexity.

The AIC is defined as
``` math
\begin{align*}
\text{AIC} = - 2 \log (\mathcal{L}^\text{(H)HMM}(\theta,(\theta^{*(i)})_i\mid (X_t)_t,((X^*_{t,t^*})_{t^*})_t)) + 2 p,
\end{align*}
```
where $`p`$ denotes the number of parameters, while the BIC is defined
as
``` math
\begin{align*}
\text{BIC} = - 2 \log (\mathcal{L}^\text{(H)HMM}(\theta,(\theta^{*(i)})_i\mid (X_t)_t,((X^*_{t,t^*})_{t^*})_t)) + \log(T) p,
\end{align*}
```
where $`T`$ is the total number of observations.

## Challenges associated with model selection

In practice, however, information criteria often favor overly complex
models. Real data typically exhibit more structure than can actually be
captured by the model. This can be the case if the true state-dependent
distributions are too complex to be fully modeled by some (rather
simple) parametric distribution, or if certain temporal patterns are
neglected in the model formulation. Additional states may be able to
capture this structure, which can lead to an increased goodness of fit
that outweighs the higher model complexity. However, as models with too
many states are difficult to interpret and are therefore often not
desired, information criteria should be treated with some caution and
only considered as a rough guidance. For an in-depth discussion of
pitfalls, practical challenges, and pragmatic solutions regarding model
selection, see Pohle et al. ([2017](#ref-poh17)).

## The `compare_models()` function

The [fHMM](https://loelschlaeger.de/fHMM/) package provides a convenient
tool for comparing different models via the
[`compare_models()`](https://loelschlaeger.de/fHMM/reference/compare_models.md)
function. The models (arbitrarily many) can be directly passed to the
[`compare_models()`](https://loelschlaeger.de/fHMM/reference/compare_models.md)
function that returns an overview of the above model selection criteria.
Below, we compare a 2-state HMM with normal state-dependent
distributions with a 3-state HMM with state-dependent t-distributions
for the DAX data, where the more complex model is clearly preferred:

``` r

data(dax_model_2n)
data(dax_model_3t)
compare_models(dax_model_2n, dax_model_3t)
#>              parameters loglikelihood       AIC       BIC
#> dax_model_2n          6      17403.61 -34795.21 -34755.13
#> dax_model_3t         15      17650.02 -35270.05 -35169.85
```

## References

Pohle, J., R. Langrock, F. M. van Beest, and N. M. Schmidt. 2017.
“Selecting the Number of States in Hidden Markov Models: Pragmatic
Solutions Illustrated Using Animal Movement.” *Journal of Agricultural,
Biological and Environmental Statistics* 22 (3): 270–93.

[^1]: This vignette was built using R 4.6.0 with the
    [fHMM](https://loelschlaeger.de/fHMM/) 1.4.3 package.
