# Simulated 2-state HMM with gamma distributions

A pre-computed 2-state HMM with state-dependent gamma distributions with
means fixed to `0.5` and `2` on `500` simulated observations.

## Usage

``` r
data("sim_model_2gamma")
```

## Format

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Details

The model was estimated via:


    controls <- set_controls(
      states  = 2,
      sdds    = "gamma(mu = 1|2)",
      horizon = 200,
      runs    = 10
    )
    pars <- fHMM_parameters(
      controls = controls,
      Gamma = matrix(c(0.9, 0.2, 0.1, 0.8), nrow = 2),
      sigma = c(0.5, 1),
      seed = 1
    )
    data_sim <- prepare_data(controls, true_parameters = pars, seed = 1)
    sim_model_2gamma <- fit_model(data_sim, seed = 1)
    sim_model_2gamma <- decode_states(sim_model_2gamma)
    sim_model_2gamma <- compute_residuals(sim_model_2gamma)
    summary(sim_model_2gamma)
