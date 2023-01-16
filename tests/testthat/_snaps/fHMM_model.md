# HHMM fitting works

    Code
      model
    Output
      fHMM fitted model:
      * total estimation time: 1 mins 
      * accepted runs: 2 of 2 
      * log-likelihood: -75.18242 

---

    Code
      summary(model)
    Output
      Summary of fHMM model
      
        simulated hierarchy        LL      AIC     BIC
      1      TRUE      TRUE -75.18242 174.3648 199.497
      
      State-dependent distributions:
      t(sigma = 0.1, df = Inf) gamma(sigma = 0.1) 
      
      Estimates:
                          lb   estimate         ub     true
      Gamma_2.1    6.172e-01  6.172e-01  6.172e-01  0.29065
      Gamma_1.2    3.611e-14  3.611e-14  3.611e-14  0.68330
      mu_1        -1.471e-01 -8.510e-02 -2.312e-02 -0.79832
      mu_2         2.777e-02  2.777e-02  2.777e-02  0.20168
      Gamma*1_2.1  1.758e-01  3.228e-01  5.158e-01  0.75335
      Gamma*1_1.2  2.075e-01  3.938e-01  6.171e-01  0.08551
      mu*1_1       2.568e-01  2.919e-01  3.317e-01  0.24885
      mu*1_2       7.955e-01  8.316e-01  8.694e-01  0.74885
      Gamma*2_2.1  7.164e-01  7.164e-01  7.164e-01  0.47158
      Gamma*2_1.2  5.059e-01  5.059e-01  5.059e-01  0.68509
      mu*2_1       8.712e-03  8.712e-03  8.712e-03  0.35881
      mu*2_2       2.042e+01  2.042e+01  2.042e+01  0.85881

# log-normal sdds works

    Code
      model
    Output
      fHMM fitted model:
      * total estimation time: 1 mins 
      * accepted runs: 5 of 100 
      * log-likelihood: -3954.886 

---

    Code
      summary(model)
    Output
      Summary of fHMM model
      
        simulated hierarchy        LL      AIC      BIC
      1      TRUE     FALSE -3954.886 7917.772 7937.403
      
      State-dependent distributions:
      lnorm(mu = 1|3) 
      
      Estimates:
                    lb estimate     ub   true
      Gamma_2.1 0.2688   0.3122 0.3591 0.2906
      Gamma_1.2 0.6109   0.6854 0.7515 0.6833
      sigma_1   0.4302   0.4771 0.5291 0.4492
      sigma_2   0.9030   0.9671 1.0356 0.9492

---

    Code
      summary(model)
    Output
      Summary of fHMM model
      
        simulated hierarchy        LL      AIC      BIC
      1      TRUE     FALSE -3954.886 7917.772 7937.403
      
      State-dependent distributions:
      lnorm(mu = 1|3) 
      
      Estimates:
                    lb estimate     ub   true
      Gamma_2.1 0.2688   0.3122 0.3591 0.2906
      Gamma_1.2 0.6109   0.6854 0.7515 0.6833
      sigma_1   0.4302   0.4771 0.5291 0.4492
      sigma_2   0.9030   0.9671 1.0356 0.9492
      
      States:
          decoded
      true   1   2
         1 279  29
         2  58 634

