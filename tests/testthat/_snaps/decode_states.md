# state decoding input checks work

    Code
      summary(dax_model_3t)
    Output
      Summary of fHMM model
      
        simulated hierarchy       LL       AIC       BIC
      1     FALSE     FALSE 17649.52 -35269.03 -35168.84
      
      State-dependent distributions:
      t() 
      
      Estimates:
                        lb   estimate        ub
      Gamma_2.1  1.286e-02  2.007e-02 3.113e-02
      Gamma_3.1  1.208e-06  1.198e-06 1.180e-06
      Gamma_1.2  1.557e-02  2.489e-02 3.959e-02
      Gamma_3.2  1.036e-02  1.877e-02 3.378e-02
      Gamma_1.3  4.119e-07  4.080e-07 4.019e-07
      Gamma_2.3  2.935e-03  5.275e-03 9.422e-03
      mu_1       9.655e-04  1.271e-03 1.576e-03
      mu_2      -8.483e-04 -3.102e-04 2.278e-04
      mu_3      -3.813e-03 -1.760e-03 2.932e-04
      sigma_1    5.417e-03  5.853e-03 6.324e-03
      sigma_2    1.278e-02  1.330e-02 1.384e-02
      sigma_3    2.348e-02  2.579e-02 2.832e-02
      df_1       3.957e+00  5.198e+00 6.828e+00
      df_2       3.870e+08  3.870e+08 3.870e+08
      df_3       5.549e+00  1.078e+01 2.095e+01
      
      States:
      decoded
         1    2    3 
      2278 2900  704 
      
      Residuals:
           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
      -3.519694 -0.658831  0.009613 -0.002206  0.669598  3.905726 

