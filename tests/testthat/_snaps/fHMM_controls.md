# print method of controls works

    Code
      set_controls()
    Output
      Model: HMM 
      States: 2 
      Sdds: normal() 
      Data: simulated 

# summary method of controls works

    Code
      summary(set_controls())
    Output
       $ hierarchy: logi FALSE
       $ states   : num 2
       $ sdds     :List of 1
        ..$ :List of 6
        .. ..$ distr_class : chr "normal"
        .. ..$ label       : chr "normal()"
        .. ..$ fixed_pars  : list()
        .. ..$ sample      :function (n = 1, state, ...)  
        .. ..$ density     :function (x, state, ...)  
        .. ..$ distribution:function (q, state, ...)  
       $ horizon  : num 100
       $ period   : chr NA
       $ verbose  : logi TRUE
       $ data     : logi NA
       $ fit      :List of 8
        ..$ runs       : num 100
        ..$ origin     : logi FALSE
        ..$ accept     : int  1 2 3
        ..$ gradtol    : num 1e-06
        ..$ iterlim    : num 200
        ..$ print.level: num 0
        ..$ steptol    : num 1e-06
        ..$ ncluster   : num 1

