# checks of controls for simulated HMM work

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: FALSE 
      * data type: simulated 
      * number of states: 2 
      * sdds: t() 
      * number of runs: 50  

---

    Code
      unlist(set_controls(controls))
    Output
               states       sdds.name         horizon        fit.runs      fit.origin 
                  "2"             "t"           "400"            "50"         "FALSE" 
          fit.accept1     fit.accept2     fit.accept3     fit.gradtol     fit.iterlim 
                  "1"             "2"             "3"         "1e-06"           "200" 
      fit.print.level     fit.steptol       hierarchy          period            data 
                  "0"         "1e-06"         "FALSE"              NA              NA 
            simulated 
               "TRUE" 

# checks of controls for empirical HMM work

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: FALSE 
      * data type: empirical 
      * number of states: 2 
      * sdds: t() 
      * number of runs: 100  

# checks of controls for simulated HHMM work

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy        horizon1        horizon2         states1         states2 
               "TRUE"           "100"              NA             "2"             "2" 
            sdds.name       sdds.name          period            data        fit.runs 
                  "t"             "t"             "m"              NA           "100" 
           fit.origin     fit.accept1     fit.accept2     fit.accept3     fit.gradtol 
              "FALSE"             "1"             "2"             "3"         "1e-06" 
          fit.iterlim fit.print.level     fit.steptol       simulated 
                "200"             "0"         "1e-06"          "TRUE" 

---

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy          period         states1         states2       sdds.name 
               "TRUE"              NA             "2"             "2"             "t" 
            sdds.name        horizon1        horizon2            data        fit.runs 
                  "t"           "100"            "30"              NA           "100" 
           fit.origin     fit.accept1     fit.accept2     fit.accept3     fit.gradtol 
              "FALSE"             "1"             "2"             "3"         "1e-06" 
          fit.iterlim fit.print.level     fit.steptol       simulated 
                "200"             "0"         "1e-06"          "TRUE" 

---

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy        horizon1        horizon2          period         states1 
               "TRUE"           "100"              NA             "w"             "2" 
              states2       sdds.name       sdds.name            data        fit.runs 
                  "2"             "t"             "t"              NA           "100" 
           fit.origin     fit.accept1     fit.accept2     fit.accept3     fit.gradtol 
              "FALSE"             "1"             "2"             "3"         "1e-06" 
          fit.iterlim fit.print.level     fit.steptol       simulated 
                "200"             "0"         "1e-06"          "TRUE" 

---

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy        horizon1        horizon2          period         states1 
               "TRUE"           "100"            "30"              NA             "2" 
              states2       sdds.name       sdds.name            data        fit.runs 
                  "2"             "t"             "t"              NA           "100" 
           fit.origin     fit.accept1     fit.accept2     fit.accept3     fit.gradtol 
              "FALSE"             "1"             "2"             "3"         "1e-06" 
          fit.iterlim fit.print.level     fit.steptol       simulated 
                "200"             "0"         "1e-06"          "TRUE" 

# checks of controls for empirical HHMM work

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: empirical 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

---

    Code
      set_controls(controls)
    Output
      fHMM controls:
      * hierarchy: TRUE 
      * data type: empirical 
      * number of states: 2 2 
      * sdds: t() t() 
      * number of runs: 100  

