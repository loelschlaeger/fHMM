# checks of controls work

    Code
      set_controls(controls)
    Output
      Controls:
      * hierarchy: FALSE 
      * data type: simulated 
      * number of states: 2 
      * sdds: t 
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
            simulated           merge 
               "TRUE"              NA 

---

    Code
      set_controls(controls)
    Output
      Controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t t 
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
      Controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t t 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy          period         states1         states2       sdds.name 
               "TRUE"             "w"             "2"             "2"             "t" 
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
      Controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t t 
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
      Controls:
      * hierarchy: TRUE 
      * data type: simulated 
      * number of states: 2 2 
      * sdds: t t 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
            hierarchy        horizon1        horizon2          period         states1 
               "TRUE"           "100"            "30"             "w"             "2" 
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
      Controls:
      * hierarchy: FALSE 
      * data type: empirical 
      * number of states: 2 
      * sdds: t 
      * number of runs: 100  

---

    Code
      unlist(set_controls(controls))
    Output
                                                               states 
                                                                  "2" 
                                                            sdds.name 
                                                                  "t" 
                                                              horizon 
                                                                "400" 
                                                            data.file 
      "C:\\Users\\Lennart\\AppData\\Local\\Temp\\RtmpCCL0vX\\dax.csv" 
                                                          data.column 
                                                              "Close" 
                                                            data.from 
                                                                   NA 
                                                              data.to 
                                                                   NA 
                                                      data.logreturns 
                                                              "FALSE" 
                                                           data.merge 
                                                                   NA 
                                                            hierarchy 
                                                              "FALSE" 
                                                               period 
                                                                   NA 
                                                             fit.runs 
                                                                "100" 
                                                           fit.origin 
                                                              "FALSE" 
                                                          fit.accept1 
                                                                  "1" 
                                                          fit.accept2 
                                                                  "2" 
                                                          fit.accept3 
                                                                  "3" 
                                                          fit.gradtol 
                                                              "1e-06" 
                                                          fit.iterlim 
                                                                "200" 
                                                      fit.print.level 
                                                                  "0" 
                                                          fit.steptol 
                                                              "1e-06" 
                                                            simulated 
                                                              "FALSE" 
                                                                merge 
                                                                   NA 

