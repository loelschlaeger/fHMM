# checks of controls work

    Code
      controls
    Output
      Controls:
      * path: \\fs-home.uni-bielefeld.de\home\loelschlaeger\fHMM\tests\testthat 
      * model type: hmm 
      * data type: simulated 
      * number of states: 2 
      * SDDs: t 
      * number of runs: 50  

---

    Code
      unlist(controls)
    Output
                 path          states            sdds         horizon        fit.runs 
                  "."             "2"             "t"           "400"            "50" 
          fit.at_true     fit.accept1     fit.accept2 fit.print.level     fit.gradtol 
              "FALSE"             "1"             "2"             "0"         "1e-06" 
          fit.stepmax     fit.steptol     fit.iterlim           model            data 
                  "1"         "1e-06"           "200"           "hmm"              NA 
                  sim      fixed_dfs1      fixed_dfs2 
               "TRUE"              NA              NA 

