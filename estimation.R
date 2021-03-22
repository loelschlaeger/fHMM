rm(list = ls()); cat("\f")

### initialize code
source("load_code.R")

### download data (optional)
download_data("dax","^GDAXI",path=".")
download_data("dbk","DBK.DE",path=".")
download_data("sap","sap.de",path=".")
download_data("vw","VOW3.DE",path=".")
download_data("sandp500","^GSPC",path=".")
download_data("americanairlines","AAL",path=".")

### define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### set controls
controls1 = list(
  path    = ".",
  id      = "HHMM_3_2_DAX_DBK_t_t",
  states  = c(3,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","dbk"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls2 = list(
  path    = ".",
  id      = "HHMM_2_2_DAX_DBK_t_t",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","dbk"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls3 = list(
  path    = ".",
  id      = "HHMM_3_2_DAX_SAP_t_t",
  states  = c(3,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","sap"), 
                 "column"       = c("Close","Close"),
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls4 = list(
  path    = ".",
  id      = "HHMM_2_2_DAX_SAP_t_t",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","sap"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls5 = list(
  path    = ".",
  id      = "HHMM_3_2_DAX_VW_t_t",
  states  = c(3,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","vw"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls6 = list(
  path    = ".",
  id      = "HHMM_2_2_DAX_VW_t_t",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("dax","vw"), 
                 "column"       = c("Close","Close"),
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls7 = list(
  path    = ".",
  id      = "HHMM_3_2_SANDP500_AA_t_t",
  states  = c(3,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("sandp500","americanairlines"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)
controls8 = list(
  path    = ".",
  id      = "HHMM_2_2_SANDP500_AA_t_t",
  states  = c(2,2),
  sdds    = c("t","t"),
  horizon = c(NA,"m"),
  data    = list("source"       = c("sandp500","americanairlines"), 
                 "column"       = c("Close","Close"), 
                 "truncate"     = c("2000-01-01",NA),
                 "cs_transform" = "(tail(x,1)-head(x,1))/head(x,1)", 
                 "log_returns"  = c(FALSE,TRUE)),
  fit     = list("runs" = 200)
)

all_controls = list(controls1,controls2,controls3,controls4,controls5,controls6,controls7,controls8)

### fit (H)HMMs
for(controls in all_controls) fit_hmm(controls,events)
