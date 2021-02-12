rm(list = ls()); cat("\f")

### Initialize code
source("init.R"); load_code()

### Download data
download_data()

### Display warnings when they occur
options(warn=1)

### Set controls for different models
base_control =  list(
    data_col      = c("Close","Close"),
    states        = c(3,2),
    sdds          = c("gamma","t"),
    time_horizon  = c(NA,"m"),
    truncate_data = c("2000-01-03",NA)
  )
ids = paste0("HHMM_3_2_",c("DAX_DBK","DAX_SAP","DAX_telekom","DAX_VW","SandP500_amazon","SandP500_americanairlines","SandP500_apple","SandP500_google","SandP500_waltdisney"))
data_sources = list(c("dax","dbk"),c("dax","sap"),c("dax","telekom"),c("dax","vw"),c("sandp500","amazon"),c("sandp500","americanairlines"),c("sandp500","apple"),c("sandp500","google"),c("sandp500","waltdisney"))
all_controls = list()
for(n in 1:length(ids)) {
  all_controls[[n]] = base_control
  all_controls[[n]]["id"] = ids[n]
  all_controls[[n]]["data_source"] = data_sources[n]
}

### Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### Fit
for(controls in all_controls){
  tryCatch(fit_hmm(controls,events), 
           error=function(e)message("Estimation failed:",conditionMessage(e),"\n"))
}

### Reset default warning setting
options(warn=0)
