rm(list = ls()); cat("\f")

### Initialize code
source("load_code.R")

### Download data
download_data(path=".")

### Display warnings when they occur
options(warn=1)

### Specify controls
all_controls = list()
for(data_source in list(c("dax","dax"),c("sandp500","sandp500"),c("dax","dbk"),c("dax","sap"),c("dax","vw"),c("sandp500","americanairlines"),c("sandp500","waltdisney"))){
  for(states in list(c(2,2),c(3,2))){
    for(sdds in list(c("gamma","t"),c("t","t"))){
      all_controls[[length(all_controls)+1]] = list(
        path          = ".",
        id            = paste0("HHMM_",paste0(states,collapse="_"),"_",paste0(toupper(data_source),collapse="_"),"_",paste0(sdds,collapse="_")),
        data_source   = data_source,
        data_col      = c("Close","Close"),
        data_cs_type  = ifelse(sdds[1]=="gamma","mean_abs","mean"),
        truncate_data = c("2000-01-03",NA),
        states        = states,
        sdds          = sdds,
        time_horizon  = c(NA,"m"),
        runs          = 200,
        seed          = 1
      )
    }
  }
}

### Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### Fit
for(controls in all_controls){
  tryCatch(fit_hmm(controls,events),error=function(e)message("Estimation failed: ",conditionMessage(e),"\n"))
}

### Reset default warning setting
options(warn=0)
