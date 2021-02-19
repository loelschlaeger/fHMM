rm(list = ls()); cat("\f")

### Initialize code
source("load_code.R")

### Download data
download_data(path=".")

### Display warnings when they occur
options(warn=1)

### Set controls for different models
base_control =  list(
    path          = ".",
    data_col      = c("Close","Close"),
    time_horizon  = c(NA,"m"),
    truncate_data = c("2000-01-03",NA),
    runs          = 200
  )
data_sources = list(c("dax","dbk"),c("dax","sap"),c("dax","vw"),c("sandp500","americanairlines"),c("sandp500","waltdisney"))
all_controls = list()
for(n in 1:length(data_sources)){
  for(s in 1:2){
    for(d in 1:2){
      i = 4*(n-1)+2*(s-1)+d
      all_controls[[i]] = base_control
      all_controls[[i]][["states"]] = if(s==1) c(2,2) else c(3,2)
      all_controls[[i]][["sdds"]] = if(d==1) c("gamma","t") else c("gamma","t")
      all_controls[[i]][["data_source"]] = unlist(data_sources[n])
      all_controls[[i]][["id"]] = paste0("HHMM_",paste0(all_controls[[i]][["states"]],collapse="_"),"_",paste0(unlist(all_controls[[i]][["data_source"]]),collapse="_"))
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
  tryCatch(fit_hmm(controls,events), 
           error=function(e)message("Estimation failed: ",conditionMessage(e),"\n"))
}

### Reset default warning setting
options(warn=0)
