rm(list = ls())

### 1. Initialization
source("init.R"); init()

### 2. Display warnings when they occur
options(warn=1)

### 3. Set controls for different models
all_controls = list(
  # list(
  #   id            = "HHMM_3_2_sim",
  #   time_horizon  = c(300,30),
  #   states        = c(3,2)
  #   ),
  list(
    id            = "HHMM_2_2_DAX_DB",
    data_source   = c("dax.csv","deutschebank.csv"),
    data_col      = c("Close","Close"),
    truncate_data = c("2000-01-03",NA),
    states        = c(2,2),
    time_horizon  = c(NA,30)
    )
  # list(
  #   id            = "HHMM_2_2_SandP500",
  #   data_source   = c(NA,"sandp500.csv"),
  #   data_col      = c(NA,"Close"),
  #   truncate_data = c("2000-01-03",NA),
  #   states        = c(2,2),
  #   time_horizon  = c(NA,30)
  #   )
  )

### 4. Pre-check controls
for(control_no in seq_len(length(all_controls))){
  tryCatch({
    all_controls[[control_no]] = check_controls(all_controls[[control_no]])
  }, error=function(e){cat("Check failed:",conditionMessage(e),"\n")}) 
}

### 5. Fit, decode and visualize models
for(controls in all_controls){
  tryCatch({
    data = getData(controls)
    fit = maxLikelihood(data,controls)
    decoding = applyViterbi(data,fit,controls)
    labels = list(
      dates = c("2001-09-11","2008-09-15","2020-01-27"),
      names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
      )
    visual(data,fit,decoding,controls,labels)
  }, error=function(e){cat("Estimation failed:",conditionMessage(e),"\n")})
}

### 6. Reset default warning setting
options(warn=0)