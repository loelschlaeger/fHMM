#' Check specification of controls
#'
#' @param controls A list of controls
#' 
#' @return Checked \code{controls} 
#' 
#' @examples 
#' controls = list(
#'  id           = "HMM_3_t",
#'  sdds         = c("t",NA),
#'  states       = c(3,0),
#'  time_horizon = c(500,NA),
#'  seed         = 1
#' )
#' check_controls(controls)

check_controls = function(controls){
  
  ### define types of controls
  all_controls = c("id","scale_par","data_source","data_col","truncate_data","states","time_horizon","sdds","runs","at_true","iterlim","seed","print_level","steptol","gradtol","accept_codes","overwrite","data_cs_type")
  required_controls = c("id","states","sdds")
  artificial_controls = c("sim","model","fixed_dfs","controls_checked")
  missing_controls = setdiff(all_controls,names(controls))
  missing_required_controls = setdiff(required_controls,names(controls))
  redundant_controls = setdiff(names(controls),c(all_controls,artificial_controls))
  controls_with_length_1 = c("id","runs","at_true","iterlim","seed","print_level","steptol","gradtol","overwrite","data_cs_type")
  controls_with_length_2 = c("data_source","data_col","truncate_data","states","time_horizon","sdds")
  positive_numeric_controls = c("runs","iterlim","steptol","gradtol")
  integer_controls = c("states","runs","iterlim","print_level","seed")
  boolean_controls = c("overwrite","at_true")
  
  ### check types of controls
  if(length(missing_required_controls)>0){
    stop(sprintf("%s (%s)",exception("C.2")[2],exception("C.2")[1]),call.=FALSE) 
  }
  if(any(sapply(controls[intersect(controls_with_length_1,names(controls))],length)!=1)){
    stop(sprintf("%s (%s)",exception("C.3")[2],exception("C.3")[1]),call.=FALSE) 
  }
  if(any(sapply(controls[intersect(controls_with_length_2,names(controls))],length)!=2)){
    stop(sprintf("%s (%s)",exception("C.3")[2],exception("C.3")[1]),call.=FALSE) 
  }
  if(any(sapply(controls[intersect(positive_numeric_controls,names(controls))],function(x) !is.numeric(x) || x<=0))){
    stop(sprintf("%s (%s)",exception("C.3")[2],exception("C.3")[1]),call.=FALSE) 
  }
  if(any(sapply(controls[intersect(boolean_controls,names(controls))],function(x) !is.logical(x)))){
    stop(sprintf("%s (%s)",exception("C.3")[2],exception("C.3")[1]),call.=FALSE) 
  }
  if(length(missing_controls)>=1){
    warning(sprintf("%s (%s)",exception("C.4")[2],exception("C.4")[1]),call.=FALSE)
  }
  if(length(redundant_controls)>=1){
    warning(sprintf("%s (%s)",exception("C.5")[2],exception("C.5")[1]),call.=FALSE)
  }
  
  ### set default control values
  if("accept_codes" %in% missing_controls)  controls[["accept_codes"]] = c(1,2)
  if("at_true" %in% missing_controls)       controls[["at_true"]] = FALSE
  if("data_col" %in% missing_controls)      controls[["data_col"]] = c(NA,NA)
  if("data_cs_type" %in% missing_controls)  controls[["data_cs_type"]] = NA
  if("data_source" %in% missing_controls)   controls[["data_source"]] = c(NA,NA)
  if("gradtol" %in% missing_controls)       controls[["gradtol"]] = 1e-4
  if("iterlim" %in% missing_controls)       controls[["iterlim"]] = 500
  if("truncate_data" %in% missing_controls) controls[["truncate_data"]] = c(NA,NA)
  if("time_horizon" %in% missing_controls)  controls[["time_horizon"]] = c(NA,NA)
  if("overwrite" %in% missing_controls)     controls[["overwrite"]] = FALSE
  if("print_level" %in% missing_controls)   controls[["print_level"]] = 0
  if("runs" %in% missing_controls)          controls[["runs"]] = 100
  if("steptol" %in% missing_controls)       controls[["steptol"]] = 1e-2
  if("truncate_data" %in% missing_controls) controls[["truncate_data"]] = c(NA,NA)
  if("time_horizon" %in% missing_controls)  controls[["time_horizon"]] = c(NA,NA)
  if("sdds" %in% missing_controls)          controls[["sdds"]] = c(NA,NA)
  
  ### create artificial controls
  controls[["model"]] = if(controls[["states"]][2]==0) "HMM" else "HHMM"
  if(controls[["model"]]=="HMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][1])) TRUE else FALSE
  }
  if(controls[["model"]]=="HHMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][2])) TRUE else FALSE
  }
  controls[["fixed_dfs"]] = c(NA,NA)
  controls[["scale_par"]] = c(1,1)
  
  ### check sdds
  all_sdds = c("gamma","t")
  extract_dfs = function(x){
    return(grepl("^[t][\\(]([1-9][0-9]*|Inf)[\\)]$",x))
  }
  for(i in 1:2){
    if(extract_dfs(controls[["sdds"]][i])){
      controls[["fixed_dfs"]][i] = as.numeric(sub("\\).*", "", sub(".*\\(", "", controls[["sdds"]][i])))
      controls[["sdds"]][i] = "t"
    }
  }
  if(controls[["model"]]=="HMM") controls[["fixed_dfs"]][2] = NA
  
  ### check if controls are correct
  if(controls[["model"]]=="HMM") {
    if(controls[["states"]][1]%%1!=0 || controls[["states"]][1]<2){
      stop("In 'controls': First entry of 'states' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["sim"]] & is.na(controls[["time_horizon"]][1])){
      stop("In 'controls': Either first entry of 'data_source' or 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & is.na(controls[["data_col"]][1])){
      stop("In 'controls': First entry of 'data_col' has to be specified.",call.=FALSE)
    }
    if(is.na(controls[["sdds"]][1])){
      stop("In 'controls': First entry of 'sdds' has to be specified.",call.=FALSE)
    }
    if(!controls[["sdds"]][1] %in% all_sdds){
      stop(paste0("In 'controls': First entry of 'sdds' must be 't' or 'gamma'."),call.=FALSE)
    }
    if(!is.na(controls[["sdds"]][2])){
      warning("In 'controls': Second entry of 'sdds' is ignored.",call.=FALSE)
      controls[["sdds"]][2] = NA
    }
    if(!is.na(controls[["data_cs_type"]])){
      warning("In 'controls': 'data_cs_type' is ignored.",call.=FALSE)
      controls[["data_cs_type"]] = NA
    }
    if(!is.na(controls[["data_source"]][2])){
      warning("In 'controls': Second entry of 'data_source' is ignored.",call.=FALSE)
      controls[["data_source"]][2] = NA
    }
    if(!controls[["sim"]] & !is.na(controls[["data_col"]][2])){
      warning("In 'controls': Second entry of 'data_col' is ignored.",call.=FALSE)
      controls[["data_col"]][2] = NA
    }
    if(!is.na(controls[["time_horizon"]][2])){
      warning("In 'controls': Second entry of 'time_horizon' is ignored.",call.=FALSE)
      controls[["time_horizon"]][2] = NA    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("In 'controls': First entry pf 'time_horizon' is ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
  }
  if(controls[["model"]]=="HHMM") {
    if(any(controls[["states"]]%%1!=0) || any(controls[["states"]]<2)){
      stop("In 'controls': Both entries of 'states' must be integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["sim"]] & any(is.na(controls[["time_horizon"]]))){
      stop("In 'controls': Either 'data_source' or 'time_horizon' has to be fully specified.",call.=FALSE)
    }
    if(is.na(controls[["time_horizon"]][2])){
      stop("In 'controls': Second entry of 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!is.numeric(controls[["time_horizon"]][2]) & !controls[["time_horizon"]][2] %in% c("w","m","q","y")){
      stop("In 'controls': Second entry of 'time_horizon' has to be an integer or one of 'w', 'm', 'q', 'y'.",call.=FALSE)
    }
    if(!controls[["sim"]] & any(is.na(controls[["data_col"]]))){
      stop("In 'controls': Entries of 'data_col' have to be specified.",call.=FALSE)
    }
    if(is.na(controls[["sdds"]][2])){
      stop("In 'controls': Second entry of 'sdds' has to be specified.",call.=FALSE)
    } else {
      if(!controls[["sdds"]][1] %in% all_sdds){
        stop(paste0("In 'controls': First entry of 'sdds' must be 't' or 'gamma'."),call.=FALSE)
      }
      if(!controls[["sdds"]][2] %in% all_sdds){
        stop(paste0("In 'controls': Second entry of 'sdds' must be 't' or 'gamma'."),call.=FALSE)
      }
    }
    if(!is.na(controls[["data_cs_type"]]) & !controls[["data_cs_type"]] %in% c("mean","mean_abs","sum_abs")){
      stop(paste0("In 'controls': 'data_cs_type' must be one of '",paste(c("mean","mean_abs","sum_abs"),collapse="', '"),"'."),call.=FALSE)
    }
    if(!is.na(controls[["data_cs_type"]]) & controls[["sim"]]){
      warning("In 'controls': 'data_cs_type' is ignored.",call.=FALSE)
      controls[["data_cs_type"]] = NA
    }
    if(!controls[["sim"]] & is.na(controls[["data_cs_type"]])){
      controls[["data_cs_type"]] = "mean_abs"
    }
    if(controls[["sim"]] & any(!is.na(controls[["data_source"]]))){
      warning("In 'controls': Entries of 'data_source' are ignored.",call.=FALSE)
      controls[["data_source"]] = c(NA,NA)
    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("In 'controls': First entry of 'time_horizon' is ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
  }
  if(!controls[["sim"]] & controls[["at_true"]]){
    warning("In 'controls': Value of 'at_true' is set to FALSE.",call.=FALSE)
    controls[["at_true"]] = FALSE
  }
  if(controls[["sim"]] & controls[["at_true"]] & controls[["runs"]]!=1){
    controls[["runs"]] = 1
    warning("In 'controls': Value of 'runs' is set to 1.",call.=FALSE)
  }
  if(controls[["sim"]] & any(!is.na(controls[["truncate_data"]]))){
    warning("In 'controls': Entries of 'truncate_data' are ignored.",call.=FALSE)
    controls[["truncate_data"]] = c(NA,NA)
  }
  if(controls[["sim"]] & any(!is.na(controls[["data_col"]]))){
    warning("In 'controls': Entries of 'data_col' are ignored.",call.=FALSE)
    controls[["data_col"]] = c(NA,NA)
  }
  if(any(controls[["accept_codes"]]=="all")){
    controls[["accept_codes"]] = 1:5
  }
  if(controls[["id"]]=="test"){
    controls[["overwrite"]] = TRUE
  }
  
  ### check if data paths are correct
  for(i in c(1,2)){
    if(!is.na(controls[["data_source"]][i])){
      if(!grepl(".csv$",controls[["data_source"]][i])) controls[["data_source"]][i] = paste0(controls[["data_source"]][i],".csv")
      if(!file.exists(paste0("data/",controls[["data_source"]][i]))){
        stop(paste0("File 'data/",controls[["data_source"]][i],"' does not exist."),call.=FALSE)
      }
      if(!controls[["data_col"]][i] %in% colnames(read.csv(file=paste0("data/",controls[["data_source"]][i])))){
        stop(paste0("Column '",controls[["data_col"]][i],"' does not exist in the file 'data/",controls[["data_source"]][i],"'."),call.=FALSE)
      }
    }
  }
  
  ### end of checks
  message("Controls checked.")
  controls[["controls_checked"]] = TRUE
  
  ### print model specification
  writeLines(sprintf("%8-s%s","ID:",controls[["id"]]))
  writeLines(sprintf("%8-s%s","Model:",controls[["model"]]))
  if(controls[["sim"]])  writeLines(sprintf("%8-s%s","Data:","simulated"))
  if(!controls[["sim"]]) writeLines(sprintf("%8-s%s","Data:","empirical"))
  if(controls[["model"]]=="HMM") {
    writeLines(sprintf("%8-s%s","States:",controls[["states"]][1]))
    writeLines(sprintf("%8-s%s","SDDs:",paste0(controls[["sdds"]][1],ifelse(!is.na(controls[["fixed_dfs"]][1]),paste0("(",controls[["fixed_dfs"]][1],")"),""))))
  }
  if(controls[["model"]]=="HHMM") {
    writeLines(sprintf("%8-s%s / %s","States:",controls[["states"]][1],controls[["states"]][2]))
    writeLines(sprintf("%8-s%s / %s","SDDs:",paste0(controls[["sdds"]][1],ifelse(!is.na(controls[["fixed_dfs"]][1]),paste0("(",controls[["fixed_dfs"]][1],")"),"")),paste0(controls[["sdds"]][2],ifelse(!is.na(controls[["fixed_dfs"]][2]),paste0("(",controls[["fixed_dfs"]][2],")"),""))))
  }
  writeLines(sprintf("%8-s%s %s","Runs:",controls[["runs"]],ifelse(controls[["at_true"]],"(initialised at true values)","")))
  if(!is.null(controls[["seed"]])) writeLines(sprintf("%8-s%s","Seed:",controls[["seed"]]))
  
  ### save controls
  check_saving(object   = controls, 
               filetype = "rds",
               controls = controls)
  
  return(controls)
}