check_controls = function(controls){
  
  ### check supplied control values
  all_controls = c("id","data_source","data_col","truncate_data","states","time_horizon","sdds","runs","at_true","iterlim","seed","print_level","steptol","accept_codes","overwrite","data_cs_type")
  required_controls = c("id","states","sdds")
  artificial_controls = c("sim","model","fixed_dfs","controls_checked")
  missing_controls = setdiff(all_controls,names(controls))
  redundant_controls = setdiff(names(controls),c(all_controls,artificial_controls))
  controls_with_length_1 = c("id","runs","at_true","iterlim","seed","print_level","steptol","overwrite","data_cs_type")
  controls_with_length_2 = c("data_source","data_col","truncate_data","states","time_horizon","sdds")
  numeric_controls = c("states","runs","iterlim","print_level","steptol","seed")
  boolean_controls = c("overwrite","at_true")
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))) stop(paste0("Please specify '", required_control, "' in 'controls'."),call.=FALSE)
  }
  if(length(missing_controls)>=1) warning(paste("Some controls are not specified and set to default values."),call.=FALSE)
  if(length(redundant_controls)==1) warning(paste("The following element in 'controls' is not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  if(length(redundant_controls)>1) warning(paste("The following elements in 'controls' are not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  for(control_with_length_1 in intersect(controls_with_length_1,names(controls))){
    if(length(controls[[control_with_length_1]])!=1) stop(paste0("'", control_with_length_1, "' in 'controls' must be an atomic value."),call.=FALSE)
  }
  for(control_with_length_2 in intersect(controls_with_length_2,names(controls))){
    if(length(controls[[control_with_length_2]])!=2) stop(paste0("'", control_with_length_2, "' in 'controls' must be a vector of length 2."),call.=FALSE)
  }
  for(numeric_control in intersect(numeric_controls,names(controls))){
    if(length(controls[[numeric_control]])==1) if(!is.numeric(controls[[numeric_control]])) stop(paste0("The element '", numeric_control, "' in 'controls' must be numeric."),call.=FALSE)
    if(length(controls[[numeric_control]])>1) if(!isTRUE(all(controls[[numeric_control]] == floor(controls[[numeric_control]])))) stop(paste0("The element '", numeric_control, "' in 'controls' must be a numeric vector."),call.=FALSE)
  }
  for(boolean_control in intersect(boolean_controls,names(controls))){
    if(!is.logical(controls[[boolean_control]])) stop(paste0("'", boolean_control, "' in 'controls' must be a boolean."),call.=FALSE)
  }
  
  ### set default values
  if("data_source" %in% missing_controls) controls[["data_source"]] = c(NA,NA)
  if("data_col" %in% missing_controls) controls[["data_col"]] = c(NA,NA)
  if("data_cs_type" %in% missing_controls) controls[["data_cs_type"]] = NA
  if("truncate_data" %in% missing_controls) controls[["truncate_data"]] = c(NA,NA)
  if("time_horizon" %in% missing_controls) controls[["time_horizon"]] = c(NA,NA)
  if("runs" %in% missing_controls) controls[["runs"]] = 200
  if("at_true" %in% missing_controls) controls[["at_true"]] = FALSE
  if("iterlim" %in% missing_controls) controls[["iterlim"]] = 500
  if("print_level" %in% missing_controls) controls[["print_level"]] = 0
  if("steptol" %in% missing_controls) controls[["steptol"]] = 1e-6
  if("accept_codes" %in% missing_controls) controls[["accept_codes"]] = c(1)
  if("overwrite" %in% missing_controls) controls[["overwrite"]] = FALSE
  if("sdds" %in% missing_controls) controls[["sdds"]] = c(NA,NA)
  
  ### create artificial controls
  controls[["model"]] = if(controls[["states"]][2]==0) "HMM" else "HHMM"
  if(controls[["model"]]=="HMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][1])) TRUE else FALSE
  }
  if(controls[["model"]]=="HHMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][2])) TRUE else FALSE
    if(!controls[["sim"]] & is.na(controls[["data_source"]][1])){
      controls[["data_source"]][1] = controls[["data_source"]][2]
      controls[["data_col"]][1] = controls[["data_col"]][2]
    }
  }

  ### check sdds
  possible_sdds = c("t","normal","gamma")
  check_t_sdds = function(x) grepl("^[t][\\(]([0-9]+||[I][n][f])[\\)]$",x)
  controls[["sdds"]][which(controls[["sdds"]]=="normal")] = "t(Inf)"
  controls[["fixed_dfs"]] = c(NA,NA)
  if(check_t_sdds(controls[["sdds"]][1])) controls[["fixed_dfs"]][1] = sub("\\).*", "", sub(".*\\(", "", controls[["sdds"]][1])) 
  if(check_t_sdds(controls[["sdds"]][2])) controls[["fixed_dfs"]][2] = sub("\\).*", "", sub(".*\\(", "", controls[["sdds"]][2]))
  controls[["fixed_dfs"]] = as.numeric(controls[["fixed_dfs"]])
  if(controls[["model"]]=="HHMM"){
    if(controls[["data_cs_type"]] %in% c("mean_abs","sum_abs") & controls[["sdds"]][2] != "gamma"){
      warning(paste0("For CS data type '",controls[["data_cs_type"]],"' the gamma distribution is recommended."),call.=FALSE)
    }
    if(controls[["data_cs_type"]] %in% c("mean") & controls[["sdds"]][2] == "gamma"){
      warning(paste0("For CS data type '",controls[["data_cs_type"]],"' the gamma distribution is not recommended."),call.=FALSE)
    }
  }
  
  ### check if controls are correct
  if(controls[["model"]]=="HMM") {
    if(controls[["states"]][1]%%1!=0 || controls[["states"]][1]<2){
      stop("First entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["sim"]] & is.na(controls[["time_horizon"]][1])){
      stop("Either first entry of 'data_source' or 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & is.na(controls[["data_col"]][1])){
      stop("First entry of 'data_col' has to be specified.",call.=FALSE)
    }
    if(is.na(controls[["sdds"]][1])){
      stop("First entry of 'sdds' has to be specified.",call.=FALSE)
    }
    if(!is.na(controls[["sdds"]][1]) & !controls[["sdds"]][1] %in% possible_sdds & !check_t_sdds(controls[["sdds"]][1])){
      stop(paste0("First entry of 'sdds' must be one of '",paste(possible_sdds,collapse="', '"),"'."),call.=FALSE)
    }
    if(!is.na(controls[["sdds"]][2])){
      warning("Second entry of 'sdds' will be ignored.",call.=FALSE)
    }
    if(!is.na(controls[["data_cs_type"]])){
      warning("Element 'data_cs_type' will be ignored.",call.=FALSE)
      controls[["data_cs_type"]] = NA
    }
    if(!is.na(controls[["data_source"]][2])){
      warning("Second entry of 'data_source' will be ignored.",call.=FALSE)
      controls[["data_source"]][2] = NA
    }
    if(!controls[["sim"]] & !is.na(controls[["data_col"]][2])){
      warning("Second entry of 'data_col' will be ignored.",call.=FALSE)
      controls[["data_col"]][2] = NA
    }
    if(!is.na(controls[["time_horizon"]][2])){
      warning("Second entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][2] = NA
    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("First entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
  }
  if(controls[["model"]]=="HHMM") {
    if(controls[["states"]][1]%%1!=0 || controls[["states"]][1]<2){
      stop("First entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["states"]][2]%%1!=0 || controls[["states"]][2]<2){
      stop("Second entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["sim"]] & any(is.na(controls[["time_horizon"]]))){
      stop("Either 'data_source' or 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & is.na(controls[["time_horizon"]][2])){
      stop("Second entry of 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & any(is.na(controls[["data_col"]]))){
      stop("Entries of 'data_col' have to be specified.",call.=FALSE)
    }
    if(is.na(controls[["sdds"]][2])){
      stop("Second entry of 'sdds' has to be specified.",call.=FALSE)
    }
    if(!is.na(controls[["sdds"]][2])){
      if(!controls[["sdds"]][1] %in% possible_sdds & !check_t_sdds(controls[["sdds"]][1])){
        stop(paste0("First entry of 'sdds' must be one of '",paste(possible_sdds,collapse="', '"),"'."),call.=FALSE)
      }
      if(!controls[["sdds"]][2] %in% possible_sdds & !check_t_sdds(controls[["sdds"]][2])){
        stop(paste0("Second entry 'sdds' must be one of '",paste(possible_sdds,collapse="', '"),"'."),call.=FALSE)
      }
    }
    if(!is.na(controls[["data_cs_type"]]) & !controls[["data_cs_type"]] %in% c("mean","mean_abs","sum_abs")){
      stop(paste0("Element 'data_cs_type' must be one of '",paste(c("mean","mean_abs","sum_abs"),collapse="', '"),"'."),call.=FALSE)
    }
    if(!is.na(controls[["data_cs_type"]]) & controls[["sim"]]){
      warning("Element 'data_cs_type' will be ignored.",call.=FALSE)
      controls[["data_cs_type"]] = NA
    }
    if(!controls[["sim"]] & is.na(controls[["data_cs_type"]])){
      controls[["data_cs_type"]] = "mean_abs"
    }
    if(controls[["sim"]] & any(!is.na(controls[["data_source"]]))){
      warning("Entries of 'data_source' will be ignored.",call.=FALSE)
      controls[["data_source"]] = c(NA,NA)
    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("First entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
  }
  if(!controls[["sim"]] & controls[["at_true"]]){
    controls[["at_true"]] = FALSE
    warning("Value of 'at_true' is set to FALSE.",call.=FALSE)
  }
  if(controls[["sim"]] & controls[["at_true"]] & controls[["runs"]]!=1){
    controls[["runs"]] = 1
    warning("Value of 'runs' is set to 1.",call.=FALSE)
  }
  if(controls[["sim"]] & any(!is.na(controls[["truncate_data"]]))){
    warning("Entries of 'truncate_data' will be ignored.",call.=FALSE)
    controls[["truncate_data"]] = c(NA,NA)
  }
  if(controls[["sim"]] & any(!is.na(controls[["data_col"]]))){
    warning("Entries of 'data_col' will be ignored.",call.=FALSE)
    controls[["data_col"]] = c(NA,NA)
  }
  if(controls[["overwrite"]]){
    warning("Overwriting is allowed.",call.=FALSE)
  }
  if(controls[["accept_codes"]]=="all"){
    controls[["accept_codes"]] = 1:5
  }
  
  ### check if data paths are correct
  for(i in c(1,2)){
    if(!is.na(controls[["data_source"]][i])){
      controls[["data_source"]][i] = paste0(controls[["data_source"]][i],".csv")
      if(!file.exists(paste0("data/",controls[["data_source"]][i]))){
        stop(paste0("File 'data/",controls[["data_source"]][i],"' does not exist."),call.=FALSE)
      }
      if(!controls[["data_col"]][i] %in% colnames(read.csv(file=paste0("data/",controls[["data_source"]][i])))){
        stop(paste0("Column '",controls[["data_col"]][i],"' does not exist in the file 'data/",controls[["data_source"]][i],"'."),call.=FALSE)
      }
    }
  }

  ### note that 'controls' is checked
  writeLines("Checks successful.")
  controls[["controls_checked"]] = TRUE
  
  ### print model specification
  writeLines(paste0("ID:     ",controls[["id"]]))
  writeLines(paste0("Model:  ",controls[["model"]]))
  if(controls[["sim"]])  writeLines("Data:   simulated")
  if(!controls[["sim"]]) writeLines("Data:   empirical")
  if(controls[["model"]]=="HMM") {
    writeLines(paste0("States: ",controls[["states"]][1]))
    writeLines(paste0("SDDs:   ",ifelse(controls[["sdds"]][1]=="t(Inf)","normal",controls[["sdds"]][1])))
  } 
  if(controls[["model"]]=="HHMM") {
    writeLines(paste0("States: ",controls[["states"]][1]," / ",controls[["states"]][2]))
    writeLines(paste0("SDDs:   ",ifelse(controls[["sdds"]][1]=="t(Inf)","normal",controls[["sdds"]][1])," / ",ifelse(controls[["sdds"]][2]=="t(Inf)","normal",controls[["sdds"]][2])))
  }
  writeLines(paste0("Runs:   ",controls[["runs"]],ifelse(controls[["at_true"]]," (initialised at true values)","")))
  if(!is.null(controls[["seed"]])) writeLines(paste0("Seed:   ",controls[["seed"]]))
  
  ### save controls
  check_saving(controls,controls)
  
  return(controls)
}

check_data = function(controls,data){
  if(controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(paste0("Data points: ",controls[["time_horizon"]][1]))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(paste0("Data points:  ",controls[["time_horizon"]][1]," / ",controls[["time_horizon"]][2]))
    }
  }
  if(!controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(paste0("Source:      ",controls[["data_source"]][1]))
      writeLines(paste0("Column:      ",controls[["data_col"]][1]))
      writeLines(paste0("Horizon:     ",data[["dates"]][1], " to ", rev(data[["dates"]])[1]))
      writeLines(paste0("Data points: ",length(data[["logReturns"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(paste0("Source:       ",controls[["data_source"]][1]," / ",controls[["data_source"]][2]))
      writeLines(paste0("Column:       ",controls[["data_col"]][1]," / ",controls[["data_col"]][2]))
      writeLines(paste0("CS data type: ",controls[["data_cs_type"]]))
      writeLines(paste0("Horizon:      ", data[["dates"]][1], " to ", rev(data[["dates"]])[1]))
      writeLines(paste0("FS dim:       ",controls[["time_horizon"]][2]))
      writeLines(paste0("Data points:  ",dim(data[["logReturns"]])[1]," / ",dim(data[["logReturns"]])[2]-1))
    }
  }
  
  ### save data
  check_saving(data,controls)
}

check_estimation = function(time,mods,llks,data,hessian,controls){
  writeLines(paste0("Estimation finished, it took ",time," minute(s).",ifelse(controls[["at_true"]],"",paste0("Successful runs: ",sum(!is.na(llks))," out of ",length(llks),"."))))

  ### select model with highest LL
  mod       = mods[[which.max(llks)]]
  mod_LL    = -mod[["minimum"]]
  thetaCon  = thetaUncon2thetaCon(mod[["estimate"]],controls)
  thetaList = states_decreasing(thetaCon2thetaList(thetaCon,controls),controls)
  
  ### detect unidentified states
  check_unid_states = function(Gamma,name=NULL){
    for(x in c(0,1)){
      unid_states = abs(Gamma2delta(Gamma)-x)<1e-04
      for(s in which(unid_states)){
        warning(paste(ifelse(is.null(name),"State",paste(name,"state")),s,"might be unidentified."),call.=FALSE)
      }
    }
  }
  if(controls[["model"]]=="HMM"){
    check_unid_states(thetaList[["Gamma"]])
  }
  if(controls[["model"]]=="HHMM"){
    check_unid_states(thetaList[["Gamma"]],"Coarse scale")
    for(cs in controls[["states"]]){
      check_unid_states(thetaList[["Gammas_star"]][[cs]],paste("On coarse-scale state",cs,"fine scale"))
    }
  }
  
  ### create visualization of LLs
  if(!controls[["at_true"]]) plot_ll(llks,controls)
  
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["iterlim"]]) warning("Selected estimation run reached the iteration limit. Consider increasing 'iterlim'.",call.=FALSE)
  exceeded_runs = unlist(lapply(mods,function (x) x[["iterations"]])) >= controls[["iterlim"]] 
  if(any(exceeded_runs)) warning(paste0(sum(exceeded_runs)," of ",length(llks)," runs reached the iteration limit. Consider increasing 'iterlim'."),call.=FALSE)
  
  ### compute model selection criteria
  no_par   = length(mod[["estimate"]])
  comp_AIC = function(LL) return(2*no_par-2*LL)
  comp_BIC = function(T,LL) return(log(T)*no_par-2*LL)
  
  ### create estimation output
  fit = list("LL"        = mod_LL,
             "mod"       = mod,
             "thetaList" = thetaList,
             "AIC"       = comp_AIC(-mod[["minimum"]]),
             "BIC"       = comp_BIC(prod(dim(t(data[["logReturns"]]))),-mod[["minimum"]]),
             "all_LL"    = llks,
             "all_mods"  = mods,
             "hessian"   = hessian
             )
  file = paste0("models/",controls[["id"]],"/estimates.txt")
  if(file.exists(file) & !controls[["overwrite"]]){ 
    warning(paste0("Cannot save 'estimates.txt' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
  } else {
    options(max.print=1000000)
    sink(file=file)
    first_col = c("LL","AIC","BIC","exit code","iterations", "run time (min)")
    second_col = c(fit[["LL"]],fit[["AIC"]],fit[["BIC"]],mod[["code"]],mod[["iterations"]],time)
    df = data.frame(first_col,second_col); names(df) = NULL
    writeLines(paste0("ESTIMATION RESULTS of model '",controls[["id"]],"':")); print(df,row.names=FALSE,right=FALSE); writeLines("")
    if(controls[["sim"]]){
      writeLines("TRUE parameter values:\n"); print(data[["thetaList0"]])
    }
    writeLines("ESTIMATES:\n"); print(fit[["thetaList"]])
    writeLines("GRADIENT:\n"); print(mod[["gradient"]]); writeLines("")
    writeLines("HESSIAN:\n"); print(hessian)
    sink()
    options(max.print=1000)
  }
  
  ### save results
  check_saving(fit,controls)
  
  return(fit)
}

check_decoding = function(decoding,controls){
  
  ### create states summary output
  file = paste0("models/",controls[["id"]],"/states.txt")
  if(file.exists(file) & !controls[["overwrite"]]){ 
    warning(paste0("Cannot save 'states.txt' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
  } else {
    sink(file=file)
    writeLines("FREQUENCY of decoded states:\n")
    if(controls[["model"]]=="HMM"){
      out = table((decoding)); names(out) = paste("state",names(out))
      print(out)
      writeLines("")
    }
    if(controls[["model"]]=="HHMM"){
      out_cs = table(factor(decoding[,1],levels = seq_len(controls[["states"]][1]))); names(out_cs) = paste("CS state",names(out_cs))
      print(out_cs); writeLines("")
      for(state in seq_len(controls[["states"]][1])){
        writeLines(paste0("Conditional on CS state ",state,":"))
        out_fs = table(factor(decoding[decoding[,1]==state,-1],levels = seq_len(controls[["states"]][2])))
        names(out_fs) = paste("FS state",names(out_fs))
        print(out_fs)
        writeLines("")
      }
    }
    if(controls[["sim"]]){
      compare_true_predicted_states = function(no_states,decoded_states,true_states,label=NULL){
        c_table = matrix(0,no_states,no_states)
        rownames(c_table) = paste0("true ",label,"state ",seq_len(no_states))
        colnames(c_table) = paste0("decoded ",label,"state ",seq_len(no_states))
        for(i in seq_len(no_states)){
          for(j in seq_len(no_states)){
            c_table[i,j] = sum(decoded_states==i & true_states==j)
          }
        }
        return(c_table)
      }
      writeLines("COMPARISON between true states and predicted states:\n")
      if(controls[["model"]]=="HMM"){
        print(compare_true_predicted_states(controls[["states"]][1],decoding,data[["states0"]]))
      }
      if(controls[["model"]]=="HHMM"){
        print(compare_true_predicted_states(controls[["states"]][1],decoding[,1],data[["states0"]][,1],label="CS "))
        writeLines("")
        for(cs_state in seq_len(controls[["states"]][1])){
          writeLines(paste0("Conditional on decoded CS state ",cs_state,":"))
          print(compare_true_predicted_states(controls[["states"]][2],decoding[decoding[,1]==cs_state,-1],data[["states0"]][decoding[,1]==cs_state,-1],label="FS "))
          writeLines("")
          writeLines(paste0("Conditional on true CS state ",cs_state,":"))
          print(compare_true_predicted_states(controls[["states"]][2],decoding[data[["states0"]][,1]==cs_state,-1],data[["states0"]][data[["states0"]][,1]==cs_state,-1],label="FS "))
          writeLines("")
        }
      }
    }
    sink()
  }
  
  ### save decoding
  check_saving(decoding,controls)
  
  writeLines("State decoding successful.")
}

check_saving = function(object,controls){
  overwrite = controls[["overwrite"]]
  path = paste0("models/",controls[["id"]])
  name = deparse(substitute(object))
  filename = paste0(path,"/",name)
  if(!dir.exists(path)){
    dir.create(path)
  }
  if(file.exists(filename)){
    if(overwrite){
      saveRDS(object,file=filename)
    } else { 
      warning(paste0("Cannot save '",name,"' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
      }
  } else {
    saveRDS(object,file=filename)
    }
}
