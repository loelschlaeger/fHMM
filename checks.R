### check supplied controls
check_controls = function(controls){
  
  ### define controls
  all_controls = c("id","scale_par","data_source","data_col","truncate_data","states","time_horizon","sdds","runs","at_true","iterlim","seed","print_level","steptol","gradtol","accept_codes","overwrite","data_cs_type")
  required_controls = c("id","states","sdds")
  artificial_controls = c("sim","model","fixed_dfs","controls_checked")
  missing_controls = setdiff(all_controls,names(controls))
  redundant_controls = setdiff(names(controls),c(all_controls,artificial_controls))
  controls_with_length_1 = c("id","runs","at_true","iterlim","seed","print_level","steptol","gradtol","overwrite","data_cs_type")
  controls_with_length_2 = c("data_source","data_col","truncate_data","states","time_horizon","sdds")
  positive_numeric_controls = c("runs","iterlim","steptol","gradtol")
  integer_controls = c("states","runs","iterlim","print_level","seed")
  boolean_controls = c("overwrite","at_true")
  
  ### check subsets of controls
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))){
      stop(paste0("In 'controls': '", required_control,"' must be specified."),call.=FALSE)
    }
  }
  for(control_with_length_1 in intersect(controls_with_length_1,names(controls))){
    if(length(controls[[control_with_length_1]])!=1){
      stop(paste0("In 'controls': '", control_with_length_1, "' must be atomic."),call.=FALSE)
    }
  }
  for(control_with_length_2 in intersect(controls_with_length_2,names(controls))){
    if(length(controls[[control_with_length_2]])!=2){
      stop(paste0("In 'controls': '", control_with_length_2, "' must be a vector of length 2."),call.=FALSE)
    }
  }
  for(positive_numeric_control in intersect(positive_numeric_controls,names(controls))){
    if(!is.numeric(controls[[positive_numeric_control]]) || !all(controls[[positive_numeric_control]]>0,na.rm=TRUE)){
      stop(paste0("In 'controls': '", positive_numeric_control,"' must be a positive numeric value."),call.=FALSE)
    }
  }
  for(integer_control in intersect(integer_controls,names(controls))){
    if(!is.numeric(controls[[integer_control]]) || !isTRUE(all(controls[[integer_control]] == floor(controls[[integer_control]])))){
      stop(paste0("In 'controls': '", integer_control,"' must be an integer."),call.=FALSE)
    }
  }
  for(boolean_control in intersect(boolean_controls,names(controls))){
    if(!is.logical(controls[[boolean_control]])){
      stop(paste0("In 'controls': '", boolean_control, "' must be boolean."),call.=FALSE)
    }
  }
  
  ### set default values
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

  ### general checks
  if(length(missing_controls)>=1){
    warning("Some controls are not specified and set to default values.",call.=FALSE)
  }
  if(length(redundant_controls)>=1){
    warning(paste("In 'controls': The following elements are not supported:",paste(redundant_controls,collapse=", ")),call.=FALSE)
  }
  if(controls[["id"]]=="test"){
    controls[["overwrite"]] = TRUE
  }
  if(controls[["overwrite"]]){
    warning("Overwriting is allowed.",call.=FALSE)
  }
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

### print data characteristics
check_data = function(controls,data){
  if(controls[["model"]]=="HMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["logReturns"]]<0)){
      stop("Gamma distribution not allowed.",call.=FALSE)
    }
  }
  if(controls[["model"]]=="HHMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["logReturns"]][,1]<0)){
      stop("...",call.=FALSE)
    }
    if(controls[["sdds"]][2]=="gamma" & any(data[["logReturns"]][,-1]<0)){
      stop("...",call.=FALSE)
    }
  }
  if(controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(paste("Data points:",length(data[["logReturns"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(paste("Data points:",dim(data[["logReturns"]])[1],"/",length(data[["logReturns"]][,-1][!is.na(data[["logReturns"]][,-1])])))
    }
  } 
  if(!controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(sprintf("%13-s%s","Source:",controls[["data_source"]][1]))
      writeLines(sprintf("%13-s%s","Column:",controls[["data_col"]][1]))
      writeLines(sprintf("%13-s%s to %s","Horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%13-s%s","Data points:",length(data[["logReturns"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(sprintf("%14-s%s / %s","Source:",controls[["data_source"]][1],controls[["data_source"]][2]))
      writeLines(sprintf("%14-s%s / %s","Column:",controls[["data_col"]][1],controls[["data_col"]][2]))
      writeLines(sprintf("%14-s%s to %s","Horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%14-s%s / %s","Data points:",dim(data[["logReturns"]])[1],length(data[["logReturns"]][,-1][!is.na(data[["logReturns"]][,-1])])))
      writeLines(sprintf("%14-s%s","CS data type:",controls[["data_cs_type"]]))
      if(is.numeric(controls[["time_horizon"]][2])) writeLines(sprintf("%14-s%s","FS dim:",controls[["time_horizon"]][2]))
      if(controls[["time_horizon"]][2]=="w") writeLines(sprintf("%14-s%s","FS dim:","weekly"))
      if(controls[["time_horizon"]][2]=="m") writeLines(sprintf("%14-s%s","FS dim:","monthly"))
      if(controls[["time_horizon"]][2]=="q") writeLines(sprintf("%14-s%s","FS dim:","quarterly"))
      if(controls[["time_horizon"]][2]=="y") writeLines(sprintf("%14-s%s","FS dim:","yearly"))
    }
  }
  check_saving(object   = data,
               filetype = "rds",
               controls = controls)
}

### check estimated model
check_estimation = function(time,mods,llks,data,hessian,controls){
  message("Estimation finished.")
  writeLines(paste("Computation time:",time,"minute(s)."))
  if(!controls[["at_true"]]){
    writeLines(paste0("Accepted runs: ",sum(!is.na(llks))," out of ",length(llks),"."))
  }
  
  ### select run with highest LL
  mod       = mods[[which.max(llks)]]
  mod_LL    = -mod[["minimum"]]
  thetaCon  = thetaUncon2thetaCon(mod[["estimate"]],controls)
  thetaList = states_decreasing(thetaCon2thetaList(thetaCon,controls),controls)
  
  ### detect unidentified states
  check_unid_states = function(matrix_list){
    flag = FALSE
    for(matrix in matrix_list){
      for(x in c(0,1)){
        unid_states = abs(suppressWarnings(Gamma2delta(matrix))-x)<1e-04
        if(any(unid_states)==TRUE) flag = TRUE
      }
    }
    if(flag) warning("Possibly unidentified states.",call.=FALSE)
  }
  if(controls[["model"]]=="HMM"){
    check_unid_states(list(thetaList[["Gamma"]]))
  }
  if(controls[["model"]]=="HHMM"){
    check_unid_states(list(thetaList[["Gamma"]],thetaList[["Gammas_star"]][seq_len(controls[["states"]][1])]))
  }
  
  ### create visualization of LLs
  plot_ll(llks,controls)
  
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["iterlim"]]) warning("Selected estimation run reached the iteration limit. Consider increasing 'iterlim'.",call.=FALSE)
  exceeded_runs = unlist(lapply(mods,function (x) x[["iterations"]])) >= controls[["iterlim"]] 
  if(any(exceeded_runs) & controls[["runs"]]>1) warning(paste0(sum(exceeded_runs)," of ",length(llks)," runs reached the iteration limit. Consider increasing 'iterlim'."),call.=FALSE)
  
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
  if(check_saving(name     = "estimates",
                  filetype = "txt",
                  controls = controls)){  
    options(max.print=1000000)
      sink(file = paste0("models/",controls[["id"]],"/estimates.txt"))
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
  
  ### create comparison in case of simulated data
  if(controls[["sim"]]){
    if(check_saving(name     = "comparison",
                    filetype = "txt",
                    controls = controls)){
      sink(file = paste0("models/",controls[["id"]],"/comparison.txt"))
        true = unlist(data$thetaList0,use.names=FALSE)
        est = unlist(fit$thetaList,use.names=FALSE)
        rbias = ((est-true)/true)*100
        compare = cbind(as.numeric(sprintf("%.4f",true)),
                        as.numeric(sprintf("%.4f",est)),
                        as.numeric(sprintf("%.2f",rbias)))
        rownames(compare) = names(unlist(data$thetaList))
        colnames(compare) = c("true","estimate","rel. bias [%]")
        print(compare)
      sink()
    }
  }
  
  ### save results
  check_saving(object   = fit,
               filetype = "rds",
               controls = controls)
  
  return(fit)
}

### create output of decoding characteristics
check_decoding = function(decoding,data,controls){
  ### create states summary output
  if(check_saving(name     = "states",
                  filetype = "txt",
                  controls = controls)){
    sink(file=paste0("models/",controls[["id"]],"/states.txt"))
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
          print(c_table)
        }
        writeLines("COMPARISON between true states and predicted states:\n")
        if(controls[["model"]]=="HMM"){
          compare_true_predicted_states(controls[["states"]][1],decoding,data[["states0"]])
        }
        if(controls[["model"]]=="HHMM"){
          compare_true_predicted_states(controls[["states"]][1],decoding[,1],data[["states0"]][,1],label="CS ")
          writeLines("")
          for(cs_state in seq_len(controls[["states"]][1])){
            writeLines(paste0("Conditional on decoded CS state ",cs_state,":"))
            compare_true_predicted_states(controls[["states"]][2],decoding[decoding[,1]==cs_state,-1],data[["states0"]][decoding[,1]==cs_state,-1],label="FS ")
            writeLines("")
            writeLines(paste0("Conditional on true CS state ",cs_state,":"))
            compare_true_predicted_states(controls[["states"]][2],decoding[data[["states0"]][,1]==cs_state,-1],data[["states0"]][data[["states0"]][,1]==cs_state,-1],label="FS ")
            writeLines("")
          }
        }
      }
    sink()
  }
  
  ### save decoding
  check_saving(object   = decoding,
               filetype = "rds",
               controls = controls)
  
  message("Hidden states decoded.")
}

### save 'object' and check for overwriting
check_saving = function(object=NULL,name=NULL,filetype,controls){
  if(is.null(object) & is.null(name)){
    stop("Either 'object' or 'name' has to be specified.",call.=FALSE)
  }
  path = paste0("models/",controls[["id"]])
  if(!dir.exists(path)){
    dir.create(path)
  }
  if(!is.null(object)){
    name = deparse(substitute(object))
  }
  filename = paste0(path,"/",name,".",filetype)
  if(!file.exists(filename) || controls[["overwrite"]]){
    if(filetype == "rds") {
      saveRDS(object,file=filename)
    } else {
      return(TRUE)
    }
  } else {
    warning(paste0("'",filename,"' already exists and overwriting is forbidden."),call.=FALSE) 
    return(FALSE)
  }
}
