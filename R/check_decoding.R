#' @title Decoding check
#' @description Summarizes and saves decoded states.
#' @param decoding A vector (in case of a HMM) or a matrix (in case of a hierarchical HMM) of decoded states.
#' @param data A list of processed data information.
#' @param controls A list of controls.
#' @return No return value. Creates output file "states.txt".

check_decoding = function(decoding,data,controls){
  if(check_saving(name = "states",filetype = "txt",controls = controls)){
    sink(file=paste0(controls[["path"]],"/models/",controls[["id"]],"/states.txt"))
      
      ### frequency of decoded states
      writeLines("Frequency of decoded states\n")
      if(controls[["model"]]=="HMM"){ 
        out = table((decoding))
        names(out) = paste("state",names(out))
        print(out)
        cat("\n")
      }
      if(controls[["model"]]=="HHMM"){
        out_cs = table(factor(decoding[,1],levels = seq_len(controls[["states"]][1])))
        names(out_cs) = paste("CS state",names(out_cs))
        print(out_cs)
        cat("\n")
        for(state in seq_len(controls[["states"]][1])){
          writeLines(paste0("Conditional on CS state ",state,":"))
          out_fs = table(factor(decoding[decoding[,1]==state,-1],levels = seq_len(controls[["states"]][2])))
          names(out_fs) = paste("FS state",names(out_fs))
          print(out_fs)
          cat("\n")
        }
      }
      
      ### comparison between true states and predicted states
      if(controls[["sim"]]){
        compare_true_predicted_states = function(no_states,decoded_states,true_states,label=NULL){
          c_table = matrix(0,no_states,no_states)
          rownames(c_table) = paste0("true ",label,"state ",seq_len(no_states))
          colnames(c_table) = paste0("decoded ",label,"state ",seq_len(no_states))
          for(i in seq_len(no_states)) for(j in seq_len(no_states)){
            value = sum(decoded_states==i & true_states==j) / sum(decoded_states==i) * 100
            c_table[i,j] = if(is.nan(value)) "NA" else sprintf("%.0f%%",value)
          }
          print(c_table,quote=FALSE,right=TRUE)
        }
        writeLines("Comparison between true and decoded states\n")
        if(controls[["model"]]=="HMM"){
          compare_true_predicted_states(controls[["states"]][1],decoding,data[["states0"]])
        }
        if(controls[["model"]]=="HHMM"){
          compare_true_predicted_states(controls[["states"]][1],decoding[,1],data[["states0"]][,1],label="CS ")
          writeLines("")
          for(cs_state in seq_len(controls[["states"]][1])){
            writeLines(paste0("Conditional on true CS state ",cs_state,":"))
            compare_true_predicted_states(controls[["states"]][2],decoding[data[["states0"]][,1]==cs_state,-1],data[["states0"]][data[["states0"]][,1]==cs_state,-1],label="FS ")
            writeLines("")
          }
        }
      }
    sink()
  }
  
  ### save decoding
  check_saving(object = decoding, filetype = "rds", controls = controls)
  message("Hidden states decoded.")
}