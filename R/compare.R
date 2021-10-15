#' Comparison of multiple \code{fHMM_model}.
#' @description 
#' This function compares multiple \code{fHMM_model}.
#' @param ...
#' A list of one or more objects of class \code{fHMM_model}.
#' @return
#' A data frame.

compare = function(...) {
  
  ### read models
  models = as.list(list(...))
  
  ### get model names
  model_names = unlist(lapply(sys.call()[-1], as.character))
  
  ### check if models are of class "RprobitB_model"
  for(i in seq_len(length(models)))
    if(class(models[[i]]) != "fHMM_model")
      stop(paste0("Model '",model_names[i],"' is not of class 'fHMM_model'."))
  
  ### check if data is the same for each model
  for(i in seq_len(length(models))){
    data_i = as.numeric(unlist(models[[i]]$data$data))
    for(j in 1:i){
      data_j = as.numeric(unlist(models[[j]]$data$data))
      if(!identical(data_i,data_j))
        warning(paste0("Models '",model_names[i],"' and '",model_names[j],
                       "' are not estimated on the same data."))
    }
  }
  
  ### create output matrix
  criteria = c("parameters", "log-likelihood", "AIC", "BIC")
  output = matrix(NA, nrow = length(models), ncol = length(criteria))
  rownames(output) = model_names
  colnames(output) = criteria
  
  ### fill output
  for(i in seq_len(length(models))){
    par_i = length(models[[i]]$estimated_parameter)
    n_i = length(as.vector(models[[i]]$data$data))
    ll_i = models[[i]]$ll
    aic_i = -2*ll_i + 2*par_i
    bic_i = -2*ll_i + par_i*log(n_i)
    output[i,"parameters"] = par_i
    output[i,"log-likelihood"] = ll_i
    output[i,"AIC"] = aic_i
    output[i,"BIC"] = bic_i
  }
  
  ### return output
  return(round(output,2))
}