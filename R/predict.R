#' Prediction
#' @description
#' This function...
#' @param model
#' An object of class \code{fHMM_model}.
#' @param ahead
#' A positive integer.
#' @inheritParams compute_ci
#' @return 
#' An object of class \code{fHMM_predict}.

predict = function(model, ahead, ci_level = 0.05) {
  
  ### check input
  
  
  ### extract parameters
  par = parUncon2par(model$estimate, model$data$controls)
  M = model$data$controls$states[1]
  N = model$data$controls$states[2]
  sdds = model$data$controls$sdds
  
  ### predict states
  state_prediction = matrix(NA, nrow = ahead, ncol = M)
  state_prob = replace(numeric(M), tail(model$decoding, n = 1), 1)
  for(i in 1:ahead){
    state_prob = state_prob %*% par$Gamma
    state_prediction[i,] = state_prob
  }
  rownames(state_prediction) = 1:ahead
  colnames(state_prediction) = paste("state", 1:M, sep = "_")

  ### predict data
  data_prediction = matrix(NA, nrow = ahead, ncol = 3)
  props = sort(c(ci_level, 0.5, 1 - ci_level))
  if(sdds[[1]]$name == "t"){
    for(i in 1:ahead){
      data_prediction[i,] = sapply(props, function(x) state_prediction[i,] %*% 
               (qt(p = x, df = par$dfs) * par$sigmas + par$mus))
    }
  }
  if(sdds[[1]]$name == "gamma"){
    for(i in 1:ahead){
      data_prediction[i,] = sapply(props, function(x) state_prediction[i,] %*% 
                qgamma(p = x, shape = par$mus^2/par$sigmas^2, 
                        scale = par$sigmas^2/par$mus))
    }
  }
  rownames(data_prediction) = 1:ahead
  colnames(data_prediction) = c("lb","estimate","ub")
  
  ### build and return output
  out = list("states" = state_prediction, "data" = data_prediction)
  class(out) = "fHMM_predict"
  return(out)
}
