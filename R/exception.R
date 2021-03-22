#' @title Debugging
#' @description Provides suggestions for debugging for a given exception code.
#' @param code A character, the exception code.
#' @return A list containing the following elements:
#' \item{code}{exception code}
#' \item{response}{message}
#' \item{debugging}{suggestions for debugging}
#' @examples
#' exception("S.1")
#' @export

exception = function(code){
  
  exceptions = data.frame(rbind(
    S.1 = c("Id invalid.","Id already exists in your path, set a unique identifier."),
    S.2 = c("Saving failed.","Filename already exists in your path and overwriting is forbidden, choose another id or allow overwriting."),
    S.3 = c("Path not found.","Path set in controls not found, check spelling."),
    C.1 = c("Controls invalid.","Controls is not checked, supply controls to check_controls."),
    C.2 = c("Controls incomplete.","Some controls have to be specified, see Readme-file."),
    C.4 = c("Controls misspecified.","Some controls are not supported and ignored, check spelling."),
    C.5 = c("Iterlim reached.","Selected estimation run reached the iteration limit, consider increasing iterlim."),
    C.6 = c("Possibly unidentified states.","Some states might be unidentified, consider reducing number of states."),
    C.7 = c("Gamma SDD not allowed.","Gamma SDD only allowed for non-negative data, select t SDD instead."),
    D.1 = c("Parameter from invalid.","Selected values too low, from is set to lower bound of '1902-01-01'."),
    D.2 = c("Symbol unknown.","Symbol for name not saved, supply symbol to function download_data."),
    D.3 = c("Symbol invalid.","Symbol does not exist on finance.yahoo.com, check spelling."),
    D.4 = c("Data invalid.","Data must have a column named 'Date' and columns specified by data_col in controls."),
    F.1 = c("Stationary distribution set to uniform distribution.","Computation of stationary distribution failed, continue with uniform distribution."),
    F.2 = c("Initialisation failed.","The likelihood function could not be computed at any of the selected start values, increase runs or adjust scale_par in controls."),
    F.3 = c("Bad start values.","The likelihood function could not be computed at more than half of the selected start values, increase runs or adjust scale_par in controls."),
    F.4 = c("Estimation failed.","None of the estimation runs ended successfully, adapt accept_codes or increase runs."),
    F.6 = c("Some confidence intervals could not be computed.","The corresponding estimates may lie close to the boundaries of their parameter space, the confidence intervals may be unreliable and are therefore replaced by NA."),
    V.1 = c("Events ignored.","Events is only used for empirical data."),
    V.2 = c("Events invalid.","Make sure that dates and names in events are of the same length.")
    ))
  
  colnames(exceptions) = c("response","debugging")
  if(code %in% rownames(exceptions)){
    return(list("code" = code, "response" = exceptions[code,"response"], "debugging" = exceptions[code,"debugging"]))
  } else {
    message("Error code unknown.")
  }
}