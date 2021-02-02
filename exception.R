#' Define exception messages
#'
#' @param code An exception code in character form
#'
#' @return A list containing the following elements:
#' \item{code}{the exception code}
#' \item{response}{the message}
#' \item{debugging}{suggestions for debugging}
#'
#' @examples
#' exception("S.1")
exception = function(code){
  exceptions = data.frame(rbind(
    S.1 = c("'id' invalid","'id' already exists, set a unique identifier"),
    S.2 = c("saving failed","filename already exists and overwriting is forbidden, choose another 'id' or set 'overwriting' to TRUE"),
    C.1 = c("'controls' invalid","'controls' is not checked, supply 'controls' to function 'check_controls'"),
    C.2 = c("'controls' incomplete","some elements of 'controls' have to be specified, see Readme-file"),
    C.3 = c("'controls' misspecified","some elements of 'controls' must fulfill restrictions, see Readme-file"),
    C.4 = c("'controls' contains default values","some elements of 'controls' are not specified and set to default values, see Readme-file"),
    C.5 = c("'controls' contains unsupported elements","some elements of 'controls' are not supported and ignored, check spelling"),
    C.6 = c("'iterlim' reached","selected estimation run reached the iteration limit, consider increasing 'iterlim'"),
    C.7 = c("possibly unidentified states","some states might be unidentified, consider reducing number of states"),
    C.8 = c("Gamma SDD not allowed","Gamma SDD only allowed for non-negative data, select t SDD instead"),
    D.1 = c("'from' invalid","selected values too low, 'from' is set to lower bound of '1902-01-01'"),
    D.2 = c("'symbol' unknown","'symbol' for 'name' not saved, supply 'symbol' to function 'download_data'"),
    D.3 = c("'symbol' invalid","'symbol' does not exist on finance.yahoo.com, check spelling"),
    D.4 = c("'data' invalid","'data' must have a column named 'Date' and columns specified by 'data_col' in 'controls'"),
    F.1 = c("stationary distribution set to uniform distribution","computation of stationary distribution failed, continue with uniform distribution over number of states"),
    F.2 = c("initialisation failed","the likelihood function could not be computed at any of the selected start values, increase 'runs' or adjust 'scale_par' in 'controls'"),
    F.3 = c("bad start values","the likelihood function could not be computed at more than half of the selected start values, increase 'runs' or adjust 'scale_par' in 'controls'"),
    F.4 = c("estimation failed","none of the estimation runs ended successfully, adapt 'accept_codes' or increase 'runs'"),
    F.5 = c("'alpha' invalid","value of 'alpha' needs to be between 0 and 1"),
    F.6 = c("some confidence intervals could not be computed","the corresponding estimates may lie close to the boundaries of their parameter space"),
    V.1 = c("'events' ignored","events' is only used for empirical data"),
    V.2 = c("'events' invalid","make sure that 'dates' and 'names' in 'events' are of the same length")
    ))
  colnames(exceptions) = c("response","debugging")
  if(code %in% rownames(exceptions)){
    return(list("code" = code, "response" = exceptions[code,"response"], "debugging" = exceptions[code,"debugging"]))
  } else {
    message("Error code unknown.")
  }
}
