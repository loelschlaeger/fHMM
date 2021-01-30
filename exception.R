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
    S.1 = c("'id' invalid","'id' already exists, choose another one"),
    C.1 = c("'controls' invalid","supply 'controls' to function 'check_controls'"),
    C.2 = c("'controls' incomplete","some elements of 'controls' have to be specified, see Readme-file"),
    C.3 = c("'controls' misspecified","some elements of 'controls' must fulfill restrictions, see Readme-file"),
    C.4 = c("defaults for 'controls'","some elements of 'controls' are not specified and set to default values, see Readme-file"),
    C.5 = c("'controls' contains unsupported elements","some elements of 'controls' are not supported"),
    D.1 = c("'from' invalid","'from' is set to lower bound of '1902-01-01'"),
    D.2 = c("'symbol' unknown","supply 'symbol' to function 'download_data'"),
    D.3 = c("'symbol' invalid","check spelling, 'symbol' does not exist on finance.yahoo.com"),
    D.4 = c("'data' invalid","'data' must have a column names 'Date' and columns specified by 'data_col' in 'controls'"),
    F.1 = c("stationary distribution set to uniform distribution","computation of stationary distribution failed"),
    F.2 = c("initialisation failed","adjust 'scale_par' in 'controls'"),
    F.3 = c("bad start values","adjust 'scale_par' in 'controls'"),
    F.4 = c("estimation failed","none of the estimation runs ended successfully, increase 'runs'"),
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
