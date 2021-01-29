#' Define exception messages
#'
#' @param code An exception code
#'
#' @return A list containing the following elements:
#' \item{response}{the printed message}
#' \item{definition}{the definition of the exception}
#'
#' @examples
#' exception("S.1")
exception = function(code){
  exceptions = data.frame(rbind(
    S.1 = c("","",""),
    C.1 = c("'controls' invalid","'controls' has not successfully passed function 'check_controls'","pass 'controls' to 'check_controls'"),
    V.1 = c("'events' ignored","'events' is not used by the code","'events' is only used for empirical data"),
    V.2 = c("'events' invalid","lengths of 'dates' and 'names' in 'events' differ","make sure that 'dates' and 'names' are of the same length")
    ))
  colnames(exceptions) = c("response","definition","suggestion")
  if(code %in% rownames(exceptions)){
    return(list("code" = code, "response" = exceptions[code,"response"], "definition" = exceptions[code,"definition"], "suggestion" = exceptions[code,"suggestion"]))
  } else {
    stop("Error code unknown.",call.=FALSE)
  }
}