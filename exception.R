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
    S.1 = c("",""),
    C.1 = c("object 'controls' is invalid","object 'controls' has not successfully passed function 'check_controls'")
    ))
  colnames(exceptions) = c("response","definition")
  if(code %in% rownames(exceptions)){
    return(list("response" = exceptions[code,"response"], "definition" = exceptions[code,"definition"]))
  } else {
    stop("Error code unknown.",call.=FALSE)
  }
}
