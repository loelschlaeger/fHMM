#' Check for overwriting and save model results
#'
#' @param object An object to be saved, default \code{NULL}
#' @param name The name of an object to be saved, default \code{NULL}
#' @param filetype The filetype of an object to be saved
#' @param controls A list of controls
#' 
#' @return A boolean, determining whether saving is allowed or not 

check_saving = function(object=NULL,name=NULL,filetype,controls){
  if(is.null(object) & is.null(name)) stop("Either 'object' or 'name' has to be specified.")
  
  ### specify saving path
  path = paste0("models/",controls[["id"]])
  if(!dir.exists(path)) dir.create(path)
  if(!is.null(object)) name = deparse(substitute(object))
  filename = paste0(path,"/",name,".",filetype)
  
  ### check for overwriting
  if(!file.exists(filename) || controls[["overwrite"]]){
    if(filetype == "rds") {
      saveRDS(object,file=filename)
    } else {
      return(TRUE)
    }
  } else {
    warning(sprintf("%s (%s)",exception("S.2")[2],exception("S.2")[1]),call.=FALSE)
    return(FALSE)
  }
}