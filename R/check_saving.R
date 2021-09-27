#' @title 
#' Saving check
#' @description 
#' This function saves model results while checking for overwriting.
#' @param object An object to be saved.
#' @param name A character, the name of the object to be saved.
#' @param filetype A character, the filetype of the object to be saved.
#' @param controls A list of controls.
#' @return A boolean, determining whether saving is possible or not. If \code{filetype="rds"}, \code{object} is saved.

check_saving = function(object=NULL,name=NULL,filetype,controls){
  if(is.null(object) & is.null(name)){
    stop("Either 'object' or 'name' has to be specified.")
  }
  
  ### specify saving path
  path = paste0(controls[["path"]],"/models/",controls[["id"]])
  if(!dir.exists(path)) dir.create(path)
  if(!is.null(object)) name = deparse(substitute(object))
  filename = paste0(path,"/",name,".",filetype)
  
  ### check for overwriting
  if(!file.exists(filename) || controls[["results"]][["overwrite"]]){
    if(filetype == "rds") saveRDS(object,file=filename) else return(TRUE)
  } else {
    warning("S.2", call. = FALSE)
    return(FALSE)
  }
}
