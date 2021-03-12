#' @title Saving check
#' @description Checks for overwriting and saves model results.
#' @param object An .rds-object to be saved, default \code{NA}.
#' @param name A character, the name of the object to be saved, default \code{NA}.
#' @param filetype A character, the filetype of the object to be saved.
#' @param controls A list of controls.
#' @return A boolean, determining whether saving is possible or not. If \code{filetype="rds"}, \code{object} is saved.

check_saving = function(object=NA,name=NA,filetype,controls){
  if(is.na(object) && is.na(name)){
    stop("Either 'object' or 'name' has to be specified.")
  }
  
  ### specify saving path
  path = paste0(controls[["path"]],"/models/",controls[["id"]])
  if(!dir.exists(path)) dir.create(path)
  if(any(!is.na(object))) name = deparse(substitute(object))
  filename = paste0(path,"/",name,".",filetype)
  
  ### check for overwriting
  if(!file.exists(filename) || controls[["results"]][["overwrite"]]){
    if(filetype == "rds") saveRDS(object,file=filename) else return(TRUE)
  } else {
    warning(sprintf("%s (%s)",exception("S.2")[2],exception("S.2")[1]),call.=FALSE)
    return(FALSE)
  }
}
