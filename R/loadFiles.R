#' @title loadFiles
#' @description This function 
#' @param tblList
#' @param dataPath
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom stats setnames
loadFiles <- function(tblList = NULL, dataPath = NULL){
  rvData <- stats::setNames(lapply(tblList, function(file) {
    readRDS(file.path(dataPath,  paste0(file, "_raw.rds")))
  }),tblList)
  return(rvData)
}