#' @title updatePackageData
#' @description This function 
#' @param tblList default is \code{NULL}. This is a list full of named dataframes that will be 
#' added to the package.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
updatePackageData<- function(tblList = NULL){
  requireNamespace("usethis", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  purrr::walk2(tblList, names(tblList), function(obj, name) {
    assign(name, obj)
    do.call("use_data", list(as.name(name), overwrite = TRUE))
  })
}