#' @title updatePackageData
#' @description This function 
#' @param dfList
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom usethis use_data
#' @importFrom purrr walk2
updatePackageData<- function(dfList = NULL){
  purrr::walk2(dfList, names(dfList), function(obj, name) {
    assign(name, obj)
    do.call("use_data", list(as.name(name), overwrite = TRUE))
  })
}