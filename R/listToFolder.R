#' @title listToFolder
#' @description This function 
#' @param dfList
#' @param dataPath
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
listToFolder <- function(dfList=NULL, dataPath = NULL){
  lapply(names(dfList), function(item_name) {
    df <- dfList[[item_name]]
    rds_filename <- file.path(dataPath, paste0(item_name, "_raw.rds"))
    saveRDS(df, rds_filename)
  })
  return(invisible(NULL))
}