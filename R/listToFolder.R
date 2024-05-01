#' @title listToFolder
#' @description This function 
#' @param tblList default is \code{NULL}. This is a list full of named dataframes that should be 
#' saved to the location identified as \code{dataPath}.  These will all be saved as individual *.rds
#' files, and each will have "_raw" appended to the name.
#' @param dataPath default is \code{NULL}. This is the path to the folder where the rds files should
#' be saved.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
listToFolder <- function(tblList=NULL, dataPath = NULL){
  lapply(names(tblList), function(item_name) {
    df <- tblList[[item_name]]
    rds_filename <- file.path(dataPath, paste0(item_name, "_raw.rds"))
    saveRDS(df, rds_filename)
  })
  return(invisible(NULL))
}