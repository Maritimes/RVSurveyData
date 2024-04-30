#' @title extractor
#' @description This function 
#' @param tblNames
#' @param schema
#' @param uid
#' @param pw
#' @param dsn
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
extractor <- function(tblNames=NULL, schema=NULL, uid = NULL, pw = NULL, dsn = "PTRAN"){
  con <- ROracle::dbConnect(DBI::dbDriver("Oracle"), uid, pw, dsn)
  res <- lapply(tblNames,
                function(myTable){
                  sqlStatement <- paste0("select * from ",schema,".",myTable)
                  ROracle::dbGetQuery(con, sqlStatement)
                })
  names(res)<- groundfishTables
  return(res)
}
