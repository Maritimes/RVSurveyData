#' @title extractor
#' @description This function 
#' @param tblNames default is \code{NULL}. This is a list full of table names corresponding to 
#' tables that should be extracted from the schema identified in  \code{schema}
#' @param schema default is \code{NULL}. This is the schema that houses the tables listed in 
#' \code{tblNames}.
#' @param uid default is \code{NULL}. This is an oracle username with access to the desired tables.
#' @param pw default is \code{NULL}. This is the oracle password for the account listed as \code{uid}
#' @param dsn default is \code{"PTRAN"}.  This is the name of the database.  Almost certainly PTRAN.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
extractor <- function(tblNames=NULL, schema=NULL, uid = NULL, pw = NULL, dsn = "PTRAN"){
  requireNamespace("ROracle", quietly = TRUE)
  con <- ROracle::dbConnect(DBI::dbDriver("Oracle"), uid, pw, dsn)
  res <- lapply(tblNames,
                function(myTable){
                  sqlStatement <- paste0("select * from ",schema,".",myTable)
                  ROracle::dbGetQuery(con, sqlStatement)
                })
  names(res)<- tblNames
  return(res)
}
