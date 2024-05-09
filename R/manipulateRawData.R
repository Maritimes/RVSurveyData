#' @title makeGSSPECIES_NEW
#' @description This function merges GSSPEC and GSSPECIES_ANDES into a single object
#' @param GSSPECIES_ANDES_ original GSSPECIES_ANDES object
#' @param GSSPEC_ original GSSPEC object 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
makeGSSPECIES_NEW <- function(GSSPECIES_ANDES_=NULL, GSSPEC_=NULL){
  GSSPECIES_APHIA <-  merge(GSSPECIES_ANDES_, GSSPEC_[, c("SPEC","LGRP","LFSEXED")], by.x= "CODE", by.y = "SPEC", all.x=T)
  return(GSSPECIES_APHIA)
}

#' @title fixHerringLengths
#' @description This function creates 
#' @param GSDET_ original GSDET object 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
fixHerringLengths <-function(GSDET_ = NULL){
  #Fix herring lengths - ensure all are in mm
  #NED 2016016 - first instance of measuring herring in mm - convert all prior data from cm to mm
  GSDET_[GSDET_$SPEC == 60 &
           substr(GSDET_$MISSION,4,7) <= 2016 & 
           !GSDET_$MISSION %in% c("NED2016116","NED2016016") & 
           !is.na(GSDET_$FLEN),"FLEN"] <- GSDET_[GSDET_$SPEC == 60 &
                                                   substr(GSDET_$MISSION,4,7) <= 2016 & 
                                                   !GSDET_$MISSION %in% c("NED2016116","NED2016016") & 
                                                   !is.na(GSDET_$FLEN),"FLEN"]*10
  return(GSDET_)
}

#' @title disentangleGSDET
#' @description This function creates 
#' @param GSDET_ original GSDET object 
#' @param GSCAT_ original GSCAT object 
#' @param GSINF_ original GSINF object 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
disentangleGSDET <-function(GSDET_=NULL, GSCAT_= NULL, GSINF_ = NULL){
  # 1 create table from GSDET with CLEN for each MISSION SETNO SPEC FLEN FSEX.
  #  - need to correct CLEN for size_class 1st
  #  - each set will be reduced to 1 record each combination of SPEC, FLEN and FSEX.
  suppressPackageStartupMessages(requireNamespace("dplyr", quietly = TRUE))
  `%>%` <- dplyr::`%>%`
  
  CLEN <- FLEN <- FSEX <- MISSION <- SETNO <- SIZE_CLASS <- SPEC <- NULL
  res<- list()
  #if sex is na, it's fair to say we don't know it.
  GSDET_[is.na(GSDET_$FSEX), "FSEX"] <- 0
  GSDET_<- fixHerringLengths(GSDET_)
  dataLF <- GSDET_
  dataDETS <- GSDET_
  
  dataLF <- dataLF[!is.na(dataLF$FLEN), c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "FSEX","FLEN", "CLEN")]
  dataLF <- dataLF %>%
    dplyr::group_by(MISSION, SETNO, SPEC, SIZE_CLASS, FSEX, FLEN) %>%
    dplyr::summarise(CLEN_RAW = sum(CLEN), .groups = "keep") %>%
    as.data.frame()
  #get the totwgt and sampwgt for every mission/set/spec/size_class combo, and use them to create
  #a ratio, and apply it to existing CLEN
  dataLF <- merge(dataLF, 
                  GSCAT_[,c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","SAMPWGT","TOTWGT")],
                  all.x = T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS"))
  dataLF$CLEN_new <- dataLF$CLEN_RAW
  dataLF[!is.na(dataLF$TOTWGT) & !is.na(dataLF$SAMPWGT) & (dataLF$SAMPWGT> 0) & (dataLF$TOTWGT>0),"CLEN_new"] <- (dataLF[!is.na(dataLF$TOTWGT) & !is.na(dataLF$SAMPWGT) & (dataLF$SAMPWGT> 0) & (dataLF$TOTWGT>0),"TOTWGT"]/dataLF[!is.na(dataLF$TOTWGT) & !is.na(dataLF$SAMPWGT)  & (dataLF$SAMPWGT> 0) & (dataLF$TOTWGT>0),"SAMPWGT"])*dataLF[!is.na(dataLF$TOTWGT) & !is.na(dataLF$SAMPWGT) & (dataLF$SAMPWGT> 0) & (dataLF$TOTWGT>0),"CLEN_RAW"]
  
  #need to bump up CLEN by TOW dist!
  dataLF <- merge(dataLF, GSINF_[,c("MISSION", "SETNO", "DIST")],all.x = T, by = c("MISSION", "SETNO"))
  #force NA dists to 1.75
  dataLF[is.na(dataLF$DIST),"DIST"] <- 1.75
  dataLF$CLEN <- round(dataLF$CLEN_new *(1.75/dataLF$DIST),7)
  dataLF$DIST <- dataLF$SAMPWGT <- dataLF$TOTWGT <- dataLF$CLEN_new <- dataLF$CLEN_RAW <- NULL
  
  #now that we have correct numbers at length for all lengths, we can drop add them (and drop size classes)
  dataLF <- dataLF %>%
    dplyr::group_by(MISSION, SETNO, SPEC, FSEX, FLEN) %>%
    dplyr::summarise(CLEN = sum(CLEN), .groups = "keep") %>%
    as.data.frame()
  
  res$dataLF <- dataLF
  
  # 2 capture the other material from GSDET that is focused on individual measurements - e.g. FSHNO, 
  #   SPECIMEN_ID, FMAT, FLEN, FSEX, FWT, AGE ...
  #  - multiple internal, age-related fields are being dropped
  
  
  dataDETS <- dataDETS[,c("MISSION", "SETNO", "SPEC", "FSHNO", "SPECIMEN_ID", "FLEN", "FSEX", "FMAT",  "FWT", "AGMAT", "AGE")]
  # keep only informative records
  dataDETS <- dataDETS[!is.na(dataDETS$FLEN) |  !is.na(dataDETS$FMAT)| !is.na(dataDETS$FWT)| !is.na(dataDETS$AGE),]
  
  res$dataDETS <- dataDETS
  return(res)
}

#' @title rmSizeClasses
#' @param GSCAT_ original GSCAT object 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
rmSizeClasses <-function(GSCAT_= NULL){
  GSCAT_[is.na(GSCAT_$TOTNO),"TOTNO"]<-0
  GSCAT_[is.na(GSCAT_$TOTWGT),"TOTWGT"]<-0
  GSCAT_[is.na(GSCAT_$SAMPWGT),"SAMPWGT"]<-0
  GSCAT_[is.na(GSCAT_$CALWT),"CALWT"]<-0
  
  
  GSCAT_ <- stats::aggregate(
    x = list(
      CALWT = GSCAT_$CALWT,
      SAMPWGT = GSCAT_$SAMPWGT,
      TOTWGT = GSCAT_$TOTWGT,
      TOTNO = GSCAT_$TOTNO),
    by = list(
      MISSION = GSCAT_$MISSION,
      SETNO = GSCAT_$SETNO,
      SPEC = GSCAT_$SPEC),
    sum
  )
  return(GSCAT_)
}

#' @title addDDCoords
#' @param GSINF_ original GSINF object 
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
addDDCoords <- function(GSINF_=NULL){
  requireNamespace("Mar.utils", quietly = TRUE)
  GSINF_ <- Mar.utils::DDMMx_to_DD(df=GSINF_, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
  colnames(GSINF_)[colnames(GSINF_)=="LAT_DD"] <- "SLAT_DD"
  colnames(GSINF_)[colnames(GSINF_)=="LON_DD"] <- "SLONG_DD"
  GSINF_ <- Mar.utils::DDMMx_to_DD(df=GSINF_, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
  colnames(GSINF_)[colnames(GSINF_)=="LAT_DD"] <- "ELAT_DD"
  colnames(GSINF_)[colnames(GSINF_)=="LON_DD"] <- "ELAT_DD"
  return(GSINF_)
}

## @title roundDD2Min
## @description This function can be used to round decimal degrees to the nearest geographic minute.  
## It can be set to round up or down, and is useful for generating plots with boundaries that make sense.
## @param x this is value that should be rounded.
## @param how the default is \code{"round"}, but values of \code{"ceiling"} and \code{"floor"} are also 
## acceptable. \code{"round"} just rounds the value, while the others forcibly round it up or down 
## (respectively) to the nearest minute.
## @param nearestMin the default is \code{1}, but values between 1 and 60 make sense. 
## @param digits the default is \code{4}.  This is how many decimal places the resultant data should be rounded to.
## @returns the original value, but rounded.
## @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
roundDD2Min<-function(x=NULL, how = "round", nearestMin = 1, digits=4){
  minDD = 0.016666666666 #this is 1 min in DD
  base = nearestMin*minDD
  if (how =="round"){
    res <- base * round(x/base)
  }else if (how =="ceiling"){
    res <- base * ceiling(x/base)
  }else if (how=="floor"){
    res <- base * floor(x/base)
  }
  res <- round(res,digits)
  return(res)
}