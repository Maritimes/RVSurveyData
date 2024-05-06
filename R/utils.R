groundfishTblNames <- c('GSAUX', 'GSCAT', 'GSCURNT', 'GSDET', 'GSFORCE', 'GSGEAR', 'GSHOWOBT',
'GSINF', 'GSMATURITY', 'GSMISSIONS', 'GSSEX', 'GSSPEC', 'GSSPECIES_ANDES',
'GSSTRATUM', 'GSWARPOUT', 'GSXTYPE')
data = "C:/Users/McMahonM/OneDrive - DFO-MPO/Documents - Data Management Team-Test Shared Channel/RVSurveyData"
#"https://086gc.sharepoint.com/sites/DataManagementTeam-SharedChannel/Shared%20Documents/RVSurveyData""

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