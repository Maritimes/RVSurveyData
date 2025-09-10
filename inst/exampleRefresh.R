##### SETUP
#source utils for the variables groundfishTblNames and dataPath
tblNames <- c('GSAUX', 'GSCAT', 'GSCURNT', 'GSDET', 'GSFORCE', 'GSGEAR', 'GSHOWOBT',
              'GSINF', 'GSMATURITY', 'GSMISSIONS', 'GSSEX', 'GSSPEC', 'GSSPECIES_ANDES',
              'GSSTRATUM', 'GSWARPOUT', 'GSXTYPE', "GSCRUISELIST")
dataPath<-"C:/DFO-MPO/PESDData/RVSurveyData/"

##### PURPOSE
#' The purpose of this example function is to capture the process for updating the data in the 
#' package.  Data must be extracted, processed, and then loaded back into the package. 
#' For convenience, functions exist for saving list objects to a folder to reduce the need for 
#' the slow process of re-extracting

# 1 Extract the desired data tables to a list
tblList<-extractor(tblNames, schema = "GROUNDFISH", uid = groundfish.username, pw = groundfish.password)

if (F){
  #' 2 If you need to stop working on this, but want to save the objects locally, you can do this 
  #' to write the objects within a list to discrete rds files within a specified folder
  listToFolder(tblList = tblList, dataPath)
  # no need to extract again - just load them:
  tblList <- folderToList(tblNames = tblNames, dataPath )
}

# Manipulate the data as necessary
# convert all herring lengths to mm
# convert GSDET into a length frequency product (dataLF) as well as a table with fish info (dataDETS)
# (size classes still retained here for use in samp wgt ratios)
GSDETProducts <- disentangleGSDET(tblList$GSDET, GSCAT_ = tblList$GSCAT, GSINF_ = tblList$GSINF)
tblList <- c(tblList, GSDETProducts)

#size classes removed from GSCAT
tblList$GSCAT <- rmSizeClasses(tblList$GSCAT)

#add DD versions of coordinates to GSINF
tblList$GSINF <- addDDCoords(tblList$GSINF)

#get all the species info in one place
tblList$GSSPECIES_NEW <- makeGSSPECIES_NEW(GSSPECIES_ANDES_ = tblList$GSSPECIES_ANDES, GSSPEC_ = tblList$GSSPEC)
tblList$GSSPEC <- tblList$GSSPECIES_ANDES <- tblList$GSDET <- GSDETProducts <- NULL
tblList$RVSurveyDataTables <- c('dataDETS', 'dataLF', 'GSAUX', 'GSCAT','GSINF', 'GSMISSIONS', 
                        'GSSPECIES_NEW','GSSTRATUM', 'GSWARPOUT', 'GSXTYPE')

#refresh the Spatial Data
spat <- updatePackageSpatialData()
tblList<-c(tblList,spat)
spat <- NULL
# When all manipulations are complete, load the data objects into the package
library(usethis)
updatePackageData(tblList)


# usethis::use_data(RVSurveyDataTables, overwrite = T)
#' note that the following object exist within the package but are not within the default tables:
#' 
#' GSCRUISELIST
#' GSCURNT
#' GSDET
#' GSFORCE
#' GSGEAR
#' GSHOWOBT
#' GSMATURITY
#' GSSEX

#' 
