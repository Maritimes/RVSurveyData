#check utils for the variables groundfishTables and sharepointPath
test<-extractor(tblNames = groundfishTables, schema = "GROUNDFISH", uid = groundfish.username, pw = groundfish.password)
listToFolder(dfList = test, dataPath = "C:/Users/McMahonM/OneDrive - DFO-MPO/RVTransmogrifier")
tblList <- loadFiles(tblList = groundfishTables, dataPath = sharepointPath)
updatePackageData(tblList)
