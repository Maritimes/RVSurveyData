#' @title updatePackageSpatialData
#' @description This function generates the sf files included in RVSurveyData
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}

if (F){
  #stuff below is for Kasia's new improved layer.
  # the layer read in below already has the US areas, and new fields.
  nafo_sf <- sf::st_read("inst/rawSpatial/NAFO_BEST_KasiaUS2022.shp")
  nafo_sf$OBJECTID_1 <- nafo_sf$ORIG_FID <- nafo_sf$OBJECTID <- nafo_sf$Shape_Leng <- nafo_sf$Shape_Le_1 <- nafo_sf$Shape_Area <- NULL
  colnames(nafo_sf)[colnames(nafo_sf)=="AREA"] <- "AREA_ID"
  
  strataMar4VSW_sf <- sf::st_read("inst/rawSpatial/4VsW_geom.shp")
  colnames(strataMar4VSW_sf)[colnames(strataMar4VSW_sf)=="StrataID"] <- "STRATA_ID"
  colnames(strataMar4VSW_sf)[colnames(strataMar4VSW_sf)=="Areakm"] <- "AREA_KM"
  strataMar4VSW_sf$fid <- NULL
  strataMar4VSW_sf <- sf::st_transform(strataMar4VSW_sf, crs = 4326)
  
  strataMar_sf <- sf::st_read("inst/rawSpatial/MaritimesRegionEcosystemAssessmentStrata(2014-).shp")
  colnames(strataMar_sf)[colnames(strataMar_sf)=="StrataID"] <- "STRATA_ID"
  colnames(strataMar_sf)[colnames(strataMar_sf)=="Areakm"] <- "AREA_KM"
  strataMar_sf$TYPE <- NULL
  strataMar_sf <- sf::st_transform(strataMar_sf, crs = 4326)

  maritimesCoast_sf <- sf::st_read("inst/rawSpatial/maritimesCoast.shp")
  colnames(maritimesCoast_sf)[colnames(maritimesCoast_sf)=="region"] <- "REGION"
  colnames(maritimesCoast_sf)[colnames(maritimesCoast_sf)=="subregion"] <- "SUBREGION"
  maritimesCoast_sf$fid <- maritimesCoast_sf$group <- NULL
  maritimesCoast_sf <- sf::st_transform(maritimesCoast_sf, crs = 4326)
  
  Mar.utils::df_sf_to_gpkg(nafo_sf, layerName = "nafo_sf", gpkgName = "RVSurveyDataSpatial.gpkg")
  Mar.utils::df_sf_to_gpkg(strataMar4VSW_sf, layerName = "strataMar4VSW_sf", gpkgName = "RVSurveyDataSpatial.gpkg")
  Mar.utils::df_sf_to_gpkg(strataMar_sf, layerName = "strataMar_sf", gpkgName = "RVSurveyDataSpatial.gpkg")
  Mar.utils::df_sf_to_gpkg(maritimesCoast_sf, layerName = "maritimesCoast_sf", gpkgName = "RVSurveyDataSpatial.gpkg")

  #if you pull from the gpkg, the geometry column will become geom
  # strataMar_sf<-st_read("RVSurveyDataSpatial.gpkg", layer= "strataMar_sf")
  # colnames(strataMar_sf)[colnames(strataMar_sf)=="geom"] <- "geometry"
  # st_geometry(strataMar_sf) <- "geometry"
  # strataMar4VSW_sf<-st_read("RVSurveyDataSpatial.gpkg", layer= "strataMar4VSW_sf")
  # colnames(strataMar4VSW_sf)[colnames(strataMar4VSW_sf)=="geom"] <- "geometry"
  # st_geometry(strataMar4VSW_sf) <- "geometry"
  # nafo_sf<-st_read("RVSurveyDataSpatial.gpkg", layer= "nafo_sf")
  # colnames(nafo_sf)[colnames(nafo_sf)=="geom"] <- "geometry"
  # st_geometry(nafo_sf) <- "geometry"
  # maritimesCoast_sf<-st_read("RVSurveyDataSpatial.gpkg", layer= "maritimesCoast_sf")
  # colnames(maritimesCoast_sf)[colnames(maritimesCoast_sf)=="geom"] <- "geometry"
  # st_geometry(maritimesCoast_sf) <- "geometry"  
  
  usethis::use_data(nafo_sf, overwrite = TRUE)
  usethis::use_data(strataMar_sf, overwrite = TRUE)
  usethis::use_data(strataMar4VSW_sf, overwrite = TRUE)
  usethis::use_data(maritimesCoast_sf, overwrite = TRUE)
}

if (F){
#'   #' created generously large area (based on the strata) 
#'   #' for which to extract coastline and bathy
strataRng1<- as.vector(sf::st_bbox(strataMar_sf))
strataRng2<- as.vector(sf::st_bbox(strataMar4VSW_sf))
strataRng <- c(pmin(strataRng1[1], strataRng2[1]),
               pmin(strataRng1[2], strataRng2[2]),
               pmax(strataRng1[3], strataRng2[3]),
               pmax(strataRng1[4], strataRng2[4]))

limits <-   c(roundDD2Min(strataRng[1],nearestMin=30, how = "floor"),
              roundDD2Min(strataRng[3],nearestMin=30, how = "ceiling"),
              roundDD2Min(strataRng[2],nearestMin=30, how = "floor"),
              roundDD2Min(strataRng[4],nearestMin=30, how = "ceiling"))
bbox <- sf::st_bbox(c(xmin = limits[1], xmax = limits[2], ymin = limits[3], ymax = limits[4]), crs = sf::st_crs(4326))
bbox_poly <- sf::st_as_sfc(bbox)

library(mapdata)
library(marmap)
library(dplyr)
library(sf)
library(lwgeom)
data <- ggplot2::map_data("world2Hires",region = c('Canada', 'USA', 'France'), wrap = c(-180,180))
sf_data <- sf::st_as_sf(data, coords = c("long", "lat"), crs = 4326)
sf::st_write(sf_data, "test.shp")
sf::st_write(bbox_poly, "bbox_poly.shp",append=F)
# <do GIS stuff>
strataMar_sf<-st_read("RVSurveyDataSpatial.gpkg", layer= "strataMar_sf")
# attemps below to export a cropped polygon of the desired area failed. So I did in in QGIS

# intersected_data <- sf::st_intersection(sf_data, bbox_poly)
# grouped_data <- intersected_data %>% 
#   group_by(region, subregion, group) %>% 
#   summarise(do_union = FALSE) %>% 
#   st_cast("POLYGON") %>% 
#   summarise(geometry = sf::st_union(lwgeom::lwgeom_make_valid(geometry)), do_union = FALSE)



maritimesLand_grp <- maritimesLand %>% 
  group_by(region, subregion, group) %>% 
  summarise(geometry = sf::st_combine(sf::st_as_sf(., coords = c("long", "lat"), crs = 4326)))
# maritimesLand_sf <- sf::st_as_sf(maritimesLand_grp, coords = c("long", "lat"), crs = 4326)
maritimesLand_sf <- sf::st_as_sf(maritimesLand_grp, crs = 4326)
tt <- st_crop(maritimesLand_sf, bbox)


sf_data <- st_cast(maritimesLand_sf, "POLYGON")

tt<- ggplot2::geom_polygon(data = maritimesLand, ggplot2::aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) 

maritimesLand_sf <- sf::st_crop(maritimesLand_sf, bbox)
sf_data <- sf::st_cast(maritimesLand_sf, "subregion")
maritimesBathy <- marmap::fortify.bathy(marmap::getNOAA.bathy(lon1 = limits[1],
                                                              lon2 = limits[2],
                                                              lat1 = limits[3],
                                                              lat2 = limits[4],
                                                              resolution = 1))
maritimesBathy_sf <- sf::st_as_sf(maritimesBathy, coords = c("x", "y"), crs = 4326)
}
usethis::use_data(nafo_sf, overwrite = TRUE)
usethis::use_data(strataMar_sf, overwrite = TRUE)
usethis::use_data(strataMar4VSW_sf, overwrite = TRUE)
usethis::use_data(maritimesLand_sf, overwrite = TRUE)
usethis::use_data(maritimesBathy, overwrite = TRUE)


#'   
#'   library(mapdata)
#'   
#'   maritimesLand <- ggplot2::map_data("world2Hires",region = c('Canada', 'USA', 'France'), wrap = c(-180,180))
#'   maritimesBathy <- marmap::fortify.bathy(marmap::getNOAA.bathy(lon1 = limits[1], 
#'                                                           lon2 = limits[2], 
#'                                                           lat1 = limits[3], 
#'                                                           lat2 = limits[4], 
#'                                                           resolution = 1))
#'   usethis::use_data(maritimesLand, overwrite = TRUE)
#'   usethis::use_data(maritimesBathy, overwrite = TRUE)
#' }

