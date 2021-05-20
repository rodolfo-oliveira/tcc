#funcao para converter as bases simuladas para sp

convert_spatial <- function(database, origem = T){
  require(sp)
  require(dplyr)
  
  if(origem == T){coord_name <- c("OrLong", "OrLat")}
  if(origem == F){coord_name <- c("DestLong", "DestLat")}
  
  return(SpatialPointsDataFrame(coords = database[,coord_name], 
                                data = database, 
                                proj4string = CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 4674,]$prj4))))
}

