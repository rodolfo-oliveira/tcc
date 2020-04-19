#funcao para converter as bases da OD para sf

convert_spatial <- function(database, origem = T){
  require(sp)
  require(dplyr)
  
  if(origem == T){coord_name <- c("CO_O_X", "CO_O_Y")}
  if(origem == F){coord_name <- c("CO_D_X", "CO_D_Y")}
  return(SpatialPointsDataFrame(coords = database[,coord_name], 
                                data = database, 
                                proj4string = CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 31983,]$prj4))))
}
  

  