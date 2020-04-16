

convert_spatial <- function(database, projection, origem){
  require(sf)
  require(dpyr)
  
  if(origem == T){coord_name <- c("CO_O_X", "CO_O_Y")}
  else{coord_name <- c("CO_D_X", "CO_D_Y")}
  
  return(st_as_sf(database_publico, coords = coord_name, crs = projection))
}
  

  