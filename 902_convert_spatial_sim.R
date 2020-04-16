#funcao para converter as bases simuladas ppara sf

convert_spatial <- function(database, projection, origem = T){
  require(sf)
  require(dplyr)
  
  if(origem == T){coord_name <- c("OrLong", "OrLat")}
  else{coord_name <- c("DestLong", "DestLat")}
  
  return(st_as_sf(database, coords = coord_name, crs = projection))
}

