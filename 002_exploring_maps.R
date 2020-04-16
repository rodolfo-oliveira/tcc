library(dplyr)
library(sf)
library(ggplot2)
#abrindo bancos
source(file = '001_opening_filtering_OD.R')

mapa <- sp::SpatialPointsDataFrame  ('OD 2017/Mapas/Shape/Municipios_2017_region.shp')

plot(mapa$geometry)
plot(x=database$CO_O_X, y= database$CO_O_Y, add=T)
plot(ad)

ggplot(data = mapa) +
  geom_sf() +
  geom_point(
    data = database, aes(x = CO_O_X, y = CO_O_Y), size = 1, color = "steelblue"
  ) +
  coord_sf()  # the points are pre-projected

sf::st_crs(mapa)
sp::CRS(projection$wkt)
