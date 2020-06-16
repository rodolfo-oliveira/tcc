#script para mapas LISA



plot_MAP <- function(
  mapa,
  column,
  agregLab, 
  inte,
  col)
{
  require(rgdal)
  require(spdep)
  require(dplyr)
  require(sp)
  require(colortools)
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  
  if(col == 'forestgreen'){
    palette <- RColorBrewer::brewer.pal(6,'YlGn')
  }else(palette <- RColorBrewer::brewer.pal(6,'YlOrRd'))
    
  par(mar=c(0,0,2,0))
  print(spplot(mapa, column, col.regions = palette, cuts=5,
           col = 'gray88', main = agregLab, par.settings=list(fontsize=list(text=20))))



}
