library(rgdal)
library(dplyr)
library(car)
library(jtools)
library(sandwich)

names <- c('masterDatabaseOrigem.shp', 'masterDatabaseOrigemTimeRes.shp')


for(i in names){
  mapa <- readOGR(i)
  
  mapaAux <- foreign::read.dbf('OD 2017/Banco de dados/OD_2017.dbf')
  mapaAux <- rbind(mapaAux[mapaAux$MODOPRIN %in% 9:12,], mapaAux[mapaAux$MODOPRIN %in% 1:6,])
  
  mapaAux %>%
    select(ZONA,PONTO_BR, RENDA_FA, GRAU_INS, CD_ATIVI) -> mapaAux
  
  mapa@data <- bind_cols(mapa@data, mapaAux)
  
  mapa <- mapa[is.na(mapa@data$diffrnc) != T,]
  
  mapa$DURACAOOD <- 60*(mapa$DURACAO + mapa$ANDA_D + mapa$ANDA_O)
  
  modelo <- lm(mapa$diffrnc ~ as.integer(mapa$PONTO_BR) + as.factor(mapa$tipo) + mapa$DURACAOOD + as.factor(mapa$GRAU_INS) + mapa$RENDA_FA + as.factor(mapa$CD_ATIVI))
  modelo <- step(modelo,trace = T)
  
  sink(paste0(stringr::str_remove(i, '.shp'),'.txt'))
  print(summary(modelo))
  sink()


}

#spatial modelling

#reagionalizing variables----
mapa <- readOGR('ZonasODDados.shp', encoding = "UTF-8")
mapaAux <- foreign::read.dbf('OD 2017/Banco de dados/OD_2017.dbf')
mapaAux <- rbind(mapaAux[mapaAux$MODOPRIN %in% 9:12,], mapaAux[mapaAux$MODOPRIN %in% 1:6,])

mapaAux %>%
  select(ZONA,PONTO_BR, RENDA_FA, GRAU_INS, CD_ATIVI, DURACAO, ANDA_D, ANDA_O) -> mapaAux

for(i in 1:length(mapa)){
  
  aux <- mapaAux[mapaAux$ZONA==i,]
  
  DURACAOOD <- mean((aux$DURACAO+aux$ANDA_D+aux$ANDA_O)*60)
  ptBR <- mean(aux$PONTO_BR, na.rm = T)
  rendaM <- mean(aux$RENDA_FA, na.rm = T)
  
  aux %>%
    group_by(GRAU_INS) %>%
    summarise(perc = n()) -> GInst
  
  GInst$perc <- GInst$perc/sum(GInst$perc)
  
  for(j in 2:5){
    assign(paste0(names(GInst)[1],j),value = ifelse(length(GInst$perc[GInst$GRAU_INS == j]) == 0,0,GInst$perc[GInst$GRAU_INS == j]))
    }

  aux %>%
    group_by(CD_ATIVI) %>%
    summarise(perc = n()) -> CDAtiv
  
  CDAtiv$perc <- CDAtiv$perc/sum(CDAtiv$perc)
  
  for(j in 2:8){
    assign(paste0(names(CDAtiv)[1],j),value = ifelse(length(CDAtiv$perc[CDAtiv$CD_ATIVI == j]) == 0,0,CDAtiv$perc[CDAtiv$CD_ATIVI == j]))
  }  
  
  mapa[mapa$NumerZn==i,"RENDAMED"] <- rendaM
  mapa[mapa$NumerZn==i,"PTBRMED"] <- ptBR
  mapa[mapa$NumerZn==i,"DURACAOOD"] <- DURACAOOD
  if(is.na(GRAU_INS2)!= T){mapa[mapa$NumerZn==i,"GRINS2"] <- GRAU_INS2}
  if(is.na(GRAU_INS3)!= T){mapa[mapa$NumerZn==i,"GRINS3"] <- GRAU_INS3}
  if(is.na(GRAU_INS4)!= T){mapa[mapa$NumerZn==i,"GRINS4"] <- GRAU_INS4}
  if(is.na(GRAU_INS5)!= T){mapa[mapa$NumerZn==i,"GRINS5"] <- GRAU_INS5}
  if(is.na(CD_ATIVI2)!= T){mapa[mapa$NumerZn==i,"CDATIV2"] <- CD_ATIVI2}
  if(is.na(CD_ATIVI3)!= T){mapa[mapa$NumerZn==i,"CDATIV3"] <- CD_ATIVI3}
  if(is.na(CD_ATIVI4)!= T){mapa[mapa$NumerZn==i,"CDATIV4"] <- CD_ATIVI4}
  if(is.na(CD_ATIVI5)!= T){mapa[mapa$NumerZn==i,"CDATIV5"] <- CD_ATIVI5}
  if(is.na(CD_ATIVI6)!= T){mapa[mapa$NumerZn==i,"CDATIV6"] <- CD_ATIVI6}
  if(is.na(CD_ATIVI7)!= T){mapa[mapa$NumerZn==i,"CDATIV7"] <- CD_ATIVI7}
  if(is.na(CD_ATIVI8)!= T){mapa[mapa$NumerZn==i,"CDATIV8"] <- CD_ATIVI8}
  rm(list = c("GRAU_INS2",
       "GRAU_INS3",
       "GRAU_INS4",
       "GRAU_INS5",
       "CD_ATIVI2",
       "CD_ATIVI3",
       "CD_ATIVI4",
       "CD_ATIVI5",
       "CD_ATIVI6",
       "CD_ATIVI7",
       "CD_ATIVI8",
       "rendaM", 
       "ptBR"))
  }

writeOGR(mapa, dsn = 'ZonasODDados.shp',
         layer = 'ZonasODDados',
         encoding = "UTF-8",
         driver = "ESRI Shapefile",
         overwrite_layer = T,
         delete_dsn = T)


#modeling variables ----

mapa <- readOGR('ZonasODDados.shp', encoding = "UTF-8")

mapa %>%
  lm(formula = dfDstPb ~ 
       RENDAMED +
#       PTBRMED + 
       DURACAOOD +
#       GRINS2 +
       GRINS3 +
       GRINS4 +
#       GRINS5 +
       CDATIV2 +
       CDATIV3 +
       CDATIV4 +
       CDATIV5 +
       CDATIV6 +
       CDATIV7 +
       CDATIV8) -> modelo

summary(modelo)

vif(modelo)
