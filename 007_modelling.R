library(rgdal)
library(dplyr)
library(car)
library(dummies)
options(scipen = 999)
#library(spgwr)
library(GWmodel)
library(spdep)
#library(gwrr)



#testes T----
#calculo dos testes T de diferença de médias para verificar a existencia de diferença de tempos de viagem simulados e da Pesquisa OD para transporte público e privado

#loop para realizar a analise a partir dos bancos com e sem  consideração da restrição de tempo
for(j in c('TimeRes', '')){
  print (j)
  #leitura do banco de dados
  mapa <- readOGR(paste0('masterDatabaseOrigem',j,'.shp'))
  #impressão do nome do banco para confirmaçao
  print(paste0('masterDatabaseOrigem',j,'.shp'))
  #loop para operar os dados de trasnporte público e privado
  for(i in c('publico', 'privado')){
    
    
    sink(paste0('testeT',i,j,'.txt'))
    print(t.test(x = mapa@data$smltdTm[mapa@data$tipo==i], 
           y = 60*(mapa@data$DURACAO[mapa@data$tipo==i] + mapa@data$ANDA_D[mapa@data$tipo==i] + mapa@data$ANDA_O[mapa@data$tipo==i]),
           paired = T, conf.level = 0.95))
    
    print(wilcox.test(x = mapa@data$smltdTm[mapa@data$tipo==i], 
                y = 60*(mapa@data$DURACAO[mapa@data$tipo==i] + mapa@data$ANDA_D[mapa@data$tipo==i] + mapa@data$ANDA_O[mapa@data$tipo==i]),
                paired = T, conf.level = 0.95))
    sink()
    
    
  }
}



#associando as variaveis aos bancos de pontos----
#names <- c('masterDatabaseOrigem.shp', 
#           'masterDatabaseOrigemTimeRes.shp',
#           'masterDatabaseDestino.shp',
#           'masterDatabaseDestinoTimeRes.shp')
#
#for(i in names){
#
#  
#  mapa <- readOGR(i, encoding = 'UTF-8')
#  
#  mapaAux <- foreign::read.dbf('OD 2017/Banco de dados/OD_2017.dbf')
#  mapaAux <- rbind(mapaAux[mapaAux$MODOPRIN %in% 9:12,], mapaAux[mapaAux$MODOPRIN %in% 1:6,])
#  
#  mapaAux %>%
#    select(ZONA,PONTO_BR, RENDA_FA, GRAU_INS, CD_ATIVI,MOTIVO_D, MOTIVO_O) -> mapaAux
#  
#  #outros
#  mapaAux$MOTIVO <- 3
#  
#  
#  #trabalho 
#  mapaAux$MOTIVO[(mapaAux$MOTIVO_D %in% c(1,2,3) & mapaAux$MOTIVO_O %in% c(8)) |
#                     (mapaAux$MOTIVO_O %in% c(1,2,3) & mapaAux$MOTIVO_D %in% c(8))] <- 1
#  # escola
#  mapaAux$MOTIVO[(mapaAux$MOTIVO_D %in% c(4) & mapaAux$MOTIVO_O %in% c(8)) |
#                   (mapaAux$MOTIVO_O %in% c(4) & mapaAux$MOTIVO_D %in% c(8))] <- 2
#
#
#  
#  mapa@data <- bind_cols(mapa@data, mapaAux)
#  
#  writeOGR(mapa,
#           dsn = i,
#           layer = i,
#           driver = 'ESRI Shapefile',
#           delete_dsn = T,
#           overwrite_layer = T,
#           encoding = 'UTF-8')
#}
#

## analises de correlaçao ----
#nomes dos bancos 
names <- c('masterDatabaseOrigem.shp', 'masterDatabaseOrigemTimeRes.shp')


for(i in names){

  #leitura do banco
  mapa <- readOGR(i, encoding = 'UTF-8')

  #retirada de valores nulos
  mapa <- mapa[is.na(mapa@data$diffrnc) != T,]
  
  #calculo da duraçao de viagem da pesquisa OD considerando tempo andando e conversão em segundos
  mapa$DURACAOOD <- 60*(mapa$DURACAO + mapa$ANDA_D + mapa$ANDA_O)
  
  #fitragem dos dados a serem usados na analise
  mapa@data <- mapa@data[,c("diffrnc","ZONA","PONTO_BR","RENDA_FA","GRAU_INS","CD_ATIVI","MOTIVO","DURACAOOD","tipo",'smltdTm')]
  
  #calculo da média da pontuação brasil para as zonas OD
  mapa@data %>%
    group_by(ZONA) %>%
    summarise(mean = mean(PONTO_BR,na.rm=T)) -> PBRmeans
  
  #Associação da média da pontuação brasil para as observações da zona com valores nulos
  for(j in 1:length(PBRmeans$ZONA)){
    mapa@data[is.na(mapa@data$PONTO_BR) & mapa@data$ZONA == PBRmeans$ZONA[j],'PONTO_BR'] <- PBRmeans$mean[j]
  }
  
  mapa <- mapa[sample(1:nrow(mapa)), ]
  
  
  #scatter panel
  my_cols <- c("forestgreen", "darkgoldenrod2")  

  plot_colors <- my_cols
  text <- c("Privado","Público")
  
  #correlation panel
  panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y,use = 'complete.obs'), digits=2)
    print(r)
    txt <- paste0("R = ", r)
    cex.cor <- 1
    text(0.5, 0.5, txt, cex = cex.cor * (1.6+abs(r)))
  }
  # Customize upper panel
  upper.panel<-function(x, y){
    points(x,y, pch = 19, col = my_cols[mapa@data$tipo], cex = 0.1)
  }
  # Create the plots
  jpeg(filename = paste0(stringr::str_remove(i,'.shp'),"correlacao.jpeg"), width = 750, height = 500,quality = 100)
  pairs(mapa@data[,c("diffrnc","PONTO_BR","RENDA_FA","DURACAOOD")], #main = '',
        labels = c('Diferença', "Pontuação Critério Brasil",'Renda Familiar','Duração viagem OD'),  cex.labels =  1.5,
        lower.panel = panel.cor,
        upper.panel = upper.panel, oma=c(8,4,4,4))
  
  
  par(xpd=TRUE)
  legend(0.45,0.05,legend = text, text.width = max(sapply(text, strwidth)),
              fill=plot_colors,horiz = T)
  dev.off()


  
  #modeling of data----
  
  #transformando variáveis qualitativas em factors
  mapa$GRAU_INS <- as.factor(mapa$GRAU_INS)
  mapa$CD_ATIVI <- as.factor(mapa$CD_ATIVI)
  mapa$MOTIVO <- as.factor(mapa$MOTIVO)
  mapa$tipo <- as.factor(mapa$tipo)
 # mapa$PONTO_BR[is.na(mapa$PONTO_BR)]  <- mean(mapa$PONTO_BR, na.rm = T)

  #filtro de colunas com dados para analise
  mapa@data <- mapa@data[,colnames(mapa@data) %in% c("MOTIVO_D","MOTIVO_O")==F]
  
  #transformação das variaveis factor em variaveis dummy
  mapa@data <- dummy.data.frame(data = mapa@data)
  
  
  
  
  #modelagem simples
  modelo <- lm(data = mapa@data[,colnames(mapa@data) %in% c("ZONA","MOTIVO1","tipoprivado","GRAU_INS1","CD_ATIVI1", "smltdTm")==F], formula = diffrnc ~. )
  #stepwise do modelo
  modelo <- step(modelo,trace = T)
  
  
  #all_vifs <- car::vif(modelo)
  #signif_all <- names(all_vifs)
  #while(any(all_vifs > 4)){
  #  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  #  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  #  myForm <- as.formula(paste("ozone_reading ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  #  selectedMod <- lm(myForm, data=inputData)  # re-build model with new formula
  #  all_vifs <- car::vif(selectedMod)
  #}
  
  #retirada de valores não significantes
  all_vars <- names(modelo[[1]])[-1]  # names of all X variables
  # Get the non-significant vars
  summ <- summary(modelo)  # model summary
  pvals <- summ[[4]][, 4]  # get all p values
  not_significant <- character()  # init variables that aren't statsitically significant
  not_significant <- names(which(pvals > 0.05))
  not_significant <- not_significant[!not_significant %in% "(Intercept)"]  # remove 'intercept'. Optional!
  
  # If there are any non-significant variables, 
  while(length(not_significant) > 0){
    all_vars <- all_vars[!all_vars %in% not_significant[1]]
    all_vars <- unique(stringr::str_remove(all_vars, '(?<=\\)).+'))
    myForm <- as.formula(paste("diffrnc ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
    modelo <- lm(data = mapa@data, formula = myForm)  # re-build model with new formula
    
    # Get the non-significant vars.
    summ <- summary(modelo)
    pvals <- summ[[4]][, 4]
    not_significant <- character()
    not_significant <- names(which(pvals > 0.05))
    not_significant <- not_significant[!not_significant %in% "(Intercept)"]
  }
  summary(modelo)
  
  #impressão do modelo final
  sink(paste0(stringr::str_remove(i, '.shp'),'.txt'))
  print(summary(modelo))
  print(vif(modelo))
  sink()

  
  
  #spatial GWR diagnostics----
  #diagnósticos espaciais
  
  #leitura do mapa de distritos de Sâo Paulo
  mapa <- readOGR(dsn = 'OD 2017/Mapas/Shape/Distritos_2017_region.shp', encoding = 'UTF-8')
  
  #condicional para ajuste do nome da base de analise
  if(i =='masterDatabaseOrigem.shp'){
    nomes <- c('Origem - ', "Destino - ")
    }else{nomes <- c('Destino com Restrição Temporal - ',
                  'Origem Restrição Temporal - ')}
  
  #nomes das variáveis de analise
  variableNames <- c("Intercept",
                     "RENDA_FA",
                     "PONTO_BR",
                     "GRAU_INS2",
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
                     "MOTIVO2",
                     "MOTIVO3",
                     "DURACAOOD",
                     "tipopublico")
  
  #nome por extenso das variáveis de analise
  xtenseVariableNames <-  c("Intercepto",
                            "Renda Familiar",
                            "Pontuação Crit. Brasil",
                            "Ensino Fundamental II Incompleto",
                            "Ensino Médio Incompleto",
                            "Ensino Superior Incompleto",
                            "Ensino Superior Completo",
                            "CA: Faz Bico",
                            "CA: Em Licença Médica",
                            "CA: Aposentado/Pensionista",
                            "CA: Sem Trabalho",
                            "CA: Nunca Trabalhou",
                            "CA: Dona de Casa",
                            "CA: Estudante",
                            "Motivo Escola",
                            "Outros Motivos",
                            "Duração ViagemOD",
                            "Transporte Público")
  
  #loop para analise dos bancos de dados
  for(p in nomes){
    #condicionais para leitura do banco de dados
    if(stringr::str_detect(p, "Origem") & stringr::str_detect(p, "Restrição")){pontosMA <- rgdal::readOGR(dsn="masterDatabaseOrigemTimeRes.shp",layer='masterDatabaseOrigemTimeRes')}
    if(!stringr::str_detect(p, "Origem") & stringr::str_detect(p, "Restrição")){pontosMA <- rgdal::readOGR(dsn="masterDatabaseDestinoTimeRes.shp",layer='masterDatabaseDestinoTimeRes')}
    if(stringr::str_detect(p, "Origem") & !stringr::str_detect(p, "Restrição")){pontosMA <- rgdal::readOGR(dsn="masterDatabaseOrigem.shp",layer='masterDatabaseOrigem')}
    if(!stringr::str_detect(p, "Origem") & !stringr::str_detect(p, "Restrição")){pontosMA <- rgdal::readOGR(dsn="masterDatabaseDestino.shp",layer='masterDatabaseDestino')}
    
    #separação dos dados para analise, retirando os as linhas onde a diferença calculada é nula
    pontosMA <- pontosMA[is.na(pontosMA$diffrnc)==F,]
    
    #criação de uma grade para impressao dos dados
    grd <- SpatialGrid(GridTopology(c(313767,7357074),c(1000,1000),c(50,56)))
    
    #cálculo das médias zonais da pontuação brasil
    pontosMA@data %>%
      group_by(ZONA) %>%
      summarise(mean = mean(PONTO_BR,na.rm=T)) -> PBRmeans
    
    #associaçao das médias zonais da pontuação brasil às abservações dentro da zona com valor nulo
    for(l in 1:length(PBRmeans$ZONA)){
      pontosMA@data[is.na(pontosMA@data$PONTO_BR) & pontosMA@data$ZONA == PBRmeans$ZONA[l],'PONTO_BR'] <- PBRmeans$mean[l]
    }
    
    pontos <- pontosMA#[pontosMA$TIPO=='publico',]
    #pontos <- pontos[sample(length(pontos),replace = F,size = length(pontos)*0.3),]
    
    
    ############################################################################
    ## GWR - Geographically Weighted Regression ################################----
    ############################################################################
    
    pontos$DURACAOOD <- 60*(pontos$DURACAO + pontos$ANDA_O + pontos$ANDA_D)
    
    #filtro de colunas com dados para analise
    pontos@data <- pontos@data[,c("diffrnc","PONTO_BR","RENDA_FA","GRAU_INS","CD_ATIVI","MOTIVO","DURACAOOD", "tipo")]
   
    #transformando variáveis qualitativas em factors
    pontos$CD_ATIVI <- as.factor(pontos$CD_ATIVI)
    pontos$GRAU_INS <- as.factor(pontos$GRAU_INS)
    pontos$MOTIVO <- as.factor(pontos$MOTIVO)
    pontos$tipo <- as.factor(pontos$tipo)
    #transformação das variaveis factor em variaveis dummy
    pontos@data <- dummies::dummy.data.frame(pontos@data)
    

    
    
    #pontos <- pontos[sample(length(pontos),replace = F,size = length(pontos)*0.2),]
    
    #a <- matrix(nrow = length(amostra), ncol = length(amostra))
    #for(r in 1:length(a[1,])){
    #  aux <- gw.dist(dp.locat = coordinates(amostra), focus = r)
    #  a[,r] <- aux[,1]
    #  print(r)
    #}
    
    
    #formula sando a diferença de tempos de viagem e todas as variáveis do modelo
    formula <- as.formula(paste("diffrnc ~ ", paste(names(modelo[[1]])[-1], collapse=" + "), sep=""))
    # Calcula largura de banda (em # de registros) para diversos tipos de kernel
    
   

    #bw.ap  <- bw.gwr(dMat = a, formula,
    #                 data=amostra, approach="AICc", 
    #                 kernel="gaussian", adaptive=TRUE)
    #rm(a)

    #calculo da matriz de distancias para modelo de regressão espacial
    DW <- gw.dist(dp.locat = coordinates(pontos), rp.locat = coordinates(grd),focus = 0)

    #calculo do modelo de regressão espacial
    gwr.ap <- gwr.basic(formula = formula,
                        regression.points = grd, 
                        data = pontos, 
                        bw=130, 
                        kernel="gaussian",
                        adaptive=TRUE,
                        dMat = DW,
                        cv = T,F123.test = T
                        )
    
    #salvando o modelo
    save(gwr.ap,file = stringr::str_remove_all(paste0(i, p), 'shp|\\.| |-'))
    
    sink(paste0(stringr::str_remove_all(paste0(i, p), 'shp|\\.| |-'),'.txt'))
    print(gwr.ap)
    sink()
    
    rm(DW)
    
    
    #imprimindo resultados do modelo
    
    #definindo nomes, cores e layouts
    plotnames <- colnames(gwr.ap$SDF@data)
    mypalette <- RColorBrewer::brewer.pal(11,'RdBu')
    map.layout <- list(as(mapa[mapa$NumeroDist %in% 1:96,], "SpatialLines"), width = 1, col = 'gray35')
    
    #loop para impressão dos mapas
    for (j in 1:length(plotnames)){
      jpeg(filename = paste0('71', 
                             which(p==nomes),
                             "_GWR_",
                             stringr::str_remove_all(p," |-"),
                             "_",
                             stringr::str_remove_all(xtenseVariableNames[which(variableNames == plotnames[j])]," |:|-|\\/"),'.jpeg'),width = 650, height = 800, quality = 100)
      
      print(spplot(gwr.ap$SDF, plotnames[j], key.space = "right", par.settings=list(fontsize=list(text=20)),
             col.regions = mypalette, cuts = 10, sp.layout = map.layout,
             main = paste0(strwrap(paste(p,  xtenseVariableNames[which(variableNames == plotnames[j])], sep = " "), width = 25), collapse = '\n')))
      dev.off()
    }
  }
  

}

#spatial modelling

#reagionalizing variables----
#mapa <- readOGR('ZonasODDados.shp', encoding = "UTF-8")
#mapaAux <- foreign::read.dbf('OD 2017/Banco de dados/OD_2017.dbf')
#mapaAux <- rbind(mapaAux[mapaAux$MODOPRIN %in% 9:12,], mapaAux[mapaAux$MODOPRIN %in% 1:6,])
#
#mapaAux %>%
#  select(ZONA,PONTO_BR, RENDA_FA, GRAU_INS, CD_ATIVI, DURACAO, ANDA_D, ANDA_O) -> mapaAux
#
#for(i in 1:length(mapa)){
#  
#  aux <- mapaAux[mapaAux$ZONA==i,]
#  
#  DURACAOOD <- mean((aux$DURACAO+aux$ANDA_D+aux$ANDA_O)*60)
#  ptBR <- mean(aux$PONTO_BR, na.rm = T)
#  rendaM <- mean(aux$RENDA_FA, na.rm = T)
#  
#  aux %>%
#    group_by(GRAU_INS) %>%
#    summarise(perc = n()) -> GInst
#  
#  GInst$perc <- GInst$perc/sum(GInst$perc)
#  
#  for(j in 2:5){
#    assign(paste0(names(GInst)[1],j),value = ifelse(length(GInst$perc[GInst$GRAU_INS == j]) == 0,0,GInst$perc[GInst$GRAU_INS == j]))
#    }
#
#  aux %>%
#    group_by(CD_ATIVI) %>%
#    summarise(perc = n()) -> CDAtiv
#  
#  CDAtiv$perc <- CDAtiv$perc/sum(CDAtiv$perc)
#  
#  for(j in 2:8){
#    assign(paste0(names(CDAtiv)[1],j),value = ifelse(length(CDAtiv$perc[CDAtiv$CD_ATIVI == j]) == 0,0,CDAtiv$perc[CDAtiv$CD_ATIVI == j]))
#  }  
#  
#  mapa[mapa$NumerZn==i,"RENDAMED"] <- rendaM
#  mapa[mapa$NumerZn==i,"PTBRMED"] <- ptBR
#  mapa[mapa$NumerZn==i,"DURACAOOD"] <- DURACAOOD
#  if(is.na(GRAU_INS2)!= T){mapa[mapa$NumerZn==i,"GRINS2"] <- GRAU_INS2}
#  if(is.na(GRAU_INS3)!= T){mapa[mapa$NumerZn==i,"GRINS3"] <- GRAU_INS3}
#  if(is.na(GRAU_INS4)!= T){mapa[mapa$NumerZn==i,"GRINS4"] <- GRAU_INS4}
#  if(is.na(GRAU_INS5)!= T){mapa[mapa$NumerZn==i,"GRINS5"] <- GRAU_INS5}
#  if(is.na(CD_ATIVI2)!= T){mapa[mapa$NumerZn==i,"CDATIV2"] <- CD_ATIVI2}
#  if(is.na(CD_ATIVI3)!= T){mapa[mapa$NumerZn==i,"CDATIV3"] <- CD_ATIVI3}
#  if(is.na(CD_ATIVI4)!= T){mapa[mapa$NumerZn==i,"CDATIV4"] <- CD_ATIVI4}
#  if(is.na(CD_ATIVI5)!= T){mapa[mapa$NumerZn==i,"CDATIV5"] <- CD_ATIVI5}
#  if(is.na(CD_ATIVI6)!= T){mapa[mapa$NumerZn==i,"CDATIV6"] <- CD_ATIVI6}
#  if(is.na(CD_ATIVI7)!= T){mapa[mapa$NumerZn==i,"CDATIV7"] <- CD_ATIVI7}
#  if(is.na(CD_ATIVI8)!= T){mapa[mapa$NumerZn==i,"CDATIV8"] <- CD_ATIVI8}
#  rm(list = c("GRAU_INS2",
#       "GRAU_INS3",
#       "GRAU_INS4",
#       "GRAU_INS5",
#       "CD_ATIVI2",
#       "CD_ATIVI3",
#       "CD_ATIVI4",
#       "CD_ATIVI5",
#       "CD_ATIVI6",
#       "CD_ATIVI7",
#       "CD_ATIVI8",
#       "rendaM", 
#       "ptBR"))
#  }
#
#writeOGR(mapa, dsn = 'ZonasODDados.shp',
#         layer = 'ZonasODDados',
#         encoding = "UTF-8",
#         driver = "ESRI Shapefile",
#         overwrite_layer = T,
#         delete_dsn = T)
#
#
##modeling variables ----
#
#mapa <- readOGR('ZonasODDados.shp', encoding = "UTF-8")
#
#mapa %>%
#  lm(formula = dfDstPb ~ 
#       RENDAMED +
##       PTBRMED + 
#       DURACAOOD +
##       GRINS2 +
#       GRINS3 +
#       GRINS4 +
##       GRINS5 +
#       CDATIV2 +
#       CDATIV3 +
#       CDATIV4 +
#       CDATIV5 +
#       CDATIV6 +
#       CDATIV7 +
#       CDATIV8) -> modelo
#
#summary(modelo)
#
#vif(modelo)
