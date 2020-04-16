setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/")

#pacotes ----------------------------------------------

library(tidyverse)
library(rgdal)
library(sp)
library(car)
library (raster)
library (rgeos)
library(GWmodel)
library(spatstat)
library(boot)
library(stringr)
library(RColorBrewer)

dados <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp",layer = "mapa_Origem_tempo_relativo_distritos")

kernel_type <-"gaussian"
bw_def <-20


gw.ss.bs <- gwss(dados, vars = c("Tempo_dif","Temp_rel","DENESTAB","DENEMPREG","DENDOM","DENPTBUS","DKMLINBUS","DENLINBUS",
                                               "RENDP2010","DENPOP2018","PNBRAN2010","PDOMM2010","PDOMC2010","DUMMETRO","DUMCPTM"), 
                  kernel = kernel_type, adaptive = TRUE, bw = 20, quantile = TRUE)
  

setwd('..')
setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019")

map.na = list("SpatialPolygonsRescale", layout.north.arrow(),
              offset = c(329000, 261500), scale = 4000, col = 1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(),
                   offset = c(326500, 217000), scale = 5000, col = 1,
                   fill = c("transparent", "blue"))
map.scale.2 = list("sp.text", c(326500, 217900), "0", cex = 0.9, col = 1)
map.scale.3 = list("sp.text", c(331500, 217900), "5km", cex = 0.9, col = 1)
map.layout <- list(map.na, map.scale.1, map.scale.2, map.scale.3)
mypalette.1 <- brewer.pal(8, "Reds")


i=1
while(i<=length(names(gw.ss.bs$SDF))){

  mapa = spplot(gw.ss.bs$SDF, names(gw.ss.bs$SDF[i]), key.space = "right",
                col.regions = mypalette.1, cuts = 7, sp.layout = map.layout,
                main = names(gw.ss.bs$SDF[i]))
  
  jpeg(paste("GWR_Model/", names(gw.ss.bs$SDF[i]),"_", kernel_type, ".jpg", sep = ""), width = 700, height = 500)
  plot(mapa)

  dev.off()
  i <- i+1
}

mod_lin_global <- lm(Tempo_dif~DENESTAB+DENEMPREG+DENDOM+DENPTBUS+DKMLINBUS+DENLINBUS+
                       RENDP2010+DENPOP2018+PNBRAN2010+PDOMM2010+PDOMC2010+DUMMETRO+DUMCPTM,
                     data=dados)

summary(mod_lin_global)
#selecao de variaveis
VarDep <- "Tempo_dif"
VarsInd <- c("DENESTAB","DENEMPREG","DENDOM","DENPTBUS","DKMLINBUS","DENLINBUS",
             "RENDP2010","DENPOP2018","PNBRAN2010","PDOMM2010","PDOMC2010","DUMMETRO","DUMCPTM")
#selecao de variaveis para os modelos
modelo_selec <- model.selection.gwr(VarDep, VarsInd, data = dados,
                                    kernel = kernel_type, adaptive = TRUE, bw=bw_def)
#ordenamento de modleos por AIC
ordem.models <-model.sort.gwr(modelo_selec, numVars = length(VarsInd), 
                              ruler.vector = modelo_selec[[2]][,2])
lista_modelos <- ordem.models[[1]]

model.view.gwr(VarDep,VarsInd, model.list = lista_modelos)

plot(ordem.models[[2]][,2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection procedure",
     ylab = "AICc", xlab = "Model number", type = "b")

novo_modelo <- ordem.models[[1]][which(ordem.models[[2]][,2]==min(ordem.models[[2]][,2]))][[1]][1]

#calibrando o bw

bw.gwr1 <- bw.gwr(as.formula(novo_modelo[[1]]),
                  data = dados, approach = "AICc", kernel = kernel_type, adaptive = TRUE)

bw_def<-bw.gwr1
formula_em_teste <- as.formula(novo_modelo[[1]])

#ajuste do modelo GWR para diferenca de tempo

gwr.res <- gwr.basic(Tempo_dif ~ PNBRAN2010 + DENESTAB + DENPOP2018 + DUMCPTM + DENDOM + 
                       PDOMM2010 + DUMMETRO + DENPTBUS,
                  data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                  bw = bw_def)

diagnostico = gwr.collin.diagno(Tempo_dif ~ PNBRAN2010 + DENESTAB + DENPOP2018 + DUMCPTM + DENDOM + 
                                  PDOMM2010 + DUMMETRO + DENPTBUS,
                  data = dados, kernel = kernel_type, adaptive = TRUE,
                  bw = bw_def)

summary(diagnostico$VIF)


#retirando DENDOM - colinearidade

gwr.res <- gwr.basic(Tempo_dif ~ PNBRAN2010 + DENESTAB + DENPOP2018 + DUMCPTM + 
                       PDOMM2010 + DUMMETRO + DENPTBUS,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Tempo_dif ~ PNBRAN2010 + DENESTAB + DENPOP2018 + DUMCPTM +
                                  PDOMM2010 + DUMMETRO + DENPTBUS,
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)


#testando novo modelo


VarDep <- "Tempo_dif"
VarsInd <- c("PNBRAN2010", "DENESTAB", "DENPOP2018", "DUMCPTM",
               "PDOMM2010", "DUMMETRO", "DENPTBUS")
#selecao de variaveis para os modelos
modelo_selec <- model.selection.gwr(VarDep, VarsInd, data = dados,
                                    kernel = kernel_type, adaptive = TRUE, bw=bw_def)
#ordenamento de modleos por AIC
ordem.models <-model.sort.gwr(modelo_selec, numVars = length(VarsInd), 
                              ruler.vector = modelo_selec[[2]][,2])
lista_modelos <- ordem.models[[1]]

model.view.gwr(VarDep,VarsInd, model.list = lista_modelos)

plot(ordem.models[[2]][,2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection procedure",
     ylab = "AICc", xlab = "Model number", type = "b")

novo_modelo <- ordem.models[[1]][which(ordem.models[[2]][,2]==min(ordem.models[[2]][,2]))][[1]][1]

formula_em_teste <- as.formula(novo_modelo[[1]])

#testes de significancia
gwr.res <- gwr.basic(Tempo_dif ~ PNBRAN2010 + DENPTBUS + DUMMETRO + DUMCPTM + DENESTAB + 
                       DENPOP2018,
                       data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                       bw = bw_def)

diagnostico = gwr.collin.diagno(Tempo_dif ~ PNBRAN2010 + DENPTBUS + DUMMETRO + DUMCPTM + DENESTAB + 
                                  DENPOP2018,
                                data = dados, kernel = kernel_type, adaptive = TRUE, 
                                bw = bw_def)

#testes de significancia - sem DENESTAB
gwr.res <- gwr.basic(Tempo_dif ~ PNBRAN2010 + DENPTBUS + DUMMETRO + DUMCPTM + 
                       DENPOP2018,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Tempo_dif ~ PNBRAN2010 + DENPTBUS + DUMMETRO + DUMCPTM + 
                                  DENPOP2018,
                                data = dados, kernel = kernel_type, adaptive = TRUE, 
                                bw = bw_def)




#mapas 
for (i in 1:length(names(gwr.res$SDF[,]))){
  print(i)
  nome = names(gwr.res$SDF[,i])
  jpeg(paste("GWR_regressions/", "TEMPO_DIF_",names(gwr.res$SDF[1,i]),"_", kernel_type, ".jpg", sep = ""), width = 700, height = 500)
  mapa = spplot(gwr.res$SDF, nome, key.space = "right",
       col.regions = rev(heat.colors(50)), at = seq(min(gwr.res$SDF[,i]@data)-0.001,max(gwr.res$SDF[,i]@data)+0.001,
                                           (max(gwr.res$SDF[,i]@data)-min(gwr.res$SDF[,i]@data)+0.002)/50),
       main = paste("Regressão GWR - Diferenças de tempo - Estimativas para", names(gwr.res$SDF[1,i]), sep = " "),
       sp.layout=map.layout)
  plot(mapa)
  dev.off()
}



#tudo de novo para tempos relativos -----------------

mod_lin_global <- lm(Temp_rel~DENESTAB+DENEMPREG+DENDOM+DENPTBUS+DKMLINBUS+DENLINBUS+
                       RENDP2010+DENPOP2018+PNBRAN2010+PDOMM2010+PDOMC2010+DUMMETRO+DUMCPTM,
                     data=dados)

summary(mod_lin_global)
#selecao de variaveis
VarDep <- "Temp_rel"
VarsInd <- c("DENESTAB","DENEMPREG","DENDOM","DENPTBUS","DKMLINBUS","DENLINBUS",
             "RENDP2010","DENPOP2018","PNBRAN2010","PDOMM2010","PDOMC2010","DUMMETRO","DUMCPTM")
#selecao de variaveis para os modelos
modelo_selec <- model.selection.gwr(VarDep, VarsInd, data = dados,
                                    kernel = kernel_type, adaptive = TRUE, bw=bw_def)
#ordenamento de modleos por AIC
ordem.models <-model.sort.gwr(modelo_selec, numVars = length(VarsInd), 
                              ruler.vector = modelo_selec[[2]][,2])
lista_modelos <- ordem.models[[1]]

model.view.gwr(VarDep,VarsInd, model.list = lista_modelos)

plot(ordem.models[[2]][,2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection procedure",
     ylab = "AICc", xlab = "Model number", type = "b")

novo_modelo <- ordem.models[[1]][which(ordem.models[[2]][,2]==min(ordem.models[[2]][,2]))][[1]][1]

#calibrando o bw

bw.gwr1 <- bw.gwr(as.formula(novo_modelo[[1]]),
                  data = dados, approach = "AICc", kernel = kernel_type, adaptive = TRUE)

bw_def<-bw.gwr1
formula_em_teste <- as.formula(novo_modelo[[1]])

#ajuste do modelo GWR para diferenca de tempo

gwr.res <- gwr.basic(formula_em_teste <- as.formula(novo_modelo[[1]]),
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(formula_em_teste <- as.formula(novo_modelo[[1]]),
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)


#retirando DENDOM - colinearidade

gwr.res <- gwr.basic(Temp_rel ~ DUMMETRO + PDOMM2010 + DENESTAB + DUMCPTM + DENPTBUS + 
                       RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018 + 
                       DENLINBUS,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Temp_rel ~ DUMMETRO + PDOMM2010 + DENESTAB + DUMCPTM + DENPTBUS + 
                                  RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018 + 
                                  DENLINBUS,
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)

#retirando DENDOM e DENESTAB - colinearidade

gwr.res <- gwr.basic(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                       RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018 + 
                       DENLINBUS,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                                  RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018 + 
                                  DENLINBUS,
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)

#retirando DENDOM, DENESTAB e DENLINBUS- colinearidade

gwr.res <- gwr.basic(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                       RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                                  RENDP2010 + DENEMPREG + DKMLINBUS + PNBRAN2010 + DENPOP2018,
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)

#retirando DENDOM, DENESTAB, DENLINBUS e PNBRAN2010- colinearidade

gwr.res <- gwr.basic(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                       RENDP2010 + DENEMPREG + DKMLINBUS + DENPOP2018,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Temp_rel~DUMMETRO + PDOMM2010 + DUMCPTM + DENPTBUS + 
                                  RENDP2010 + DENEMPREG + DKMLINBUS + DENPOP2018,
                                data = dados, kernel = kernel_type, adaptive = TRUE,
                                bw = bw_def)

summary(diagnostico$VIF)




#testando novo modelo


VarDep <- "Temp_rel"
VarsInd <- c("DENEMPREG","DKMLINBUS", "DENPTBUS",
             "RENDP2010","DENPOP2018","PDOMM2010","DUMMETRO","DUMCPTM")
#selecao de variaveis para os modelos
modelo_selec <- model.selection.gwr(VarDep, VarsInd, data = dados,
                                    kernel = kernel_type, adaptive = TRUE, bw=bw_def)
#ordenamento de modleos por AIC
ordem.models <-model.sort.gwr(modelo_selec, numVars = length(VarsInd), 
                              ruler.vector = modelo_selec[[2]][,2])
lista_modelos <- ordem.models[[1]]

model.view.gwr(VarDep,VarsInd, model.list = lista_modelos)

plot(ordem.models[[2]][,2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection procedure",
     ylab = "AICc", xlab = "Model number", type = "b")

novo_modelo <- ordem.models[[1]][which(ordem.models[[2]][,2]==min(ordem.models[[2]][,2]))][[1]][1]

#saiu o DKLIMBUS e DENPOP2018

gwr.res <- gwr.basic(Temp_rel~DUMMETRO+PDOMM2010+DUMCPTM+DENPTBUS+RENDP2010+DENEMPREG,
                     data = dados, kernel = kernel_type, adaptive = TRUE, F123.test = TRUE,
                     bw = bw_def)

diagnostico = gwr.collin.diagno(Temp_rel~DUMMETRO+PDOMM2010+DUMCPTM+DENPTBUS+RENDP2010+DENEMPREG,
                                data = dados, kernel = kernel_type, adaptive = TRUE, 
                                bw = bw_def)

#retirada de variaveis pela significancia

#mapas
for (i in 1:length(names(gwr.res$SDF[,]))){
  print(i)
  nome = names(gwr.res$SDF[,i])
  jpeg(paste("GWR_regressions/", "TEMPO_REL_",names(gwr.res$SDF[1,i]),"_", kernel_type, ".jpg", sep = ""), width = 700, height = 500)
  mapa = spplot(gwr.res$SDF, nome, key.space = "right",
                col.regions = rev(heat.colors(50)), at = seq(min(gwr.res$SDF[,i]@data)-0.001,max(gwr.res$SDF[,i]@data)+0.001,
                                                             (max(gwr.res$SDF[,i]@data)-min(gwr.res$SDF[,i]@data)+0.002)/50),
                main = paste("Regressão GWR - Tempos Relativos - Estimativas para", names(gwr.res$SDF[1,i]), sep = " "),
                sp.layout=map.layout)
  plot(mapa)
  dev.off()
}
