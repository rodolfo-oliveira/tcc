## Arquivo: MODELOS GWR_GWmodel.R

#############################################
#### Estatística Espacial                ####
#### FGV Management - 1o Sem 2020        ####
#### Eduardo de Rezende Francisco        ####
#############################################

## Limpa o workspace
## (libera memória de eventuais objetos manipulados antes)
rm(list=ls())

#install.packages("GWmodel")
install.packages("spdep")

# load MAPTOOLS, SPGWR and SPDEP packages (extensions) in R environment

#library(maptools)

library(rgdal)
#library(spgwr)
library(GWmodel)
library(spdep)

# change working directory
setwd("C:/teste3")

# Source file: AREAP_SP.CSV (456 Weighted Areas in Sao Paulo City)
# "ap" is the variable that points to the input table
# (ap is the abbreviation of weighted areas in Portuguese)
ap <- read.csv("areacens_sp.csv")

# select some columns (5) in the input table and still point to ap
ap <- as.data.frame(cbind(ap$ID,ap$RENDA,ap$ENERGIA,ap$XCENTR,ap$YCENTR))

# rename these 5 columns
colnames(ap) <- c("ID","Income","Energy","X","Y")

# Map of São Paulo's weighted areas
#poligonos <- readShapePoly("areacens_sp.shp")
pontos <- rgdal::readOGR(dsn="c:/teste3",layer="pontos_origem_gc")
length(pontos)
amostra <- sample(1:length(pontos),size=5000,replace=FALSE)
pontos2 <- pontos[amostra,]
#pontos <- rgdal::readOGR(dsn="c:/teste3",layer="p1")
#pontos <- rgdal::readOGR(dsn="c:/teste3",layer="Porigem")

colnames(pontos@data)
plot(pontos)
# calculate global residual SST (SQT)
SST <- sum((ap$Income - mean(ap$Income))^2)


############################################################################
## REGRESSÃO LINEAR SIMPLES ################################################
############################################################################

# "lm" is the function used to fit linear models
# "Income ~ Energy" is the way you explicit regression formulas in R
# (Income is the dependent and Energy is one (the only one) independent var)
lm.ap <- lm(Income ~ Energy,data=ap)
lm.ap
summary(lm.ap)

# store the residuals (the response minus fitted values of the model)
OLS_SSE <- sum(lm.ap$residuals^2)

# calculate R2 of the global model and store in "results.ap" variable
r2_OLS <- 1 - (OLS_SSE/SST)
r2_OLS

# define coords (X and Y coordinates)
coords <- cbind(ap$X,ap$Y)
colnames(coords) <- c("X","Y")

############################################################################
## GWR - Geographically Weighted Regression ################################
############################################################################

a <- gw.dist(as.matrix(pontos@coords),as.matrix(pontos@coords),focus=0,longlat=FALSE)

# Calcula largura de banda (em # de registros) para diversos tipos de kernel
bw.ap  <- bw.gwr(DIFFRNC ~ RENDA_FA + GRAU_INS + CD_ATIVI + MOTIVO + DURACAOOD,
                 data=pontos2, approach="AICc", 
                 kernel="gaussian", adaptive=TRUE)
bw.ap


# Aplica GWR para os diversos tipos de kernel

gwr.ap <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bw.ap, kernel="bisquare",
                      adaptive=TRUE)
gwr.ap

############################################################################
## GWR - Geographically Weighted Regression ################################
## COMPARANDO LARGURAS DE BANDA ############################################
############################################################################

# define coords (X and Y coordinates)
coords <- cbind(ap$X,ap$Y)
colnames(coords) <- c("X","Y")

# Calcula largura de banda (em % de registros) para diversos tipos de kernel
bwGauss  <- bw.gwr(RENDA~ENERGIA, data=poligonos, approach="AICc",
                   kernel="gaussian", adaptive=TRUE)
bwExpon  <- bw.gwr(RENDA~ENERGIA, data=poligonos, approach="AICc",
                   kernel="exponential", adaptive=TRUE)
bwBisqr  <- bw.gwr(RENDA~ENERGIA, data=poligonos, approach="AICc",
                   kernel="bisquare", adaptive=TRUE)
bwTriCub <- bw.gwr(RENDA~ENERGIA, data=poligonos, approach="AICc",
                   kernel="tricube", adaptive=TRUE)
bwBoxCar <- bw.gwr(RENDA~ENERGIA, data=poligonos, approach="AICc",
                   kernel="boxcar", adaptive=TRUE)

bwGauss
bwExpon
bwBisqr
bwTriCub
bwBoxCar

# Aplica GWR para os diversos tipos de kernel

gwrGauss <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bwGauss, kernel="gaussian",
                      adaptive=TRUE)
gwrExpon <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bwExpon, kernel="exponential",
                      adaptive=TRUE)
gwrBisqr <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bwBisqr, kernel="bisquare",
                      adaptive=TRUE)
gwrTriCub <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bwTriCub, kernel="tricube",
                       adaptive=TRUE)
gwrBoxCar <- gwr.basic(RENDA~ENERGIA, data=poligonos, bw=bwBoxCar, kernel="boxcar",
                       adaptive=TRUE)

r2_GWRGauss <- gwrGauss$GW.diagnostic$gw.R2
r2_GWRExpon <- gwrExpon$GW.diagnostic$gw.R2
r2_GWRBisqr <- gwrBisqr$GW.diagnostic$gw.R2
r2_GWRTriCub <- gwrTriCub$GW.diagnostic$gw.R2
r2_GWRBoxCar <- gwrBoxCar$GW.diagnostic$gw.R2

############################################################################
# SAR - Spatial Autoregressive lag model ###################################
# (k nearest neighbours [k from AIC Bisquare minimisation]) ################
############################################################################

k.ap <- bw.ap
 
# create spatial weights using k nearest neighbours (knearneigh command)
# and convert to a W matrix style (knn2nb and nb2listw commands)
myknn <- knearneigh(coords,k=k.ap,longlat=FALSE,RANN=FALSE)
mynb <- knn2nb(myknn,sym=TRUE)
mylistw <- nb2listw(mynb,style="W")

# "lagsarlm" is the function that implements SAR Lag model in R
sar.ap <- lagsarlm(Income ~ Energy,data=ap,mylistw,method="Matrix")
  
# store RSS and R2 of the SAR lag model
SARk_SSE <- sar.ap$SSE
r2_SARk <- 1 - (SARk_SSE/SST)
r2_SARk

#Comparando o R2 dos diversos modelos
r2_GWRGauss
r2_GWRBisqr
r2_GWRExpon
r2_GWRTriCub
r2_GWRBoxCar
r2_SARk

